/**
 * Created on September 24, 2009
 * Copyright (c) 2009-2010, Wei-ju Wu
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of Wei-ju Wu nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY WEI-JU WU ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL WEI-JU WU BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.mahatma68k;

/**
 * This is the Cpu class. The design goal was to keep the interface as simple
 * as possible. Users of the Cpu class should implement the AddressSpace
 * in order to provide the memory environment for the Cpu.
 * <p><b>Example:</b></p>
 * <pre>
 *   ...
 *   Cpu cpu = new Cpu();
 *   cpu.setAddressSpace(myAddressSpace);
 *   Cpu.Instruction instr = cpu.nextInstruction();
 *   int cycles = instr.numCycles();
 *   instr.execute();
 *   ...
 * </pre>
 * @author Wei-ju Wu
 * @version 1.0
 * @see AddressSpace
 */
public class Cpu {

  /**
   * Instruction interface. Instructions are returned by the Cpu.nextInstruction()
   * method. If the Cpu code was generated with disassembly functionality (this
   * is the default), calling <code>toString()</code> on an Instruction object
   * returns the disassemble assembly source this object represents.
   * Note that the <code>execute()</code> and <code>numCycles()</code>
   * instructions are called separate from each other. This is a design decision
   * which allows to predict how many cycles the instruction will use
   * <b>before</b> it is executed. This might be helpful in some tricky bus
   * synchronization situations.
   */
  public interface Instruction {
    /**
     * Executes the instruction.
     */
    void execute();
    /**
     * Returns the number of cycles this instruction needs to execute.
     * @return the number of cycles
     */
    int numCycles();
  }

  private class InterruptRequest {
    int level;
    InterruptAcknowledge ack;
    InterruptRequest(int level, InterruptAcknowledge ack) {
      this.level = level;
      this.ack = ack;
    }
    public InterruptAcknowledge.VectorType acknowledge() {
      return ack.acknowledge(level);
    }
  }

  private Instruction[] instructions = new Instruction[65536];
	private int usedInstructions;
  private AddressSpace mem;
  private int[] areg = new int[7]; // a0-a6
  private int[] dreg = new int[8]; // d0-d7
  private int pc;       // program counter, moves with decoding
  private int decodePc; // last decode position stays at last instr start

  // Status register: (System byte | User byte)
  // System byte: T0 T1  S  M  0 I2 I1 I0
  // User byte:    0  0  0  X  N  Z  V  C
  // T1, T0 = Trace enable (00: no trace, 10: trace on any instruction,
  //                        01: trace on change of flow, 11: undef)
  // S, M   = active stack (0x: USP, 10: ISP, 11: MSP)
  static final int SR_C  =     1; // Carry
  static final int SR_V  =     2; // Overflow
  static final int SR_Z  =     4; // Zero
  static final int SR_N  =     8; // Negative
  static final int SR_X  =    16; // Extended
  static final int SR_I0 =   256; // Interrupt 0
  static final int SR_I1 =   512; // Interrupt 1
  static final int SR_I2 =  1024; // Interrupt 2
  static final int SR_M  =  4096; // Master/Interrupt state
  static final int SR_S  =  8192; // Supervisor/User state
  static final int SR_T0 = 16384; // Trace enable 1
  static final int SR_T1 = 32768; // Trace enable 0

  static final int MSB_L = 0x80000000;
  static final int MSB_W = 0x8000;
  static final int MSB_B = 0x80;

  // Exception Vectors
  static final int ILLEGAL_INSTRUCTION = 4;
  static final int ZERO_DIVIDE         = 5;
  static final int PRIVILEGE_VIOLATION = 8;
  static final int SPURIOUS_INTERRUPT  = 24;

  // Supervisor mode, Interrupt mask = 7
  private int sr = 0x2700;
  private int ssp; // supervisor stack pointer
  private int usp; // user stack pointer
  // current "simple effective address operation word" - specifies length,
  // effective addressing mode and operation
  private int currentInstrWord;
  private ResetHandler resetHandler;

  // Interrupt related attributes
  private InterruptRequest pendingInterruptRequest;


  // illegal instruction object, switches to Supervisor mode
  private Instruction illegal = new Instruction() {
    public int numCycles() { return 34; }
    public void execute() { doException(ILLEGAL_INSTRUCTION); }
    public String toString() { return "<ILLEGAL INSTRUCTION>"; }
  };

  // **********************************************************************
  // ***** PUBLIC INTERFACE
  // **********************************************************************
  /**
   * Constructor.
   */
  public Cpu() {
		long startTime = System.currentTimeMillis();
    initInstructionTable();
    // we do not have a complete instruction set yet and therefore
    // filling the empty slots with illegal makes debugging more difficult
    // currently, illegal is only set to the slots that Kickstart uses
    instructions[0x4e7b] = illegal;
    instructions[0x4afc] = illegal;
		long endTime = System.currentTimeMillis();
		System.out.printf("initialized, # instructions: %d, %d ms.\n",
			usedInstructions, (int) (endTime - startTime));
  }

    public boolean intercept_jsr(int newpc) {
        System.out.printf("Intercepting JSR instruction to $%04x\n", newpc);
        return false;
    }

  /**
   * Decodes and returns the instruction at the current program counter.
   * @return the decoded instruction object
   */
  public Instruction nextInstruction() {
    handleInterruptRequest();
    decodePc = pc;
    // convert to unsigned so we can use it as an array index
    currentInstrWord = mem.readShort(pc) & 0xffff;
    pc += 2;
    Instruction instruction = instructions[currentInstrWord];
    if (instruction == null) {
      System.out.printf("illegal opcode: %04x\n", currentInstrWord);
    }
    return instruction;
  }

  /**
   * Handles a pending interrupt request if one exists.
   */
  private void handleInterruptRequest() {
    if (pendingInterruptRequest != null) {
      int currentInterruptLevel = sr & 0x700;
      int intlevel = pendingInterruptRequest.level;
      if (intlevel > currentInterruptLevel) {
        InterruptAcknowledge.VectorType vectorType =
          pendingInterruptRequest.acknowledge();
        int vector = SPURIOUS_INTERRUPT;
        if (vectorType == InterruptAcknowledge.VectorType.AUTOVECTOR) {
          vector += intlevel;
        }
        doException(vector);
        setCurrentInterruptLevel(intlevel);
        pendingInterruptRequest = null;
      }
    }
  }

  /**
   * Returns a string representation of the current CPU state.
   * @return string representation of the CPU state
   */
  public String getState() {
    StringBuilder builder = new StringBuilder();
    builder.append(String.format("PC = $%04x SR = %04x\n", pc, sr));
    for (int i = 0; i < 8; i++) {
      builder.append(String.format("d%d: #$%02x ", i, dreg[i]));
    }
    builder.append("\n");
    for (int i = 0; i < 8; i++) {
      builder.append(String.format("a%d: #$%02x ", i,
                                   getAddressRegisterValueL(i)));
    }
    builder.append("\n");
    return builder.toString();
  }

  /**
   * Sets the address space the CPU will operate on.
   * @param addressSpace the AddressSpace instance representing the system
   */
  public void setAddressSpace(AddressSpace addressSpace) { mem = addressSpace; }

  // Methods to access the internal state of the CPU from external
  // clients, e.g. for testing

  /**
   * Determines whether the CPU is currently in Supervisor Mode.
   * @return true = Supervisor Mode, false = User Mode
   */
  public boolean isSupervisorMode() { return (0x2000 & sr) == 0x2000; }

  /**
   * Sets the value of the program counter.
   * @param pc new value of the program counter
   */
  public void setPC(int pc) { this.pc = pc; }

  /**
   * Returns the value of the program counter.
   * @return value of the program counter
   */
  public int getPC() { return pc; }

  /**
   * Returns the value of the entire status register (system and user byte).
   * @return status register value
   */
  public int getSR() { return sr; }

  /**
   * Returns the value of the Supervisor Stack Pointer.
   * @return Supervisor Stack Pointer value
   */
  public int getSSP() { return ssp; }

  /**
   * Sets the value of the Supervisor Stack Pointer. Some systems might want
   * to initialize this at system start.
   * @param value Supervisor Stack Pointer value
   */
  public void setSSP(int value) { ssp = value; }

  /**
   * Returns the value of the user stack pointer.
   * @return User Stack Pointer value
   */
  public int getUSP() { return usp; }

  /**
   * Returns the value of the current instruction word.
   * @return current instruction word value
   */
  public int currentInstrWord() { return currentInstrWord; }

  /**
   * Sets the value of the specified address register.
   * A7 maps to the current stack pointer.
   * @param regnum register number (0-7)
   * @param value new value
   */
  public void setAddressRegisterValue(int regnum, int value) {
    setAddressRegisterDirectValueL(regnum, value);
  }

  /**
   * Returns the value contained in the specified address register.
   * A7 maps to the current stack pointer.
   * @param regnum register number (0-7)
   * @return value contained in the register
   */
  public int getAddressRegisterValue(int regnum) {
    return getAddressRegisterValueL(regnum);
  }

  /**
   * Sets the value of the specified data register.
   * @param regnum register number (0-7)
   * @param value new value
   */
  public void setDataRegisterValue(int regnum, int value) { dreg[regnum] = value; }

  /**
   * Returns the value contained in the specified data register.
   * @param regnum register number
   * @return value contained in the register
   */
  public int getDataRegisterValue(int regnum) { return dreg[regnum]; }

  /**
   * Sets the ResetHandler. The handler will be called when the RSTO
   * signal is set (this is done by the reset instruction).
   * @param handler ResetHandler
   */
  public void setResetHandler(ResetHandler handler) { resetHandler = handler; }

  /**
   * Resets the CPU. In the specification, this is called by the reset
   * exception.
   */
  public void resetCpu() {
    sr  = 0x2700;
    ssp = mem.readLong(0);
    pc  = mem.readLong(4);
    pendingInterruptRequest = null;
  }

  /**
   * Makes an interrupt request to the CPU. Set level to 0 to clear
   * the interrupt request. If level is 0, ack can (and should) be null.
   * @param level the interrupt level
   * @param ack the InterruptAcknowledger
   */
  public void makeInterruptRequest(int level, InterruptAcknowledge ack) {
    pendingInterruptRequest = level == 0 ? null :
      new InterruptRequest(level, ack);
  }

  /**
   * Determines whether this CPU has a pending interrupt request.
   * @return true if interrupt request pending, false otherwise
   */
  public boolean hasPendingInterruptRequest() {
    return pendingInterruptRequest != null;
  }

  // **********************************************************************
  // ***** PRIVATE METHODS
  // **********************************************************************
  // **********************************************************************
  // ***** EXCEPTIONS
  // **********************************************************************
  private void doException(int exceptionId) {
    int oldsr = sr; // save the original status register
    // set supervisor mode first, so the values are saved on the
    // supervisor stack
    setFlags(SR_S);
    pushLong(pc);
    pushShort(oldsr);
    // jump to illegal vector (vector number * 4)
    pc = mem.readLong(exceptionId << 2);
  }

  private void privilegeViolation() {
    pc -= 2; // rewind the PC for privilege violations
    doException(PRIVILEGE_VIOLATION);
  }

  private void divisionByZero() { doException(ZERO_DIVIDE); }

  // **********************************************************************
  // ***** ADDRESS REGISTER ACCESS
  // **********************************************************************
  // Stack operations
  private void pushLong(int value) {
    incrementStackPointer(-4);
    mem.writeLong(getStackPointerValueL(), value);
  }
  private void pushShort(int value) {
    incrementStackPointer(-2);
    mem.writeShort(getStackPointerValueL(), value);
  }
  private int popLong() {
    int result = mem.readLong(getStackPointerValueL());
    incrementStackPointer(4);
    return result;
  }
  private int popShort() {
    int result = mem.readShort(getStackPointerValueL());
    incrementStackPointer(2);
    return result;
  }

  private void incrementStackPointer(int value) {
    if (isSupervisorMode()) ssp += value;
    else usp += value;
  }
  private void setStackPointerValue(int value) {
    if (isSupervisorMode()) ssp = value;
    else usp = value;
  }
  private void setUserStackPointerValue(int value) { usp = value; }
  private int getUserStackPointerValue() { return usp; }

  private void incrementAddressRegister(int regnum, int value) {
    if (regnum == 7) incrementStackPointer(value);
    else areg[regnum] += value;
  }

  // Private getters
  private int getStackPointerValueB() {
    return getSignExtended8((isSupervisorMode() ? ssp : usp) & 0xff);
  }
  private int getStackPointerValueW() {
    return getSignExtended16((isSupervisorMode() ? ssp : usp) & 0xffff);
  }
  private int getStackPointerValueL() {
    return isSupervisorMode() ? ssp : usp;
  }
  private int getAddressRegisterValueB(int regnum) {
    return regnum == 7 ? getStackPointerValueB() :
      getSignExtended8(areg[regnum] & 0xff);
  }
  private int getAddressRegisterValueW(int regnum) {
    return regnum == 7 ? getStackPointerValueW() :
      getSignExtended16(areg[regnum] & 0xffff);
  }
  private int getAddressRegisterValueL(int regnum) {
    return regnum == 7 ? getStackPointerValueL() : areg[regnum];
  }
  private void setAddressRegisterValueB(int regnum, int value) {
    // The entire register is affected, and the input value is sign extended
    if (regnum == 7) {
      setStackPointerValue(getSignExtended8(value));
    } else {
      areg[regnum] = getSignExtended8(value);
    }
  }
  private void setAddressRegisterValueW(int regnum, int value) {
    // The entire register is affected, and the input value is sign extended
    if (regnum == 7) {
      setStackPointerValue(getSignExtended16(value));
    } else {
      areg[regnum] = getSignExtended16(value);
    }
  }
  private void setAddressRegisterValueL(int regnum, int value) {
    if (regnum == 7) setStackPointerValue(value);
    else areg[regnum] = value;
  }

  // **********************************************************************
  // ***** DATA REGISTER ACCESS
  // **********************************************************************
  private int getDataRegisterValueB(int regnum) {
    return getSignExtended8(dreg[regnum] & 0xff);
  }
  private int getDataRegisterValueW(int regnum) {
    return getSignExtended16(dreg[regnum] & 0xffff);
  }
  private int getDataRegisterValueL(int regnum) {
    return dreg[regnum];
  }

  // **********************************************************************
  // ***** CPU FLAGS
  // **********************************************************************
  private int getCCR() { return sr & 0xff; }
  private void setCCR(int value) { sr = (sr & 0xff00) | (value & 0xff); }

  /**
   * Sets the flags in the status register.
   */
  private void setFlags(int... flags) {
    for (int flag : flags) sr |= flag;
  }
  private void setFlag(int flag) { sr |= flag; }

  /**
   * Clears the flags in the status register.
   */
  private void clearFlags(int... flags) {
    int clearmask = 0;
    for (int flag : flags) clearmask |= flag;
    clearmask = ~clearmask;
    sr &= clearmask;
  }
  private void clearFlag(int flag) { sr &= ~flag; }

  private void setZeroFlag()     { setFlag(SR_Z); }
  private void setNegativeFlag() { setFlag(SR_N); }
  private void setCarryFlag()    { setFlag(SR_C); }
  private void setOverflowFlag() { setFlag(SR_V); }
  private void setExtendedFlag() { setFlag(SR_X); }
  private boolean isCarrySet() { return (sr & SR_C) == SR_C; }
  private boolean isOverflowSet() { return (sr & SR_V) == SR_V; }
  private boolean isExtendedSet() { return (sr & SR_X) == SR_X; }
  private void setCurrentInterruptLevel(int level) {
    sr = (sr & 0xf8ff) | (level << 8);
  }

  // **********************************************************************
  // ***** ACCESS TO INSTRUCTION DATA
  // **********************************************************************
  // Extract register number from current instruction
  private int getRegnum() { return (currentInstrWord >>> 9) & 0x07; }
  private int getDataValue() { return getRegnum(); }
  private int getEffAddrExtRegnum() { return getRegnum(); }
  private int getEffAddrRegnum() { return currentInstrWord & 0x07; }
  private int getDisplacement() {
    int displacement = getSignExtended8(currentInstrWord);
    if (displacement == 0) {
      // displacement is 16-bit
      displacement = mem.readShort(pc);
      pc += 2;
    }
    return displacement;
  }
  private int getTrapVector() { return (currentInstrWord & 0x0f) + 32; }

  // **********************************************************************
  // ***** CONVERSION HELPERS
  // **********************************************************************
  private static int getSignExtended8(int value) {
    return ((value & 0x80) == 0x80) ?
      (0xffffff00 | (value & 0xff)) : (value & 0xff);
  }
  private static int getSignExtended16(int value) {
    return ((value & 0x8000) == 0x8000) ?
      (0xffff0000 | (value & 0xffff)) : (value & 0xffff);
  }
  // **********************************************************************
  // ***** FORMATTING HELPERS
  // *********************************************
  /**
   * Assumes that value is signed and formats it so that negative values
   * have a minus prefix.
   * @param value the integer value, assumed to be signed
   */
  private static String formatSigned(int value) {
    return (value < 0) ? String.format("-$%02x", -value) :
      String.format("$%02x", value);
  }
  // **********************************************************************
  // ***** REGISTER LIST HELPERS FOR MOVEM
  // *********************************************
  private static int numBitsSet(final int value) {
    int count = 0;
    int n = value;
    while (n != 0) {
      count++;
      n &= (n - 1);
    }
    return count;
  }

  // Extracts the register numbers from an 8 bit mask for movem register list
  // type 1 (memory -> register)
  private static int[] regnumsInMask(final int value) {
    final int[] regnums = new int[8];
    int n = value;
    int resultsize = 0;
    for (int regnum = 0; regnum < 8; regnum++) {
      if ((n & 1) == 1) regnums[resultsize++] = regnum;
      n >>= 1;
    }
    int[] result = new int[resultsize];
    for (int i = 0; i < resultsize; i++) result[i] = regnums[i];
    return result;
  }
  // As above, but reversed, as in list type 2 (register -> memory)
  private static int[] regnumsInMaskRev(final int value) {
    final int[] regnums = new int[8];
    int n = value;
    int resultsize = 0;
    for (int regnum = 7; regnum >= 0; regnum--) {
      if ((n & 1) == 1) regnums[resultsize++] = regnum;
      n >>= 1;
    }
    // reverse the list to have the natural order
    int[] result = new int[resultsize];
    for (int i = 0; i < resultsize; i++) result[i] = regnums[i];
    return result;
  }
  
  private static String reglistMaskToString(int reglistMask) {
    int[] dregs = regnumsInMask(reglistMask & 0xff);
    int[] aregs = regnumsInMask((reglistMask >>> 8) & 0xff);
    return formatRegisterList(dregs, aregs, false);
  }
  private static String reglistMaskRevToString(int reglistMask) {
    int[] aregs = regnumsInMaskRev(reglistMask & 0xff);
    int[] dregs = regnumsInMaskRev((reglistMask >>> 8) & 0xff);
    return formatRegisterList(dregs, aregs, true);
  }

  private static String formatRegisterList(int[] dregs, int[] aregs,
                                           boolean reverse) {
    StringBuilder result = new StringBuilder();
    int elems = 0;
    result.append("[");
    if (reverse) {
      formatRegisterList(aregs, result, false, "a");
      formatRegisterList(dregs, result, result.length() > 1, "d");
    } else {
      formatRegisterList(dregs, result, false, "d");
      formatRegisterList(aregs, result, result.length() > 1, "a");
    }
    result.append("]");
    return result.toString();
  }
  private static void formatRegisterList(int[] regs,
                                         StringBuilder buffer,
                                         boolean hasElems,
                                         String prefix) {
    int elems = hasElems ? 1 : 0;
    for (int i = 0; i < regs.length; i++) {
      if (elems > 0) buffer.append(",");
      buffer.append(String.format("%s%d", prefix, regs[i]));
      elems++;
    }
  }
  // **********************************************************************
  // ***** EFFECTIVE ADDRESS GETTERS
  // *********************************************
  private int getAddressRegisterIndirect() {
    return getAddressRegisterValueL(getEffAddrRegnum());
  }
  private int getARIWithDisplacement() {
    return getARIWithDisplacement(getEffAddrRegnum());
  }
  private int getARIWithDisplacement(int basereg) {
    int displacement = mem.readShort(pc);
    pc += 2;
    return getAddressRegisterValueL(basereg) + displacement;
  }
  private int getARIWithIndex() {
    return getARIWithIndex(getEffAddrRegnum());
  }
  private int getARIWithIndex(int basereg) {
    int extword = mem.readShort(pc); pc += 2;
    int displacement8 = getSignExtended8(extword & 0xff);
    int regnum  = (extword >> 12) & 0x07;
    boolean isAddrReg = (extword & 0x8000) == 0x8000;
    boolean isLong = (extword & 0x800) == 0x800;
    int index = 0;
    if (isLong) {
      index = isAddrReg ? getAddressRegisterValueL(regnum) :
        getDataRegisterValueL(regnum);
    } else {
      index = isAddrReg ? getAddressRegisterValueW(regnum) :
        getDataRegisterValueW(regnum);
    }
    return getAddressRegisterValueL(basereg) + index +
      displacement8;
  }
  private int getAbsoluteShort() {
    int retval = mem.readShort(pc);
    pc += 2;
    return retval;
  }
  private int getAbsoluteLong() {
    int retval = mem.readLong(pc);
    pc += 4;
    return retval;
  }
  private int getPCIWithDisplacement() {
    int basepc = pc;
    int displacement = mem.readShort(pc);
    pc += 2;
    int result = basepc + displacement;
    return result;
  }
  private int getPCIWithIndex() {
    int basepc = pc;
    int extword = mem.readShort(pc); pc += 2;
    int displacement8 = getSignExtended8(extword & 0xff);
    int regnum  = (extword >> 12) & 0x07;
    boolean isAddrReg = (extword & 0x8000) == 0x8000;
    boolean isLong = (extword & 0x800) == 0x800;
    int index = 0;
    if (isLong) {
      index = isAddrReg ? getAddressRegisterValueL(regnum) :
        getDataRegisterValueL(regnum);
    } else {
      index = isAddrReg ? getAddressRegisterValueW(regnum) :
        getDataRegisterValueW(regnum);
    }
    return basepc + index + displacement8;
  }

  // **********************************************************************
  // ***** EFFECTIVE ADDRESS VALUE GETTERS
  // *********************************************

  private int getAddressRegisterDirectValueB() {
    return getAddressRegisterValueB(getEffAddrRegnum());
  }
  private int getAddressRegisterDirectValueW() {
    return getAddressRegisterValueW(getEffAddrRegnum());
  }
  private int getAddressRegisterDirectValueL() {
    return getAddressRegisterValueL(getEffAddrRegnum());
  }
  private int getDataRegisterDirectValueB() {
    return dreg[getEffAddrRegnum()] & 0xff;
  }
  private int getDataRegisterDirectValueW() {
    return dreg[getEffAddrRegnum()] & 0xffff;
  }
  private int getDataRegisterDirectValueL() {
    return dreg[getEffAddrRegnum()];
  }
  private int getAddressRegisterIndirectValueB() {
    return mem.readByte(getAddressRegisterValueL(getEffAddrRegnum()));
  }
  private int getAddressRegisterIndirectValueW() {
    return mem.readShort(getAddressRegisterValueL(getEffAddrRegnum()));
  }
  private int getAddressRegisterIndirectValueL() {
    return mem.readLong(getAddressRegisterValueL(getEffAddrRegnum()));
  }
  private int getARIWithPostincrementValueB() {
    int regnum = getEffAddrRegnum();
    int result = mem.readByte(getAddressRegisterValueL(regnum));
    incrementAddressRegister(regnum, 1);
    return result;
  }
  private int getARIWithPostincrementValueW() {
    int regnum = getEffAddrRegnum();
    int result = mem.readShort(getAddressRegisterValueL(regnum));
    incrementAddressRegister(regnum, 2);
    return result;
  }
  private int getARIWithPostincrementValueL() {
    int regnum = getEffAddrRegnum();
    int result = mem.readLong(getAddressRegisterValueL(regnum));
    incrementAddressRegister(regnum, 4);
    return result;
  }
  private int getARIWithPredecrementValueB() {
    int regnum = getEffAddrRegnum();
    incrementAddressRegister(regnum, -1);
    return mem.readByte(getAddressRegisterValueL(regnum));
  }
  private int getARIWithPredecrementValueW() {
    int regnum = getEffAddrRegnum();
    incrementAddressRegister(regnum, -2);
    return mem.readShort(getAddressRegisterValueL(regnum));
  }
  private int getARIWithPredecrementValueL() {
    int regnum = getEffAddrRegnum();
    incrementAddressRegister(regnum, -4);
    return mem.readLong(getAddressRegisterValueL(regnum));
  }
  private int getARIWithDisplacementValueB() {
    return mem.readByte(getARIWithDisplacement());
  }
  private int getARIWithDisplacementValueW() {
    return mem.readShort(getARIWithDisplacement());
  }
  private int getARIWithDisplacementValueL() {
    return mem.readLong(getARIWithDisplacement());
  }
  private int getARIWithIndexValueB() { return mem.readByte(getARIWithIndex()); }
  private int getARIWithIndexValueW() { return mem.readShort(getARIWithIndex()); }
  private int getARIWithIndexValueL() { return mem.readLong(getARIWithIndex()); }
  private int getAbsoluteShortValueB() {
    return mem.readByte(getAbsoluteShort());
  }
  private int getAbsoluteShortValueW() {
    return mem.readShort(getAbsoluteShort());
  }
  private int getAbsoluteShortValueL() {
    return mem.readLong(getAbsoluteShort());
  }
  private int getAbsoluteLongValueB() {
    return mem.readByte(getAbsoluteLong());
  }
  private int getAbsoluteLongValueW() {
    return mem.readShort(getAbsoluteLong());
  }
  private int getAbsoluteLongValueL() {
    return mem.readLong(getAbsoluteLong());
  }
  private int getPCIWithDisplacementValueB() {
    return mem.readByte(getPCIWithDisplacement());
  }
  private int getPCIWithDisplacementValueW() {
    return mem.readShort(getPCIWithDisplacement());
  }
  private int getPCIWithDisplacementValueL() {
    return mem.readLong(getPCIWithDisplacement());
  }
  private int getPCIWithIndexValueB() { return mem.readByte(getPCIWithIndex()); }
  private int getPCIWithIndexValueW() { return mem.readShort(getPCIWithIndex()); }
  private int getPCIWithIndexValueL() { return mem.readLong(getPCIWithIndex()); }
  private int getImmediateValueB() {
    // we read the low-byte of the next word
    int retval = mem.readShort(pc);
    pc += 2;
    return retval & 0xff;
  }
  private int getImmediateValueW() {
    int retval = mem.readShort(pc);
    pc += 2;
    return retval;
  }
  private int getImmediateValueL() {
    int retval = mem.readLong(pc);
    pc += 4;
    return retval;
  }

  // **********************************************************************
  // ***** VALUE GETTERS THAT DO NOT INCREMENT THE PC
  // ***** We need these if the destination operand is storing the result
  // *********************************************
  private int getAbsoluteShortValueSelfStoreB() {
    return mem.readByte(mem.readShort(pc));
  }
  private int getAbsoluteShortValueSelfStoreW() {
    return mem.readShort(mem.readShort(pc));
  }
  private int getAbsoluteShortValueSelfStoreL() {
    return mem.readLong(mem.readShort(pc));
  }
  private int getAbsoluteLongValueSelfStoreB() {
    return mem.readByte(mem.readLong(pc));
  }
  private int getAbsoluteLongValueSelfStoreW() {
    return mem.readShort(mem.readLong(pc));
  }
  private int getAbsoluteLongValueSelfStoreL() {
    return mem.readLong(mem.readLong(pc));
  }

  private int getARIWithDisplacementSelfStore() {
    int displacement = mem.readShort(pc);
    return getAddressRegisterValueL(getEffAddrRegnum()) + displacement;
  }
  private int getARIWithDisplacementValueSelfStoreB() {
    return mem.readByte(getARIWithDisplacementSelfStore());
  }
  private int getARIWithDisplacementValueSelfStoreW() {
    return mem.readShort(getARIWithDisplacementSelfStore());
  }
  private int getARIWithDisplacementValueSelfStoreL() {
    return mem.readLong(getARIWithDisplacementSelfStore());
  }

  // **********************************************************************
  // ***** EFFECTIVE ADDRESS VALUE SETTERS
  // ***** Note: These methods mostly have a regnum parameter. It could
  // ***** ----- mostly be retrieved within the method body - except for
  // ***** move instructions. The generator creates access code for the
  // ***** destination register and therefore needs to specify that here
  // ***** as well. Alternatively, we could add setters for the second
  // ***** effective address parameter
  // *********************************************
  private void setAddressRegisterDirectValueB(int regnum, int value) {
    setAddressRegisterValueB(regnum, value);
  }
  private void setAddressRegisterDirectValueW(int regnum, int value) {
    setAddressRegisterValueW(regnum, value);
  }
  private void setAddressRegisterDirectValueL(int regnum, int value) {
    setAddressRegisterValueL(regnum, value);
  }
  private void setDataRegisterDirectValueB(int regnum, int value) {
    dreg[regnum] = (dreg[regnum] & 0xffffff00) | (value & 0xff);
  }
  private void setDataRegisterDirectValueW(int regnum, int value) {
    dreg[regnum] = (dreg[regnum] & 0xffff0000) | (value & 0xffff);
  }
  private void setDataRegisterDirectValueL(int regnum, int value) {
    dreg[regnum] = value;
  }
  private void setAddressRegisterIndirectValueB(int regnum, int value) {
    mem.writeByte(getAddressRegisterValueL(regnum), value);
  }
  private void setAddressRegisterIndirectValueW(int regnum, int value) {
    mem.writeShort(getAddressRegisterValueL(regnum), value);
  }
  private void setAddressRegisterIndirectValueL(int regnum, int value) {
    mem.writeLong(getAddressRegisterValueL(regnum), value);
  }
  private void setARIWithPostincrementValueB(int regnum, int value) {
    mem.writeByte(getAddressRegisterValueL(regnum), value);
    incrementAddressRegister(regnum, 1);
  }
  private void setARIWithPostincrementValueW(int regnum, int value) {
    mem.writeShort(getAddressRegisterValueL(regnum), value);
    incrementAddressRegister(regnum, 2);
  }
  private void setARIWithPostincrementValueL(int regnum, int value) {
    mem.writeLong(getAddressRegisterValueL(regnum), value);
    incrementAddressRegister(regnum, 4);
  }
  private void setARIWithPredecrementValueB(int regnum, int value) {
    incrementAddressRegister(regnum, -1);
    mem.writeByte(getAddressRegisterValueL(regnum), value);
  }
  private void setARIWithPredecrementValueW(int regnum, int value) {
    incrementAddressRegister(regnum, -2);
    mem.writeShort(getAddressRegisterValueL(regnum), value);
  }
  private void setARIWithPredecrementValueL(int regnum, int value) {
    incrementAddressRegister(regnum, -4);
    mem.writeLong(getAddressRegisterValueL(regnum), value);
  }
  private void setARIWithDisplacementValueB(int regnum, int value) {
    mem.writeByte(getARIWithDisplacement(regnum), value);
  }
  private void setARIWithDisplacementValueW(int regnum, int value) {
    mem.writeShort(getARIWithDisplacement(regnum), value);
  }
  private void setARIWithDisplacementValueL(int regnum, int value) {
    mem.writeLong(getARIWithDisplacement(regnum), value);
  }
  private void setARIWithIndexValueB(int regnum, int value) {
    mem.writeByte(getARIWithIndex(regnum), value);
  }
  private void setARIWithIndexValueW(int regnum, int value) {
    mem.writeShort(getARIWithIndex(regnum), value);
  }
  private void setARIWithIndexValueL(int regnum, int value) {
    mem.writeLong(getARIWithIndex(regnum), value);
  }
  private void setAbsoluteShortValueB(int value) {
    mem.writeByte(getAbsoluteShort(), value);
  }
  private void setAbsoluteShortValueW(int value) {
    mem.writeShort(getAbsoluteShort(), value);
  }
  private void setAbsoluteShortValueL(int value) {
    mem.writeLong(getAbsoluteShort(), value);
  }
  private void setAbsoluteLongValueB(int value) {
    mem.writeByte(getAbsoluteLong(), value);
  }
  private void setAbsoluteLongValueW(int value) {
    mem.writeShort(getAbsoluteLong(), value);
  }
  private void setAbsoluteLongValueL(int value) {
    mem.writeLong(getAbsoluteLong(), value);
  }

  // **********************************************************************
  // ***** CONDITION CODE TESTS
  // *********************************************
  private boolean check_t() { return true; }
  private boolean check_f() { return false; }
  private boolean check_hi() {
    boolean C = (sr & SR_C) == SR_C;
    boolean Z = (sr & SR_Z) == SR_Z;
    return !C && !Z;
  }
  private boolean check_ls() {
    boolean C = (sr & SR_C) == SR_C;
    boolean Z = (sr & SR_Z) == SR_Z;
    return C || Z;
  }
  private boolean check_cc() { return (sr & SR_C) == 0; }
  private boolean check_cs() { return (sr & SR_C) == SR_C; }
  private boolean check_ne() { return (sr & SR_Z) == 0; }
  private boolean check_eq() { return (sr & SR_Z) == SR_Z; }
  private boolean check_vc() { return (sr & SR_V) == 0; }
  private boolean check_vs() { return (sr & SR_V) == SR_V; }
  private boolean check_pl() { return (sr & SR_N) == 0; }
  private boolean check_mi() { return (sr & SR_N) == SR_N; }
  private boolean check_ge() {
    boolean N = (sr & SR_N) == SR_N;
    boolean V = (sr & SR_V) == SR_V;
    return N && V || !N && !V;
  }
  private boolean check_lt() {
    boolean N = (sr & SR_N) == SR_N;
    boolean V = (sr & SR_V) == SR_V;
    return N && !V || !N && V;
  }
  private boolean check_gt() {
    boolean N = (sr & SR_N) == SR_N;
    boolean V = (sr & SR_V) == SR_V;
    boolean Z = (sr & SR_Z) == SR_Z;
    return (N && V && !Z) || (!N && !V && !Z);
  }
  private boolean check_le() {
    boolean N = (sr & SR_N) == SR_N;
    boolean V = (sr & SR_V) == SR_V;
    boolean Z = (sr & SR_Z) == SR_Z;
    return Z || N && !V || !N && V;
  }
  // **********************************************************************
  // ***** Elementary operations
  // *********************************************

$HELPERS

  // ************************************************************************
  // ****** INITIALIZATION
  // ***************************************
  /**
   * A two-level initialization loop: dest ea mode outside, regular ea mode
   * inside so we can reuse setInstrToTableForEaMode().
   * base value is always 0, we just keep a uniform invocation interface
   * for the generator.
   */
  private void setMoveSizeEaEaToTable(Instruction instr, int baseValue,
                                      int size, int eamodeSrc, int eamodeDest) {
    if ((eamodeDest & 0x07) == 0x07) {
      // eamodeExt is special, set it directly and do not iterate over the
      // register numbers
      int value = (size & 0x03) << 12;
      value |= (eamodeDest << 6) & 0x0fc0;
      setInstrToTableForEaMode(instr, value, eamodeSrc);
    } else {
      int value;
      // eamodeDest needs to iterate over register numbers
      for (int regnum = 0; regnum < 8; regnum++) {
        // init value with size and eamode, shift register number by 9
        value = ((size & 0x03) << 12) | (eamodeDest << 6) | (regnum << 9);
        setInstrToTableForEaMode(instr, value, eamodeSrc);
      }
    }
  }

  /**
   * For a given effective address mode, sets all register combinations into
   * the instruction table.
   */
  private void setInstrToTableForEaMode(Instruction instr, int baseValue,
                                        int eamode) {
    // Iterate over all register numbers
    if ((eamode & 0x38) == 0x38) { //  test for special
      int index = baseValue | eamode;
      if (instructions[index] != null) {
        System.out.printf("ALREADY OCCUPIED, INDEX: %04x\n", index);
      }
      instructions[index] = instr;
			usedInstructions++;
    } else {
      for (int regnum = 0; regnum < 8; regnum++) {
        int index = baseValue | (eamode | regnum);
        if (instructions[index] != null) {
          System.out.printf("ALREADY OCCUPIED, INDEX: %04x\n", index);
        }
        instructions[index] = instr;
				usedInstructions++;
      }
    }
  }

  /**
     This method is only placed here as an adapter for the generator to keep
     the code clean.
  */
  private void setInstrToTableForEaMode(Instruction instr, int baseValue,
                                        int size, int eamode, int eamodeExt) {
    setInstrToTableForEaMode(instr, baseValue, eamode);
  }

  /**
   * For a given effective address mode, sets all register combinations into
   * the instruction table. This one provides an outer loop for instructions
   * that use a/d<regnum>, <ea>.
   */
  private void setInstrToTableForRegNumEaMode(Instruction instr,
                                              int baseValue,
                                              int size,
                                              int eamode, int eamodeExt) {
    int baseValueWithRegNum;
    for (int regnum = 0; regnum < 8; regnum++) {
      baseValueWithRegNum = baseValue | (regnum << 9);
      setInstrToTableForEaMode(instr, baseValueWithRegNum, eamode);
    }
  }

  // size, regnum, eamode
  private void setInstrToTableForSizeRegNumEaMode(Instruction instr,
                                                  int baseValue,
                                                  int size,
                                                  int eamode, int eamodeExt) {
    int baseValueWithSize = baseValue | (size << 12);
    setInstrToTableForRegNumEaMode(instr, baseValueWithSize, size, eamode,
                                   eamodeExt);
  }

  // regnum, eamode
  private void setInstrToTableForRegNumSizeEaReg(Instruction instr,
                                                 int baseValue,
                                                 int size,
                                                 int eamode, int eamodeExt) {
    // size sits at bits [7-6]
    int baseValueWithSize = baseValue | (size << 6);
    for (int x = 0; x < 8; x++) {
      for (int y = 0; y < 8; y++) {
        instructions[baseValueWithSize | (x << 9) | y] = instr;
        usedInstructions++;
      }
    }
  }

  /**
   * For a given effective address mode, sets all data value combinations into
   * the instruction table. This one provides an outer loop for instructions
   * that use <data value>, <ea>.
   */
  private void setInstrToTableForDataSizeEaMode(Instruction instr,
                                                int baseValue,
                                                int size,
                                                int eamode, int eamodeExt) {
    int baseValueWithData;
    for (int value = 0; value < 8; value++) {
      baseValueWithData = baseValue | (value << 9) | (size << 6);
      setInstrToTableForEaMode(instr, baseValueWithData, eamode);
    }
  }

  /**
   */
  private void setInstrToTableForSizeEaMode(Instruction instr,
                                            int baseValue,
                                            int size,
                                            int eamode, int eamodeExt) {
    int baseValueWithSize = baseValue | (size << 6);
    setInstrToTableForEaMode(instr, baseValueWithSize, eamode);
  }

  /**
   * Note that this function replaces the size value with the opmode value
   * and will set it into the opmode position. The generator has to take
   * care of properly generating the opmode, because we cannot convert the
   * size to opmode without additional information.
   */
  private void setInstrToTableForRegNumOpModeEaMode(Instruction instr,
                                                    int baseValue,
                                                    int opmode,
                                                    int eamode, int eamodeExt) {
    int baseValueWithRegnum;
    for (int regnum = 0; regnum < 8; regnum++) {
      baseValueWithRegnum = baseValue | (regnum << 9) | (opmode << 6);
      setInstrToTableForEaMode(instr, baseValueWithRegnum, eamode);
    }
  }

  private void setInstrToTableForDataSizeEaReg(Instruction instr, int baseValue,
                                               int size, int eamode,
                                               int eamodeExt) {
    for (int data = 0; data < 8; data++) {
      int baseValueWithSizeAndData = baseValue | (size << 6) | (data << 9);
      for (int eareg = 0; eareg < 8; eareg++) {
        int index = baseValueWithSizeAndData | eareg;
        if (instructions[index] != null) {
          System.out.printf("ALREADY OCCUPIED, INDEX: %04x\n", index);
        }
        instructions[index] = instr;
        usedInstructions++;
      }
    }
  }

  private void setInstrToTableForEaReg(Instruction instr, int baseValue,
                                       int size, int eamode,
                                       int eamodeExt) {
    for (int eareg = 0; eareg < 8; eareg++) {
      int index = baseValue | eareg;
      if (instructions[index] != null) {
        System.out.printf("ALREADY OCCUPIED, INDEX: %04x\n", index);
      }
      instructions[index] = instr;
			usedInstructions++;
    }
  }

  private void setInstrToTableForDisplacement(Instruction instr,
                                              int baseValue,
                                              int opmode, int eamode,
                                              int eamodeExt) {
    for (int displacement = 0; displacement < 256; displacement++) {
      int index = baseValue | displacement;
      if (instructions[index] != null) {
        System.out.printf("ALREADY OCCUPIED, INDEX: %04x\n", index);
      }
      instructions[index] = instr;
			usedInstructions++;
    }
  }

  // there is only one possible decoding
  private void setInstrToTableDirect(Instruction instr,
                                     int index,
                                     int opmode,
                                     int eamode, int eamodeExt) {
    if (instructions[index] != null) {
      System.out.printf("ALREADY OCCUPIED, INDEX: %04x\n", index);
    }
    instructions[index] = instr;
    usedInstructions++;
  }

  /** For a condition code, initialize all Dbcc slots  */
  private void setDbccToTable(Instruction instr, int conditionCode) {
    int baseValue =  0x50c8 | (conditionCode << 8);
    for (int i = 0; i < 8; i++) {
      int index = baseValue | i;
      if (instructions[index] != null) {
        System.out.printf("ALREADY OCCUPIED, INDEX: %04x\n", index);
      }
      instructions[index] = instr;
			usedInstructions++;
    }
  }

  private void setSccToTable(Instruction instr, int conditionCode, int eamode) {
    int baseValue =  0x50c0 | (conditionCode << 8);
    setInstrToTableForEaMode(instr, baseValue, eamode);
  }

  private void setMoveqToTable(Instruction instr) {
    int baseValue = 0x7000;
    for (int regnum = 0; regnum < 8; regnum++) {
      for (int data = 0; data < 256; data++) {
        int index = baseValue | (regnum << 9) | data;
        if (instructions[index] != null) {
          System.out.printf("ALREADY OCCUPIED, INDEX: %04x\n", index);
        }
        instructions[index] = instr;
        usedInstructions++;
      }
    }
  }

  private void setExgToTable(Instruction instr, int opmode) {
    int baseValue = 0xc100 | (opmode << 3);
    for (int rx = 0; rx < 8; rx++) {
      for (int ry = 0; ry < 8; ry++) {
        int index = baseValue | (rx << 9) | ry;
        if (instructions[index] != null) {
          System.out.printf("ALREADY OCCUPIED, INDEX: %04x\n", index);
        }
        instructions[index] = instr;
        usedInstructions++;
      }
    }
  }

  private void setInstrToTableForTrapVector(Instruction instr,
                                            int baseValue,
                                            int opmode, int eamode,
                                            int eamodeExt) {
    for (int trapvector = 0; trapvector < 16; trapvector++) {
      int index = baseValue | trapvector;
      if (instructions[index] != null) {
        System.out.printf("ALREADY OCCUPIED, INDEX: %04x\n", index);
      }
      instructions[index] = instr;
			usedInstructions++;
    }
  }

  private void setInstrToTableForOpmodeEaReg(Instruction instr,
                                             int baseValue,
                                             int opmode, int eamode,
                                             int eamodeExt) {
    for (int regnum = 0; regnum < 8; regnum++) {
      int index = baseValue | (opmode << 6) | regnum;
      if (instructions[index] != null) {
        System.out.printf("ALREADY OCCUPIED, INDEX: %04x\n", index);
      }
      instructions[index] = instr;
			usedInstructions++;
    }
  }

  // ************************************************************************
  // ****** INSTRUCTION TABLE INIT (GENERATED)
  // *******************************************
  /**
   * Populates the instruction table with the correct instruction objects.
   */
  $INIT_INSTRUCTION_TABLE

  // ************************************************************************
  // ****** INSTRUCTION OBJECTS (GENERATED)
  // ***************************************
$INSTRUCTION_OBJECTS
}

