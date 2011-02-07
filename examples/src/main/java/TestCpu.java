/**
 * Created on October 29, 2009
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
import org.mahatma68k.*;
import java.nio.ByteBuffer;

/**
 * This is a simple test program that executes the first three lines of
 * the best operating system of all time: Amiga OS.
 */
public class TestCpu {

  /**
   * Here we implement a simple address space object that contains
   * our code.
   */
  static class DemoAddressSpace implements AddressSpace {
    private byte[] data = {
      0x4f, (byte) 0xf9, 0x00, 0x04, 0x00, 0x00,
      0x20, 0x3c, 0x00, 0x02, 0x00, 0x00,
      0x53, (byte) 0x80
    };
    private ByteBuffer buffer = ByteBuffer.wrap(data);
    public int start() { return 0; }
    public int size() { return buffer.capacity(); }

    public int readByte(int address) { return buffer.get(address); }
    public int readShort(int address) { return buffer.getShort(address); }
    public int readLong(int address) { return buffer.getInt(address); }
    public void writeByte(int address, int value) {
      buffer.put(address, (byte) value);
    }
    public void writeShort(int address, int value) {
      buffer.putShort(address, (short) value);
    }
    public void writeLong(int address, int value) {
      buffer.putInt(address, value);
    }
  }

  /**
   * We print the CPU's state before and after we we executed the tree lines
   * of "Amiga Exec".
   */
	public static void main(String[] args) {
		Cpu cpu = new Cpu();
    cpu.setAddressSpace(new DemoAddressSpace());
    System.out.println("BEFORE: ");
    System.out.println(cpu.getState());
    for (int i = 0; i < 3; i++) {
      int pc = cpu.getPC();
      Cpu.Instruction instr = cpu.nextInstruction();
      System.out.printf("$%04x: %s\n", pc, instr.toString());
      instr.execute();
    }
    System.out.println("\nAFTER: ");
    System.out.println(cpu.getState());
	}
}

