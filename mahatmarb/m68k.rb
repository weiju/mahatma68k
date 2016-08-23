# m68k.rb
# Created on September 22, 2009
# Copyright (c) 2009-2011, Wei-ju Wu
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#   * Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#   * Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#   * Neither the name of Wei-ju Wu nor the
#     names of its contributors may be used to endorse or promote products
#     derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY WEI-JU WU ''AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL WEI-JU WU BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# All knowledge about 68000 decoding and executing instructions is stored in
# this module
# This is similar as seen in Musashi or UAE: From a specification,
# generate an implementation of the CPU.
# Using Ruby as the implementation language for the generator has
# the advantage that the specification can be written in this language
# without losing clarity, but in addition gaining checks and free parsing
# through the Ruby interpreter.
# I tried to change the information structure to take advantage of
# having a scripting language instead of C

# The instruction table:
# Effective addressing modes: (daA+-DXWLpx)
# :an = address register direct
# :dn = data register direct
# :ai = address register indirect
# :ap = ARI with post-increment
# :ar = ARI with pre-decrement
# :ad  = ARI with displacement
# :ax  = ARI with index
# :aw = absolute short
# :al = absolute long
# :pd = program counter indirect with displacement
# :px = program counter indirect with index
# :im  = immediate

# Patterns:
# aaa = address register
# ddd = data register
# DDDDDDDD = 8-bit displacement
# eeeeee = effective address std
# EEEEEE = effective address rev (move.<size> <src>, <dest>)
# mmm = opmode (can be 1, 2 or a)
# ss  = size
# vvv = data value encoded in instruction
# fff = effective address register number (data register)
# FFF = effective address register number (address register)
# tttt = trap vector

# INSTR_DEFS contains all instructions except move
# move is special, because it contains two effective addresses
# [name, size, pattern, modes]
INSTR_DEFS = [
  [:adda,:opmodea,"1101aaammmeeeeee",
   [[:dn,:an,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:addi,[:b,:w,:l],"00000110sseeeeee",[[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:add_dnea, :opmode2,"1101dddmmmeeeeee",[[:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:add_eadn, :opmode1,"1101dddmmmeeeeee",
   [[:dn,:an,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:addq,[:b,:w,:l],"0101vvv0sseeeeee",[[:dn,:an,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:andi,[:b,:w,:l],"00000010sseeeeee",[[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:andi_to_ccr,:b, "0000001000111100",[[]]],
  [:andi_to_sr, :w, "0000001001111100",[[]]],
  [:and_dnea,:opmode2,"1100dddmmmeeeeee", [[:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:and_eadn,:opmode1,"1100dddmmmeeeeee",
   [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:asl_dxdy,[:b,:w,:l],"1110ddd1ss100fff", [[]]], # asl.size <dx>, <dy>
  [:asl_imdn,[:b,:w,:l],"1110vvv1ss000fff", [[]]], # asl.size <im>, <dn>
  [:asr_dxdy,[:b,:w,:l],"1110ddd0ss100fff", [[]]], # asr.size <dx>, <dy>
  [:asr_imdn,[:b,:w,:l],"1110vvv0ss000fff", [[]]], # asr.size <im>, <dn>
  [:bchg_dnea,:none,"0000ddd101eeeeee", [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:bchg_imea,:none,"0000100001eeeeee", [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:bclr_imea,:none,"0000100010eeeeee", [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:bra, :none,"01100000DDDDDDDD",[[]]],
  [:bset_dnea,:none,"0000ddd111eeeeee", [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:bset_imea,:none,"0000100011eeeeee", [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:bsr, :none,"01100001DDDDDDDD",[[]]],
  [:btst_dnea, :none,"0000ddd100eeeeee",
   [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:btst_imea, :none,"0000100000eeeeee",
   [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px]]],
  [:clr,[:b,:w,:l],"01000010sseeeeee", [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:cmp,:opmode1,"1011dddmmmeeeeee",
   [[:dn,:an,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:cmpa,:opmodea,"1011dddmmmeeeeee",
   [[:dn,:an,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:cmpi,[:b,:w,:l],"00001100sseeeeee",
   [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px]]],
  [:cmpm,[:b,:w,:l],"1011aaa1ss001FFF",[[]]],
  [:divu,:w,"1000ddd011eeeeee",
   [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:eor_dnea,:opmode2,"1011dddmmmeeeeee",[[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:eori,[:b,:w,:l],"00001010sseeeeee",[[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:eori_to_ccr,:b, "0000101000111100",[[]]],
  [:eori_to_sr, :w, "0000101001111100",[[]]],
  #bug in ext: opmodes do not quite work !!! opmode a seems inappropriate
  [:ext, :opmodea3, "0100100mmm000fff",[[]]],
  [:jmp, :none,"0100111011eeeeee", [[:ai,:ad,:ax,:aw,:al,:pd,:px]]],
  [:jsr, :none,"0100111010eeeeee", [[:ai,:ad,:ax,:aw,:al,:pd,:px]]],
  [:lea,:l,"0100aaa111eeeeee", [[:ai,:ad,:ax,:aw,:al,:pd,:px]]],
  [:link,:w,"0100111001010fff", [[]]], # should be FFF, but it works !
  # LSx operations
  [:lsl_imdn,[:b,:w,:l],"1110vvv1ss001fff", [[]]], # lsl.size <im>, <dn>
  [:lsr_imdn,[:b,:w,:l],"1110vvv0ss001fff", [[]]], # lsr.size <im>, <dn>
  # move.<size> <ea>, <ea> is the most complex instruction in the 68000
  [:move,[:b,:w,:l],"00ssEEEEEEeeeeee",
   [[:dn,:an,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im], # src
    [:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]] # dest
  ],
  [:movea,[:w,:l],"00ssaaa001eeeeee",
   [[:dn,:an,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  # movem <ea>, list
  [:movem_ealistl,:l,"0100110011eeeeee",
   [[:ai,:ap,:ad,:ax,:aw,:al,:pd,:px]]],
  [:movem_ealistw,:w,"0100110010eeeeee",
   [[:ai,:ap,:ad,:ax,:aw,:al,:pd,:px]]],
  # movem list, <ea>
  [:movem_listeal,:l,"0100100011eeeeee", [[:ai,:ar,:ad,:ax,:aw,:al]]],
  [:movem_listeaw,:w,"0100100010eeeeee", [[:ai,:ar,:ad,:ax,:aw,:al]]],
  [:move_to_ccr,  :w,"0100010011eeeeee",
   [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:move_from_sr,:w, "0100000011eeeeee", [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:move_to_sr,  :w, "0100011011eeeeee",
   [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:move_from_usp,:l,  "0100111001101FFF", [[]]],
  [:move_to_usp,:l,  "0100111001100FFF", [[]]],
  [:mulu,:w,"1100ddd011eeeeee",
   [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:muls,:w,"1100ddd111eeeeee",
   [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:neg,[:b,:w,:l],"01000100sseeeeee",[[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:negx,[:b,:w,:l],"01000000sseeeeee",[[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:not,[:b,:w,:l],"01000110sseeeeee",[[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:nop,:none,     "0100111001110001",[[]]],
  [:or_eadn,:opmode1,"1000dddmmmeeeeee",
   [[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:or_dnea,:opmode2,"1000dddmmmeeeeee",[[:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:ori,[:b,:w,:l],"00000000sseeeeee",[[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:ori_to_ccr,:b, "0000000000111100",[[]]],
  [:ori_to_sr, :w, "0000000001111100",[[]]],
  [:reset,:none, "0100111001110000",[[]]],
  [:rte,:none, "0100111001110011",[[]]],
  [:rtr,:none, "0100111001110111",[[]]],
  [:rts,:none, "0100111001110101",[[]]],
  [:pea,:l,"0100100001eeeeee", [[:ai,:ad,:ax,:aw,:al,:pd,:px]]],
  [:suba,:opmodea,"1001aaammmeeeeee",
   [[:dn,:an,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:sub_dnea, :opmode2,"1001dddmmmeeeeee",[[:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:sub_eadn, :opmode1,"1001dddmmmeeeeee",
   [[:dn,:an,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:subi,[:b,:w,:l],"00000100sseeeeee",[[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:subq,[:b,:w,:l],"0101vvv1sseeeeee",[[:dn,:an,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:swap,:none,"0100100001000fff",[[]]],
  [:tas, :b,"0100101011eeeeee",[[:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]]],
  [:trap,:none,"010011100100tttt",[[]]],
  [:trapv,:none,"0100111001110110",[[]]],
  [:tst, [:b,:w,:l],"01001010sseeeeee",
   # note: an might not be valid for 68000 CPU
   [[:dn,:an,:ai,:ap,:ar,:ad,:ax,:aw,:al,:pd,:px,:im]]],
  [:unlk,:none,"0100111001011FFF", [[]]],
]

# contains all supervisor instructions, at the moment, this only affects
# timing information
SUPERVISOR_INSTRUCTIONS = [
  :andi_to_sr, :eori_to_sr, :move_from_sr, :move_to_sr, :move_from_usp,
  :move_to_usp, :ori_to_sr, :reset, :rte, :stop
]

# Branch conditionally does not have sizes, nor does it have effective
# addresses - just condition code and 8/16-bit displacement
ALL_CONDITION_CODES = [
  :t,  :f, :hi, :ls, :cc, :cs, :ne, :eq, :vc, :vs, :pl, :mi, :ge,
  :lt, :gt, :le
]

BCC_CONDITION_CODES = [:hi, :ls, :cc, :cs, :ne, :eq, :vc, :vs, :pl,
                       :mi, :ge, :lt, :gt, :le]

SCC_EAMODES = [:dn,:ai,:ap,:ar,:ad,:ax,:aw,:al]

# Table 3-19 in PRM
COND_CODE_NUM = {
  :t  => 0,   :f => 1,  :hi => 2, :ls => 3,  :cc => 4,  :cs => 5, :ne => 6,
  :eq => 7,  :vc => 8,  :vs => 9, :pl => 10, :mi => 11, :ge => 12,
  :lt => 13, :gt => 14, :le => 15
}

OUT_DEFS = {
  :adda      => [:effaddr, :aregnum],
  :addi      => [:im_data, :effaddr],
  :add_dnea  => [:dregnum, :effaddr],
  :add_eadn  => [:effaddr, :dregnum],
  :addq      => [:data,    :effaddr],
  :andi      => [:im_data, :effaddr],
  :andi_to_ccr => [:im_data, :ccr],
  :andi_to_sr => [:im_data, :sr],
  :and_dnea  => [:dregnum, :effaddr],
  :and_eadn  => [:effaddr, :dregnum],
  :asl_dxdy  => [:dregnum, :effaddr_dregnum],
  :asl_imdn  => [:data,    :effaddr_dregnum],
  :asr_dxdy  => [:dregnum, :effaddr_dregnum],
  :asr_imdn  => [:data,    :effaddr_dregnum],
  :bchg_dnea => [:dregnum, :effaddr],
  :bchg_imea => [:im_data, :effaddr],
  :bclr_imea => [:im_data, :effaddr],
  :bra       => [:displacement],
  :bset_dnea => [:dregnum, :effaddr],
  :bset_imea => [:im_data, :effaddr],
  :bsr       => [:displacement],
  :btst_dnea => [:dregnum, :effaddr],
  :btst_imea => [:im_data, :effaddr],
  :clr       => [:effaddr],
  :cmp       => [:effaddr, :dregnum],
  :cmpa      => [:effaddr, :aregnum],
  :cmpi      => [:im_data, :effaddr],
  :cmpm      => [:effaddr_aregnum, :aregnum],
  :divu      => [:effaddr, :dregnum],
  :eor_dnea  => [:dregnum, :effaddr],
  :eori      => [:im_data, :effaddr],
  :eori_to_ccr => [:im_data, :ccr],
  :eori_to_sr => [:im_data, :sr],
  :ext       => [:effaddr_dregnum],
  :jmp       => [:effaddr],
  :jsr       => [:effaddr],
  :lea       => [:effaddr, :aregnum],
  :link      => [:effaddr_aregnum, :im_data],
  :lsl_imdn  => [:data,    :effaddr_dregnum],
  :lsr_imdn  => [:data,    :effaddr_dregnum],
  :move      => [:effaddr, :effaddr_rev],
  :movea     => [:effaddr, :aregnum],
  :movem_ealistl => [:effaddr, :reglist],
  :movem_ealistw => [:effaddr, :reglist],
  :movem_listeal => [:reglist, :effaddr],
  :movem_listeaw => [:reglist, :effaddr],
  :move_to_ccr   => [:effaddr, :ccr],
  :move_from_sr  => [:sr, :effaddr],
  :move_to_sr    => [:effaddr, :sr],
  :move_from_usp => [:usp, :effaddr_aregnum],
  :move_to_usp   => [:effaddr_aregnum, :usp],
  :muls      => [:effaddr, :dregnum],
  :mulu      => [:effaddr, :dregnum],
  :neg       => [:effaddr],
  :negx      => [:effaddr],
  :not       => [:effaddr],
  :nop       => [],
  :or_dnea   => [:dregnum, :effaddr],
  :or_eadn   => [:effaddr, :dregnum],
  :ori       => [:im_data, :effaddr],
  :ori_to_ccr => [:im_data, :ccr],
  :ori_to_sr => [:im_data, :sr],
  :pea       => [:effaddr],
  :reset     => [],
  :rte       => [],
  :rtr       => [],
  :rts       => [],
  :suba      => [:effaddr, :aregnum],
  :sub_dnea  => [:dregnum, :effaddr],
  :sub_eadn  => [:effaddr, :dregnum],
  :subi      => [:im_data, :effaddr],
  :subq      => [:data,    :effaddr],
  :swap      => [:effaddr_dregnum],
  :tas       => [:effaddr],
  :trap      => [:trap_vector],
  :trapv     => [],
  :tst       => [:effaddr],
  :unlk      => [:effaddr_aregnum],
}

# movem so far is the most complicated 68000 instruction.
# movem memory -> register
MOVEM_MEMTOREG = [
  "int reglistMask = mem.readShort(pc); pc += 2;",
  "int dregs[] = regnumsInMask(reglistMask & 0xff);",
  "int aregs[] = regnumsInMask((reglistMask >>> 8) & 0xff);",
  "IF_EAMODE(ap)", # we get the indirect address if eamode is postincrement
  "int baseAddress = getAddressRegisterIndirect();",
  "ELSE",
  "int baseAddress = GET_EFFECTIVE_ADDRESS;",
  "ENDIF",
  "for (int i = 0; i < dregs.length; i++) {",
  "  setDataRegisterValue(dregs[i], READ_MEMORY(baseAddress));",
  "  baseAddress += OPERAND_SIZE;",
  "}",
  "for (int i = 0; i < aregs.length; i++) {",
  "  setAddressRegisterValue(aregs[i], READ_MEMORY(baseAddress));",
  "  baseAddress += OPERAND_SIZE;",
  "}",
  "IF_EAMODE(ap)", # store updated address in base register
  "setAddressRegisterValue(getEffAddrRegnum(), baseAddress);",
  "ENDIF"
]

# movem register -> memory is tricky in predecrement mode: everything
# is reversed
MOVEM_REGTOMEM = [
  "int reglistMask = mem.readShort(pc); pc += 2;",
  "IF_EAMODE(ar)",
  # we get the decremented indirect address and reverse the list and
  # processing order if eamode is predecrement
  "int baseAddress = getAddressRegisterIndirect();",
  "int aregs[] = regnumsInMaskRev(reglistMask & 0xff);",
  "int dregs[] = regnumsInMaskRev((reglistMask >>> 8) & 0xff);",
  "for (int i = 0; i < aregs.length; i++) {",
  "  baseAddress -= OPERAND_SIZE;",
  "  WRITE_MEMORY(baseAddress, getAddressRegisterValue(aregs[i]));",
  "}",
  "for (int i = 0; i < dregs.length; i++) {",
  "  baseAddress -= OPERAND_SIZE;",
  "  WRITE_MEMORY(baseAddress, getDataRegisterValue(dregs[i]));",
  "}",

  # store the updated address in predecrement mode
  "setAddressRegisterValue(getEffAddrRegnum(), baseAddress);",
  "ELSE",
  "int dregs[] = regnumsInMask(reglistMask & 0xff);",
  "int aregs[] = regnumsInMask((reglistMask >>> 8) & 0xff);",
  "int baseAddress = GET_EFFECTIVE_ADDRESS;",
  "for (int i = 0; i < dregs.length; i++) {",
  "  WRITE_MEMORY(baseAddress, getDataRegisterValue(dregs[i]));",
  "  baseAddress += OPERAND_SIZE;",
  "}",
  "for (int i = 0; i < aregs.length; i++) {",
  "  WRITE_MEMORY(baseAddress, getAddressRegisterValue(aregs[i]));",
  "  baseAddress += OPERAND_SIZE;",
  "}",
  "ENDIF",
]

# Instructions, defining it this way has the advantage that we
# can specify the CPU specifics more abstract
INSTR_CODE = {
  :adda       => ["int src = GET_EFFADDR_VALUE;",
                  # adda operates on the entire address register
                  "int dest = getAddressRegisterValueL(getRegnum());",
                  "setAddressRegisterValueL(getRegnum(), src + dest);"
                 ],
  :addi       => ["int src = GET_IMMEDIATE_VALUE;",
                  "int dest = GET_EFFADDR_VALUE_STORE;",
                  "int result = ADD(src, dest);",
                  "SET_EFFADDR_VALUE(result);"
                 ],
  :add_dnea   => ["int src = GET_DATA_REGISTER;",
                  "int dest = GET_EFFADDR_VALUE_STORE;",
                  "int result = ADD(src, dest);",
                  "SET_EFFADDR_VALUE(result);"
                 ],
  :add_eadn   => ["int src = GET_EFFADDR_VALUE;",
                  "int dest = GET_DATA_REGISTER;",
                  "int result = ADD(src, dest);",
                  "SET_DATA_REGISTER(result);"
                 ],
  :addq       => ["int dest = GET_EFFADDR_VALUE_STORE;",
                  "int src = GET_DATA;",
                  "if (src == 0) src = 8;",
                  "int result = ADD(src, dest);",
                  "SET_EFFADDR_VALUE(result);"
                 ],
  :andi      => ["int src = GET_IMMEDIATE_VALUE;",
                 "int dest = GET_EFFADDR_VALUE_STORE;",
                 "int result = AND(src, dest);",
                 "SET_EFFADDR_VALUE(result);"
                ],
  :andi_to_ccr => ["setCCR(getCCR() & GET_IMMEDIATE_VALUE);"],
  :andi_to_sr => ["if (isSupervisorMode()) {",
                  "  int data = GET_IMMEDIATE_VALUE;",
                  "  sr &= data;",
                  "} else privilegeViolation();"
                 ],
  :and_dnea  => ["int src = GET_DATA_REGISTER;",
                 "int dest = GET_EFFADDR_VALUE_STORE;",
                 "int result = AND(src, dest);",
                 "SET_EFFADDR_VALUE(result);"
                ],
  :and_eadn  => ["int src = GET_EFFADDR_VALUE;",
                 "int dest = GET_DATA_REGISTER;",
                 "int result = AND(src, dest);",
                 "SET_DATA_REGISTER(result);"
                ],
  :asl_dxdy  => ["int shiftcount = GET_DATA_REGISTER & 0x3f;",
                 "int result = ASL(GET_DREG_EA, shiftcount);",
                 "SET_DREG_EA(result);"
                ],
  :asl_imdn  => ["int shiftcount = getDataValue();",
                 "if (shiftcount == 0) shiftcount = 8;",
                 "int result = ASL(GET_DREG_EA, shiftcount);",
                 "SET_DREG_EA(result);"
                ],
  :asr_dxdy  => ["int shiftcount = GET_DATA_REGISTER & 0x3f;",
                 "int result = ASR(GET_DREG_EA, shiftcount);",
                 "SET_DREG_EA(result);"
                ],
  :asr_imdn  => ["int shiftcount = getDataValue();",
                 "if (shiftcount == 0) shiftcount = 8;",
                 "int result = ASR(GET_DREG_EA, shiftcount);",
                 "SET_DREG_EA(result);"
                ],
  :bchg_dnea => ["int bitnum = getDataRegisterValueL(getRegnum());",
                 "IF_EAMODE(dn)",
                 "int mask = 1 << (bitnum & 0x1f);",
                 "int dest = GET_EFFADDR_VALUE_STORE_L;",
                 "boolean bitSet = (dest & mask) == mask;",
                 "if (bitSet) SET_EFFADDR_VALUE_L(dest & ~mask);",
                 "else SET_EFFADDR_VALUE_L(dest | mask);",
                 "ELSE",
                 "int mask = 1 << (bitnum & 0x07);",
                 "int dest= GET_EFFADDR_VALUE_STORE_B;",
                 "boolean bitSet = (dest & mask) == mask;",
                 "if (bitSet) SET_EFFADDR_VALUE_B(dest & ~mask);",
                 "else SET_EFFADDR_VALUE_B(dest | mask);",
                 "ENDIF",
                 "if (bitSet) clearFlags(SR_Z);",
                 "else setZeroFlag();"
                ],
  :bchg_imea => ["int bitnum = getImmediateValueB();",
                 "IF_EAMODE(dn)",
                 "int mask = 1 << (bitnum & 0x1f);",
                 "int dest = GET_EFFADDR_VALUE_STORE_L;",
                 "boolean bitSet = (dest & mask) == mask;",
                 "if (bitSet) SET_EFFADDR_VALUE_L(dest & ~mask);",
                 "else SET_EFFADDR_VALUE_L(dest | mask);",
                 "ELSE",
                 "int mask = 1 << (bitnum & 0x07);",
                 "int dest= GET_EFFADDR_VALUE_STORE_B;",
                 "boolean bitSet = (dest & mask) == mask;",
                 "if (bitSet) SET_EFFADDR_VALUE_B(dest & ~mask);",
                 "else SET_EFFADDR_VALUE_B(dest | mask);",
                 "ENDIF",
                 "if (bitSet) clearFlags(SR_Z);",
                 "else setZeroFlag();"
                ],
  :bclr_imea => ["int bitnum = getImmediateValueB();",
                 "IF_EAMODE(dn)",
                 "int mask = 1 << (bitnum & 0x1f);",
                 "int src = GET_EFFADDR_VALUE_STORE_L;",
                 "SET_EFFADDR_VALUE_L(src & ~mask);",
                 "ELSE",
                 "int mask = 1 << (bitnum & 0x07);",
                 "int src = GET_EFFADDR_VALUE_STORE_B;",
                 "SET_EFFADDR_VALUE_B(src & ~mask);",
                 "ENDIF",
                 "if ((src & mask) == 0) setZeroFlag();",
                 "else clearFlags(SR_Z);"
                ],
  :bset_dnea => ["int bitnum = getDataRegisterValueL(getRegnum());",
                 "IF_EAMODE(dn)",
                 "int mask = 1 << (bitnum & 0x1f);",
                 "int src = GET_EFFADDR_VALUE_STORE_L;",
                 "SET_EFFADDR_VALUE_L(src | mask);",
                 "ELSE",
                 "int mask = 1 << (bitnum & 0x07);",
                 "int src = GET_EFFADDR_VALUE_STORE_B;",
                 "SET_EFFADDR_VALUE_B(src | mask);",
                 "ENDIF",
                 "if ((src & mask) == 0) setZeroFlag();",
                 "else clearFlags(SR_Z);"
                ],
  :bset_imea => ["int bitnum = getImmediateValueB();",
                 "IF_EAMODE(dn)",
                 "int mask = 1 << (bitnum & 0x1f);",
                 "int src = GET_EFFADDR_VALUE_STORE_L;",
                 "SET_EFFADDR_VALUE_L(src | mask);",
                 "ELSE",
                 "int mask = 1 << (bitnum & 0x07);",
                 "int src = GET_EFFADDR_VALUE_STORE_B;",
                 "SET_EFFADDR_VALUE_B(src | mask);",
                 "ENDIF",
                 "if ((src & mask) == 0) setZeroFlag();",
                 "else clearFlags(SR_Z);"
                ],
  :btst_dnea => ["IF_EAMODE(dn)",
                 "int bitnum = getDataRegisterValueL(getRegnum()) & 0x1f;",
                 "int dest = GET_EFFADDR_VALUE_L;",
                 "ELSE",
                 "int bitnum = getDataRegisterValueL(getRegnum()) & 0x07;",
                 "int dest = GET_EFFADDR_VALUE_B;",
                 "ENDIF",
                 "int mask = 1 << bitnum;",
                 "if ((dest & mask) == 0) setZeroFlag();",
                 "else clearFlags(SR_Z);"
                ],
  :btst_imea => ["IF_EAMODE(dn)",
                 "int bitnum = getImmediateValueB() & 0x1f;",
                 "int dest = GET_EFFADDR_VALUE_L;",
                 "ELSE",
                 "int bitnum = getImmediateValueB() & 0x07;",
                 "int dest = GET_EFFADDR_VALUE_B;",
                 "ENDIF",
                 "int mask = 1 << bitnum;",
                 "if ((dest & mask) == 0) setZeroFlag();",
                 "else clearFlags(SR_Z);"
                ],
  :bra       => ["int basepc = pc;",
                 "pc = basepc + getDisplacement();"
                ],
  :bsr       => ["int basepc = pc;",
                 "int displacement = getDisplacement();",
                 "pushLong(pc);",
                 "pc = basepc + displacement;"
                ],
  :clr       => ["clearFlags(SR_N, SR_V, SR_C);",
                 "SET_EFFADDR_VALUE(0);",
                 "setZeroFlag();",
                ],
  :cmp       => ["clearFlags(SR_N, SR_Z, SR_V, SR_C);",
                 "int src = GET_EFFADDR_VALUE;",
                 "int dest = GET_DATA_REGISTER;",
                 "SUBTRACT(src, dest);"
                ],
  :cmpa      => ["clearFlags(SR_N, SR_Z, SR_V, SR_C);",
                 "int src = GET_EFFADDR_VALUE;",
                 "int dest = GET_ADDRESS_REGISTER;",
                 "SUBTRACT(src, dest);"
                ],
  :cmpi      => ["clearFlags(SR_N, SR_Z, SR_V, SR_C);",
                 "int src = GET_IMMEDIATE_VALUE;",
                 "int dest = GET_EFFADDR_VALUE;",
                 "SUBTRACT(src, dest);"
                ],
  :cmpm      => ["clearFlags(SR_N, SR_Z, SR_V, SR_C);",
                 "int x = getRegnum();",
                 "int y = getEffAddrRegnum();",
                 "int destAddr = getAddressRegisterValueL(x);",
                 "int srcAddr = getAddressRegisterValueL(y);",
                 "SUBTRACT(READ_MEMORY(srcAddr), READ_MEMORY(destAddr));",
                 "setAddressRegisterValueL(x, destAddr + OPERAND_SIZE);",
                 "setAddressRegisterValueL(y, srcAddr + OPERAND_SIZE);"
                ],
  :divu      => ["clearFlags(SR_N, SR_Z, SR_V, SR_C);",
                 # destination is 32 bit, src is 16 bit
                 # ensure that dest is unsigned !!
                 "int src = GET_EFFADDR_VALUE & 0xffff;",
                 "long dest = getDataRegisterValueL(getRegnum()) & 0xffffffff;",
                 "if (src == 0) divisionByZero();",
                 "else {",
                 "  int quotient = ((int) (dest / src)) & 0xffff;",
                 "  int remainder = ((int) (dest % src)) & 0xffff;",
                 "  int result = (remainder << 16) | quotient;",
                 # condition codes - we assume that there can't be an
                 # overflow with integer division
                 "  if (msbSetW(quotient)) setNegativeFlag();",
                 "  if (isZeroW(quotient)) setZeroFlag();",
                 "  setDataRegisterDirectValueL(getRegnum(), result);",
                 "}"
                ],
  :eor_dnea  => ["int src = GET_DATA_REGISTER;",
                 "int dest = GET_EFFADDR_VALUE_STORE;",
                 "int result = EOR(src, dest);",
                 "SET_EFFADDR_VALUE(result);"
                ],
  :eori      => ["int src = GET_IMMEDIATE_VALUE;",
                 "int dest = GET_EFFADDR_VALUE_STORE;",
                 "int result = EOR(src, dest);",
                 "SET_EFFADDR_VALUE(result);"
                ],
  :eori_to_ccr => ["setCCR(getCCR() ^ GET_IMMEDIATE_VALUE);"],
  :eori_to_sr => ["if (isSupervisorMode()) {",
                  "  int data = GET_IMMEDIATE_VALUE;",
                  "  sr ^= data;",
                  "} else privilegeViolation();"
                 ],
  :ext => ["clearFlags(SR_N, SR_Z, SR_V, SR_C);",
           "int result = ",
           "IF_SIZE(b)",
           "  getDataRegisterValueB(getEffAddrRegnum());",
           "setDataRegisterDirectValueW(getEffAddrRegnum(), result);",
           "if (isZeroW(result)) setZeroFlag();",
           "if (msbSetW(result)) setNegativeFlag();",
           "IF_SIZE(w)",
           "  getDataRegisterValueW(getEffAddrRegnum());",
           "setDataRegisterDirectValueL(getEffAddrRegnum(), result);",
           "if (isZeroL(result)) setZeroFlag();",
           "if (msbSetL(result)) setNegativeFlag();",
           "IF_SIZE(l)", # l, sign-extend low-order byte to long
           "  getDataRegisterValueB(getEffAddrRegnum());",
           "setDataRegisterDirectValueL(getEffAddrRegnum(), result);",
           "if (isZeroL(result)) setZeroFlag();",
           "if (msbSetL(result)) setNegativeFlag();",
           "ENDIF"
          ],
  :jmp       => ["pc = GET_EFFECTIVE_ADDRESS;"],
  :jsr       => ["int jumpLabel = GET_EFFECTIVE_ADDRESS;",
                 "if (!intercept_jsr(jumpLabel)) {",
                 "  pushLong(pc);",
                 "  pc = jumpLabel;",
                 "}"
                ],
  :lea       => ["SET_ADDRESS_REGISTER(GET_EFFECTIVE_ADDRESS);"],
  :link      => ["pushLong(getAddressRegisterValueL(getEffAddrRegnum()));",
                 "setAddressRegisterValueL(getEffAddrRegnum(),",
                 "                         getStackPointerValueL());",
                 "incrementStackPointer(GET_IMMEDIATE_VALUE);"
                ],
  :lsl_imdn  => ["int shiftcount = getDataValue();",
                 "if (shiftcount == 0) shiftcount = 8;",
                 "int result = LSL(GET_DREG_EA, shiftcount);",
                 "SET_DREG_EA(result);"
                ],
  :lsr_imdn  => ["int shiftcount = getDataValue();",
                 "if (shiftcount == 0) shiftcount = 8;",
                 "int result = LSR(GET_DREG_EA_U, shiftcount);",
                 "SET_DREG_EA(result);"
                ],
  :move      => ["clearFlags(SR_N, SR_Z, SR_V, SR_C);",
                 "int tmp = GET_EFFADDR_VALUE;",
                 "if (MSB_SET(tmp)) setNegativeFlag();",
                 "if (IS_ZERO(tmp)) setZeroFlag();",
                 "SET_EFFADDR_EXT_VALUE(tmp);"
                ],
  :movea     => ["SET_ADDRESS_REGISTER(GET_EFFADDR_VALUE);"],
  :movem_ealistl => MOVEM_MEMTOREG,
  :movem_ealistw => MOVEM_MEMTOREG,
  :movem_listeal => MOVEM_REGTOMEM,
  :movem_listeaw => MOVEM_REGTOMEM,
  :moveq     => ["clearFlags(SR_N, SR_Z, SR_V, SR_C);",
                 "int data = getSignExtended8(currentInstrWord & 0xff);",
                 "if (msbSetL(data)) setNegativeFlag();",
                 "if (isZeroL(data)) setZeroFlag();",
                 "setDataRegisterDirectValueL(getRegnum(), data);"
                ],
  :move_to_ccr  => ["setCCR(GET_EFFADDR_VALUE);"],
  :move_from_sr => ["if (isSupervisorMode()) {",
                    "  SET_EFFADDR_VALUE(sr);",
                    "} else privilegeViolation();"
                   ],
  :move_to_sr  => ["if (isSupervisorMode()) {",
                   "  sr = GET_EFFADDR_VALUE;",
                   "} else privilegeViolation();"
                  ],
  :move_from_usp => ["if (isSupervisorMode()) {",
                     "  int usp = getUserStackPointerValue();",
                     "  setAddressRegisterValueL(getEffAddrRegnum(), usp);",
                     "} else privilegeViolation();"
                    ],
  :move_to_usp => ["if (isSupervisorMode()) {",
                   "  int data = getAddressRegisterValueL(getEffAddrRegnum());",
                   "  setUserStackPointerValue(data);",
                   "} else privilegeViolation();"
                  ],
  :muls      => ["clearFlags(SR_N, SR_Z, SR_V, SR_C);",
                 # operands are 16-bit
                 "int src = getSignExtended16(GET_EFFADDR_VALUE & 0xffff);",
                 "int dest = getSignExtended16(GET_DATA_REGISTER & 0xffff);",
                 "int result = src * dest;",
                 # result is 32-bit number
                 "if (msbSetL(result)) setNegativeFlag();",
                 "if (isZeroL(result)) setZeroFlag();",
                 "setDataRegisterDirectValueL(getRegnum(), result);"
                ],
  :mulu      => ["clearFlags(SR_N, SR_Z, SR_V, SR_C);",
                 # operands are 16-bit
                 "int src = GET_EFFADDR_VALUE & 0xffff;",
                 "int dest = GET_DATA_REGISTER & 0xffff;",
                 "int result = src * dest;",
                 # result is 32-bit number
                 "if (msbSetL(result)) setNegativeFlag();",
                 "if (isZeroL(result)) setZeroFlag();",
                 "setDataRegisterDirectValueL(getRegnum(), result);"
                ],
  :neg       => ["int dest = GET_EFFADDR_VALUE_STORE;",
                 "int result = NEG(dest);",
                 "SET_EFFADDR_VALUE(result);"
                ],
  :negx      => ["int dest = GET_EFFADDR_VALUE_STORE;",
                 "int result = NEGX(dest);",
                 "SET_EFFADDR_VALUE(result);"
                ],
  :not       => ["int dest = GET_EFFADDR_VALUE_STORE;",
                 "int result = NOT(dest);",
                 "SET_EFFADDR_VALUE(result);"
                ],
  :nop       => [],
  :or_dnea   => ["int src = GET_DATA_REGISTER;",
                 "int dest = GET_EFFADDR_VALUE_STORE;",
                 "int result = OR(src, dest);",
                 "SET_EFFADDR_VALUE(result);"
                ],
  :or_eadn   => ["int src = GET_EFFADDR_VALUE;",
                 "int dest = GET_DATA_REGISTER;",
                 "int result = OR(src, dest);",
                 "SET_DATA_REGISTER(result);"
                ],
  :ori       => ["int src = GET_IMMEDIATE_VALUE;",
                 "int dest = GET_EFFADDR_VALUE_STORE;",
                 "int result = OR(src, dest);",
                 "SET_EFFADDR_VALUE(result);"
                ],
  :ori_to_ccr => ["setCCR(getCCR() | GET_IMMEDIATE_VALUE);"],
  :ori_to_sr => ["if (isSupervisorMode()) {",
                 "  int data = GET_IMMEDIATE_VALUE;",
                 "  sr |= data;",
                 "} else privilegeViolation();"
                ],
  :pea       => ["pushLong(GET_EFFECTIVE_ADDRESS);"],
  :reset     => ["if (isSupervisorMode()) {",
                 "  if (resetHandler != null) resetHandler.resetSystem();",
                 "} else privilegeViolation();",
                ],
  :rte       => ["if (isSupervisorMode()) {",
                 "int tmpsr = popShort();", # store it temp so
                 "pc = popLong();", # we will pop the pc from the correct stack
                 "sr = tmpsr;",
                 "} else privilegeViolation();"
                ],
  :rtr       => ["setCCR(popShort());", # only use the low 8 bits
                 "pc = popLong();"
                ],
  :rts       => ["pc = popLong();"],
  :suba      => ["int src = GET_EFFADDR_VALUE;",
                 "int dest = GET_ADDRESS_REGISTER;",
                 "SET_ADDRESS_REGISTER(dest - src);"
                ],
  :sub_dnea  => ["clearFlags(SR_X, SR_N, SR_Z, SR_V, SR_C);",
                 "int src = GET_DATA_REGISTER;",
                 "int dest = GET_EFFADDR_VALUE_STORE;",
                 "int result = SUBTRACT(src, dest);",
                 "if (isCarrySet()) setExtendedFlag();",
                 "SET_EFFADDR_VALUE(result);"
                ],
  :sub_eadn  => ["clearFlags(SR_X, SR_N, SR_Z, SR_V, SR_C);",
                 "int src = GET_EFFADDR_VALUE;",
                 "int dest = GET_DATA_REGISTER;",
                 "int result = SUBTRACT(src, dest);",
                 "if (isCarrySet()) setExtendedFlag();",
                 "SET_DATA_REGISTER(result);"
                ],
  :subi      => ["clearFlags(SR_X, SR_N, SR_Z, SR_V, SR_C);",
                 "int src = GET_IMMEDIATE_VALUE;",
                 "int dest = GET_EFFADDR_VALUE_STORE;",
                 "int result = SUBTRACT(src, dest);",
                 "if (isCarrySet()) setExtendedFlag();",
                 "SET_EFFADDR_VALUE(result);"
                ],
  :subq      => ["clearFlags(SR_X, SR_N, SR_Z, SR_V, SR_C);",
                 "int dest = GET_EFFADDR_VALUE_STORE;",
                 "int src = GET_DATA;",
                 "if (src == 0) src = 8;",
                 "int result = SUBTRACT(src, dest);",
                 "if (isCarrySet()) setExtendedFlag();",
                 "SET_EFFADDR_VALUE(result);"
                ],
  :swap      => ["clearFlags(SR_N, SR_Z, SR_V, SR_C);",
                 "int value = dreg[getEffAddrRegnum()];",
                 "int result = (value << 16) | (value >>> 16);",
                 "if (isZeroL(result)) setZeroFlag();",
                 "if (msbSetL(result)) setNegativeFlag();",
                 "dreg[getEffAddrRegnum()] = result;"
                ],
  :tas       => ["clearFlags(SR_N, SR_Z, SR_V, SR_C);",
                 "int value = GET_EFFADDR_VALUE_STORE;",
                 "if (IS_ZERO(value)) setZeroFlag();",
                 "if (MSB_SET(value)) setNegativeFlag();",
                 "SET_EFFADDR_VALUE(value | 0x80);"
                ],
  :trap      => ["doException(getTrapVector());"],
  :trapv     => ["if (isOverflowSet()) doException(7);"],
  :tst       => ["clearFlags(SR_N, SR_Z, SR_V, SR_C);",
                 "int value = GET_EFFADDR_VALUE;",
                 "if (IS_ZERO(value)) setZeroFlag();",
                 "if (MSB_SET(value)) setNegativeFlag();"
                ],
  :bcc       => ["boolean result = CC_CHECK;",
                 "int basepc = pc;",
                 "int displacement = getDisplacement();",
                 "if (result) pc = basepc + displacement;"
                ],
  :dbcc      => ["boolean result = CC_CHECK; ",
                 "int basepc = pc;",
                 "int displacement = getAbsoluteShort();",
                 "if (!result) {",
                 "  int regnum = getEffAddrRegnum();",
                 "  short tmp = (short) (dreg[regnum] & 0xffff);",
                 "  tmp--;",
                 "  dreg[regnum] &= 0xffff0000;",
                 "  dreg[regnum] |= (tmp & 0xffff);",
                 "  if (tmp != -1) {",
                 "    pc = basepc + displacement;",
                 "  }",
                 "}"
                ],
  :scc       => ["if (CC_CHECK) {",
                 "  SET_EFFADDR_VALUE(0xff);",
                 "} else {",
                 "  SET_EFFADDR_VALUE(0);",
                 "}"
                ],
  :unlk      => ["setStackPointerValue(getAddressRegisterValueL" +
                 "(getEffAddrRegnum()));",
                 "setAddressRegisterValueL(getEffAddrRegnum()," +
                 " mem.readLong(getStackPointerValueL()));",
                 "incrementStackPointer(4);"
                ],
}

# effective addressing mode values
EAMODE_SPECIAL = 7

# mapping from eamode symbols to their equivalent 3-bit values
EAMODE_PATTERNS = {
  :dn => 0, :an => 1, :ai => 2, :ap => 3, :ar => 4, :ad => 5, :ax => 6,
  :aw => 7, :al => 7, :pd => 7, :px => 7, :im => 7
}

# for special mode, returns the regnum value for the addressing mode
EAMODE_SPECIAL_REGNUM = {
  :aw => 0, :al => 1, :pd => 2, :px => 3, :im => 4
}

# sizes
MOVE_SIZE_NUM    = { :b => 1, :w => 3, :l => 2 }

# SIZE_NUM = OPMODE1
SIZE_NUM    = { :b => 0, :w => 1, :l => 2,:none => -1 }

# opmodes
OPMODEA     = { :w => 3, :l => 7 } # dest is address register
OPMODEA3    = { :b => 2, :w => 3, :l => 7 } # dest is address register
OPMODE1     = { :b => 0, :w => 1, :l => 2 }
OPMODE2     = { :b => 4, :w => 5, :l => 6 }

######################################################################
#### Functions to extraction the above specification
#### into InstructionSpec objects
######################################################################

# data structure to define a regular instruction
class InstructionSpec
  attr_accessor :mnemonic, :size, :base_value, :eamodes, :varpatterns,
                :eamodes_ext, :out_spec

  def initialize(mnemonic, size, base_value, varpatterns, eamodes,
                 eamodes_ext)
    @mnemonic = mnemonic
    @size = size
    @base_value = base_value
    @varpatterns = varpatterns
    #puts "eamodes: #{eamodes}"
    @eamodes = eamodes           # supported eamodes
    @eamodes_ext = eamodes_ext   # supported eamodes (for move dest)
  end
end

# Indexes in an instruction specification
MNEMONIC_POS = 0
SIZE_POS     = 1
PATTERN_POS  = 2
EAMODES_POS  = 3

# processes an instruction definition and creates
# and InstructionSpec object
PATTERN_SYMBOL_EASKIP = {
  ?a => [:aregnum, 2],
  ?d => [:dregnum, 2],
  ?e => [:effaddr, 5],
  ?E => [:effaddr_rev, 5],
  ?m => [:opmode, 2],
  ?s => [:size, 1],
  ?v => [:data, 2],
  ?D => [:displacement, 7],
  ?f => [:effaddr_dregnum, 2],
  ?F => [:effaddr_aregnum, 2],
  ?t => [:trap_vector, 3]
}

def extract_spec(instr_def)
  mnemonic = instr_def[MNEMONIC_POS]
  pattern =  instr_def[PATTERN_POS]
  varpatterns = []
  varpos = {}
  # there could be two eamode specs in an instruction, eamode_num
  # stores whether it is the first or second eamode spec
  eamode_num = 0
  # build base pattern
  base_value = 0
  i = 0

  while i < pattern.length do
    char = pattern[i]
    base_value <<= 1
    skip = 0
    if char == ?1 then
         base_value |= 1
    elsif char == ?0 then
      # do nothing
    else
      varpatterns <<= PATTERN_SYMBOL_EASKIP[char][0]
      skip = PATTERN_SYMBOL_EASKIP[char][1]
    end
    i += (1 + skip)
    base_value <<= skip
  end
  eamodes = instr_def[EAMODES_POS]
  puts "processing: #{instr_def[MNEMONIC_POS]}:#{pattern} baseval: #{base_value} varpatterns: [#{varpatterns.join(',')}]..."

  allowed_eamodes = instr_def[EAMODES_POS][0]
  allowed_eamodes_ext = nil
  if instr_def[EAMODES_POS].length > 1 then
    allowed_eamodes_ext = instr_def[EAMODES_POS][1]
  end

  instr_spec = InstructionSpec.new(mnemonic, instr_def[SIZE_POS],
                                   base_value,
                                   varpatterns, allowed_eamodes,
                                   allowed_eamodes_ext)
  instr_spec.out_spec = OUT_DEFS[mnemonic]
  instr_spec
end

# reads the instruction specification to generate a more
# processing friendly presentation from the more readable presentation
# in INSTR_DEFS
def create_instr_specs()
  instr_specs = []
  INSTR_DEFS.each do |instr_def|
	instr_specs <<= extract_spec(instr_def)
  end
  instr_specs
end
