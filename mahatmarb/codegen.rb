# codegen.rb
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

# Java code generation helpers
# This module was introduced to eliminate some silly duplication -
# we really do not want to define computing operations three times that
# only differ in their operator size
# It also contains the generation functionality for the instructions
# that do not fit the standard scheme
require 'mahatmarb/timing'
#######################################################################
##### Mappings to initialization functions
#######################################################################
VARPATTERNS_TO_INIT_FUNC = {
  [:aregnum, :effaddr] => "setInstrToTableForRegNumEaMode",
  [:dregnum, :effaddr] => "setInstrToTableForRegNumEaMode",
  [:aregnum, :opmode, :effaddr] => "setInstrToTableForRegNumOpModeEaMode",
  [:dregnum, :opmode, :effaddr] => "setInstrToTableForRegNumOpModeEaMode",
  [:size, :effaddr_rev, :effaddr] => "setMoveSizeEaEaToTable",
  [:data, :size, :effaddr] => "setInstrToTableForDataSizeEaMode",
  [:size, :effaddr] => "setInstrToTableForSizeEaMode",
  [:size, :aregnum, :effaddr] => "setInstrToTableForSizeRegNumEaMode",
  [:aregnum, :size, :effaddr_aregnum] => "setInstrToTableForRegNumSizeEaReg",
  [:effaddr] => "setInstrToTableForEaMode",
  [:displacement] => "setInstrToTableForDisplacement",
  [:data, :size, :effaddr_dregnum] => "setInstrToTableForDataSizeEaReg",
  [:dregnum, :size, :effaddr_dregnum] => "setInstrToTableForDataSizeEaReg",
  [:effaddr_dregnum] => "setInstrToTableForEaReg",
  [:effaddr_aregnum] => "setInstrToTableForEaReg",
  [:trap_vector] => "setInstrToTableForTrapVector",
  [:opmode, :effaddr_dregnum] => "setInstrToTableForOpmodeEaReg",
  [] => "setInstrToTableDirect",
}

#######################################################################
##### Effective address value accessors
#######################################################################
EA_ACCESSOR_BASE = {
  :an => "AddressRegisterDirect", :dn => "DataRegisterDirect",
  :ai => "AddressRegisterIndirect", :ap => "ARIWithPostincrement",
  :ar => "ARIWithPredecrement", :ad => "ARIWithDisplacement",
  :ax => "ARIWithIndex", :aw => "AbsoluteShort", :al => "AbsoluteLong",
  :pd => "PCIWithDisplacement", :px => "PCIWithIndex", :im => "Immediate"
}

def ea_getter(eamode)
  "get#{EA_ACCESSOR_BASE[eamode]}"
end

def eaval_getter(eamode)
  "get#{EA_ACCESSOR_BASE[eamode]}Value"
end

def eaval_setter(eamode)
  "set#{EA_ACCESSOR_BASE[eamode]}Value"
end

# for instructions that store their result in their src operand
# The problematic operands are the absolute effective addresses:
# Getting the value increases the PC, setting it would then point
# to the wrong operand address. By providing a special getter that does
# not increment the PC, this can be avoided, yet all other getters maintain
# the original efficiency
def eaval_getter_s(eamode)
  if eamode == :aw or eamode == :al or eamode == :ad or eamode == :pd then
    "#{eaval_getter(eamode)}SelfStore"
  else
    eaval_getter(eamode)
  end
end

CONST_HELPER_LINES = [
# MSB/LSB CHECK
"private boolean msbSetB(int value) { return (value & 0x80) == 0x80; }",
"private boolean msbClearB(int value) { return (value & 0x80) == 0; }",
"private boolean msbSetW(int value) { return (value & 0x8000) == 0x8000; }",
"private boolean msbClearW(int value) { return (value & 0x8000) == 0; }",
"private boolean msbSetL(int value) { return (value & 0x80000000) == 0x80000000; }",
"private boolean msbClearL(int value) { return (value & 0x80000000) == 0; }",
"private boolean lsbSet(int value) { return (value & 1) == 1; }",
"private boolean lsbClear(int value) { return (value & 1) == 0; }",

# ZERO CHECK
"private boolean isZeroB(int value) { return (value & 0xff) == 0; }",
"private boolean isZeroW(int value) { return (value & 0xffff) == 0; }",
"private boolean isZeroL(int value) { return value == 0; }",
]

#######################################################################
###### GENERATED HELPERS
###### These helpers are size-specific, i.e. they check bits
###### and zero conditions at specific word sizes
#######################################################################
SET_SUBTRACT_CC_CODE =
  "  private void setSubtractConditionCodes$SIZE(int src, int dest, " +
  "int result) {\n" +
  "    if (msbSet$SIZE(result)) setFlag(SR_N);\n" +
  "    if (isZero$SIZE(result)) setFlag(SR_Z);\n" +
  "    if (msbClear$SIZE(src) && msbSet$SIZE(dest) && msbClear$SIZE(result) " +
  "||\n" +
  "        msbSet$SIZE(src) && msbClear$SIZE(dest) && msbSet$SIZE(result)) " +
  "setFlag(SR_V);\n" +
  "    if (msbSet$SIZE(src) && msbClear$SIZE(dest) || " +
  "msbSet$SIZE(result) && msbClear$SIZE(dest) ||\n" +
  "        msbSet$SIZE(src) && msbSet$SIZE(result)) setFlag(SR_C);\n" +
  "  }\n"
SET_ADD_CC_CODE =
  "  private void setAddConditionCodes$SIZE(int src, int dest, " +
  "int result) {\n" +
  "    if (msbSet$SIZE(result)) setFlag(SR_N);\n" +
  "    if (isZero$SIZE(result)) setFlag(SR_Z);\n" +
  "    if (msbSet$SIZE(src) && msbSet$SIZE(dest) && msbClear$SIZE(result) " +
  "||\n" +
  "        msbClear$SIZE(src) && msbClear$SIZE(dest) && " +
  "msbSet$SIZE(result)) setFlag(SR_V);\n" +
  "    if (msbSet$SIZE(src) && msbSet$SIZE(dest) || msbClear$SIZE(result) " +
  "&& msbSet$SIZE(dest) ||\n" +
  "        msbSet$SIZE(src) && msbClear$SIZE(result)) setFlag(SR_C);\n" +
  "    if (isCarrySet()) setExtendedFlag();" +
  "  }\n"
SUBTRACT = 
  "  private int subtract$SIZE(int src, int dest) {\n" +
  "    int result = dest - src;\n" +
  "    setSubtractConditionCodes$SIZE(src, dest, result);\n" +
  "    return result;\n" +
  "  }\n"
ADD = 
  "  private int add$SIZE(int src, int dest) {\n" +
  "    clearFlags(SR_X, SR_N, SR_Z, SR_V, SR_C);\n" +
  "    int result = src + dest;\n" +
  "    setAddConditionCodes$SIZE(src, dest, result);\n" +
  "    return result;\n" +
  "  }\n"
NEG = 
  "  private int neg$SIZE(int dest) {\n" +
  "    clearFlags(SR_X, SR_N, SR_Z, SR_V, SR_C);\n" +
  "    int result = -dest;\n" +
  "    if (msbSet$SIZE(result)) setFlag(SR_N);\n" +
  "    if (isZero$SIZE(result)) setFlag(SR_Z);\n" +
  "    if (msbSet$SIZE(dest) && msbSet$SIZE(result)) setFlag(SR_V);\n" +
  "    if (msbSet$SIZE(dest) || msbSet$SIZE(result)) setFlag(SR_C);\n" +
  "    if (isCarrySet()) setExtendedFlag();\n" +
  "    return result;\n" +
  "  }\n"
NEGX = 
  "  private int negx$SIZE(int dest) {\n" +
  # note that Zero-flag calculation is different here:
  # clear if result is non-zero, otherwise leave unchanged
  "    clearFlags(SR_X, SR_N, SR_V, SR_C);\n" +
  "    int result = -dest - (isExtendedSet() ? 1 : 0);\n" +
  "    if (msbSet$SIZE(result)) setFlag(SR_N);\n" +
  "    if (!isZero$SIZE(result)) clearFlag(SR_Z);\n" +
  "    if (msbSet$SIZE(dest) && msbSet$SIZE(result)) setFlag(SR_V);\n" +
  "    if (msbSet$SIZE(dest) || msbSet$SIZE(result)) setFlag(SR_C);\n" +
  "    if (isCarrySet()) setExtendedFlag();\n" +
  "    return result;\n" +
  "  }\n"
NOT = 
  "  private int not$SIZE(int dest) {\n" +
  "    clearFlags(SR_N, SR_Z, SR_V, SR_C);\n" +
  "    int result = ~dest;\n" +
  "    if (msbSet$SIZE(result)) setFlag(SR_N);\n" +
  "    if (isZero$SIZE(result)) setFlag(SR_Z);\n" +
  "    return result;\n" +
  "  }\n"
OR = 
  "  private int or$SIZE(int src, int dest) {\n" +
  "    clearFlags(SR_N, SR_Z, SR_V, SR_C);\n" +
  "    int result = src | dest;\n" +
  "    if (msbSet$SIZE(result)) setFlag(SR_N);\n" +
  "    if (isZero$SIZE(result)) setFlag(SR_Z);\n" +
  "    return result;\n" +
  "  }\n"
AND = 
  "  private int and$SIZE(int src, int dest) {\n" +
  "    clearFlags(SR_N, SR_Z, SR_V, SR_C);\n" +
  "    int result = src & dest;\n" +
  "    if (msbSet$SIZE(result)) setFlag(SR_N);\n" +
  "    if (isZero$SIZE(result)) setFlag(SR_Z);\n" +
  "    return result;\n" +
  "  }\n"
EOR = 
  "  private int eor$SIZE(int src, int dest) {\n" +
  "    clearFlags(SR_N, SR_Z, SR_V, SR_C);\n" +
  "    int result = src ^ dest;\n" +
  "    if (msbSet$SIZE(result)) setFlag(SR_N);\n" +
  "    if (isZero$SIZE(result)) setFlag(SR_Z);\n" +
  "    return result;\n" +
  "  }\n"
LSR = 
  "  private int lsr$SIZE(int dest, int n) {\n" +
  "    clearFlags(SR_N, SR_Z, SR_V, SR_C);\n" +
  "    int result = dest >>> n;\n" +
  "    if (n > 0) {\n" +
  "      int mask = 1 << (n - 1);\n" +
  "      if ((dest & mask) == mask) setFlags(SR_C, SR_X);\n" +
  "      else clearFlags(SR_C, SR_X);\n" +
  "    } else clearFlag(SR_C);\n" +
  "    if (msbSet$SIZE(result)) setFlags(SR_N);\n" +
  "    if (isZero$SIZE(result)) setFlags(SR_Z);\n" +
  "    return result;\n" +
  "  }\n"
LSL =
  "  private int lsl$SIZE(int dest, int n) {\n" +
  "    clearFlags(SR_N, SR_Z, SR_V, SR_C);\n" +
  "    int result = dest << n;\n" +
  "    if (n > 0) {\n" +
  "      int mask = MSB_$SIZE >>> (n - 1);\n" +
  "      if ((dest & mask) == mask) setFlags(SR_C, SR_X);\n" +
  "      else clearFlags(SR_C, SR_X);\n" +
  "    } else clearFlag(SR_C);\n" +
  "    if (msbSet$SIZE(result)) setFlags(SR_N);\n" +
  "    if (isZero$SIZE(result)) setFlags(SR_Z);\n" +
  "    return result;\n" +
  "  }\n"
ASL =
  "  private int asl$SIZE(int dest, int n) {\n" +
  "    clearFlags(SR_N, SR_Z, SR_V, SR_C);\n" +
  "    boolean hibitSet = msbSet$SIZE(dest);\n" +
  "    boolean hasOverflow = false;\n" +
  "    int result = dest;\n" +
  "    boolean msbSet;\n" +
  "    for (int i = 0; i < n; i++) {\n" +
  "      msbSet = msbSet$SIZE(result);\n" +
  "      if (msbSet) setFlags(SR_X, SR_C);\n" +
  "      else clearFlags(SR_X, SR_C);\n" +
  "      if (hibitSet && !msbSet) hasOverflow = true;\n" +
  "      else if (!hibitSet && msbSet) hasOverflow = true;\n" +
  "      result <<= 1;\n" +
  "    }\n" +
  "    if (n == 0) clearFlag(SR_C);\n" +
  "    if (msbSet$SIZE(result)) setFlags(SR_N);\n" +
  "    if (isZero$SIZE(result)) setFlags(SR_Z);\n" +
  "    return result;\n" +
  "  }\n"
ASR =
  "  private int asr$SIZE(int dest, int n) {\n" +
  "    clearFlags(SR_N, SR_Z, SR_V, SR_C);\n" +
  "    boolean hibitSet = msbSet$SIZE(dest);\n" +
  "    boolean hasOverflow = false;\n" +
  "    int result = dest;\n" +
  "    boolean msbSet;\n" +
  "    for (int i = 0; i < n; i++) {\n" +
  "      msbSet = msbSet$SIZE(result);\n" +
  "      if (msbSet) setFlags(SR_X, SR_C);\n" +
  "      else clearFlags(SR_X, SR_C);\n" +
  "      if (hibitSet && !msbSet) hasOverflow = true;\n" +
  "      else if (!hibitSet && msbSet) hasOverflow = true;\n" +
  "      result >>= 1;\n" +
  "    }\n" +
  "    if (n == 0) clearFlag(SR_C);\n" +
  "    if (msbSet$SIZE(result)) setFlags(SR_N);\n" +
  "    if (isZero$SIZE(result)) setFlags(SR_Z);\n" +
  "    return result;\n" +
  "  }\n"

VAR_HELPERS = [
  SET_SUBTRACT_CC_CODE, SET_ADD_CC_CODE, SUBTRACT, ADD, NOT, OR, AND, NEG,
  NEGX, EOR, LSL, LSR, ASL, ASR
]

def generate_helpers
  output = ""
  indent_level = 2
  CONST_HELPER_LINES.each do |line|
    output += " " * indent_level
    output += line + "\n"
  end
  output += "\n"
  output += "  // *********************************************************\n"
  output += "  // **** CONDITION CODES\n"
  output += "  // **********************\n"

  VAR_HELPERS.each do |var_helper|
    ['L', 'W', 'B'].each do |size_char|
      output += var_helper.gsub('$SIZE', size_char)
    end
  end
  output
end

#######################################################################
##### Substitution of variables in code lines within the execute
##### body
#######################################################################
def substitute_set_effaddr_value(instr_code, size, eamode, is_ext)
  # the duplication in here is pretty ugly, we should replace this with
  # some more elegant code later
  size_char = size.to_s.upcase
  if is_ext then
    set_regexps = [ /(\SET_EFFADDR_EXT_VALUE)\((.*)\)/,
                    /(\SET_EFFADDR_EXT_VALUE_L)\((.*)\)/,
                    /(\SET_EFFADDR_EXT_VALUE_W)\((.*)\)/,
                    /(\SET_EFFADDR_EXT_VALUE_B)\((.*)\)/ ]
    set_var    = 'SET_EFFADDR_EXT_VALUE'
    set_var_l  = 'SET_EFFADDR_EXT_VALUE_L'
    set_var_w  = 'SET_EFFADDR_EXT_VALUE_W'
    set_var_b  = 'SET_EFFADDR_EXT_VALUE_B'
    get_reg    = 'getEffAddrExtRegnum'
  else
    set_regexps = [ /(\SET_EFFADDR_VALUE)\((.*)\)/, 
                    /(\SET_EFFADDR_VALUE_L)\((.*)\)/,
                    /(\SET_EFFADDR_VALUE_W)\((.*)\)/,
                    /(\SET_EFFADDR_VALUE_B)\((.*)\)/ ]
    set_var    = 'SET_EFFADDR_VALUE'
    set_var_l  = 'SET_EFFADDR_VALUE_L'
    set_var_w  = 'SET_EFFADDR_VALUE_W'
    set_var_b  = 'SET_EFFADDR_VALUE_B'
    get_reg    = 'getEffAddrRegnum'
  end
  # we currently need two subtitute actions for effective address
  # setters: the first rearranges the argument list, the second
  # sets the method name
  if EAMODE_PATTERNS[eamode] == EAMODE_SPECIAL then
    set_regexps.each do |set_regexp|
      instr_code = instr_code.gsub(set_regexp, '\1(\2)')
    end
  else
    set_regexps.each do |set_regexp|
      instr_code = instr_code.gsub(set_regexp, '\1(' + get_reg + '(), \2)')
    end
  end
  instr_code = instr_code.gsub(set_var_l,
                               "#{eaval_setter(eamode)}L")
  instr_code = instr_code.gsub(set_var_w,
                               "#{eaval_setter(eamode)}W")
  instr_code = instr_code.gsub(set_var_b,
                               "#{eaval_setter(eamode)}B")
  instr_code = instr_code.gsub(set_var,
                               "#{eaval_setter(eamode)}#{size_char}")
  instr_code
end

OPERAND_SIZE = { :b => 1, :w => 2, :l => 4 }
DATA_TYPE    = { :b => 'Byte', :w => 'Short', :l => 'Long' }
AND_MASK     = { :b => '0xff', :w => '0xffff', :l => '0xffffffff'}
def substitute_code_vars(instr_row, size, eamode, eamode_ext)
  size_char = size.to_s.upcase
  instr_code =
    instr_row.gsub("GET_EFFECTIVE_ADDRESS", "#{ea_getter(eamode)}()")
  instr_code = instr_code.gsub("GET_EFFADDR_VALUE_STORE_B",
                               "#{eaval_getter_s(eamode)}B()")
  instr_code = instr_code.gsub("GET_EFFADDR_VALUE_STORE_W",
                               "#{eaval_getter_s(eamode)}W()")
  instr_code = instr_code.gsub("GET_EFFADDR_VALUE_STORE_L",
                               "#{eaval_getter_s(eamode)}L()")
  instr_code = instr_code.gsub("GET_EFFADDR_VALUE_STORE",
                               "#{eaval_getter_s(eamode)}#{size_char}()")
  instr_code = instr_code.gsub("GET_EFFADDR_VALUE_B",
                               "#{eaval_getter(eamode)}B()")
  instr_code = instr_code.gsub("GET_EFFADDR_VALUE_W",
                               "#{eaval_getter(eamode)}W()")
  instr_code = instr_code.gsub("GET_EFFADDR_VALUE_L",
                               "#{eaval_getter(eamode)}L()")
  instr_code = instr_code.gsub("GET_EFFADDR_VALUE",
                               "#{eaval_getter(eamode)}#{size_char}()")
  instr_code = instr_code.gsub("GET_IMMEDIATE_VALUE",
                               "getImmediateValue#{size_char}()")
  instr_code = instr_code.gsub("SUBTRACT", "subtract#{size_char}")
  instr_code = instr_code.gsub("GET_ADDRESS_REGISTER",
                               "getAddressRegisterValue" +
                               "#{size_char}(getRegnum())")
  instr_code = instr_code.gsub("SET_ADDRESS_REGISTER(",
                               "setAddressRegisterDirectValue" +
                               "#{size_char}(getRegnum(), ")
  instr_code = instr_code.gsub("SET_DATA_REGISTER(",
                               "setDataRegisterDirectValue" +
                               "#{size_char}(getRegnum(), ")
  instr_code = instr_code.gsub("GET_DATA_REGISTER",
                               "getDataRegisterValue" +
                               "#{size_char}(getRegnum())")
  instr_code = instr_code.gsub("GET_DREG_EA_U", # unsigned
                               "getDataRegisterValue" +
                               "#{size_char}(getEffAddrRegnum()) &" +
                               " #{AND_MASK[size]}")
  instr_code = instr_code.gsub("GET_DREG_EA",
                               "getDataRegisterValue" +
                               "#{size_char}(getEffAddrRegnum())")
  instr_code = instr_code.gsub("SET_DREG_EA(",
                               "setDataRegisterDirectValue" +
                               "#{size_char}(getEffAddrRegnum(), ")
  instr_code = instr_code.gsub("GET_DATA", "getDataValue()")
  instr_code = instr_code.gsub("READ_MEMORY", "mem.read#{DATA_TYPE[size]}")
  instr_code = instr_code.gsub("WRITE_MEMORY", "mem.write#{DATA_TYPE[size]}")
  instr_code = substitute_set_effaddr_value(instr_code, size, eamode_ext,
                                            true)
  instr_code = substitute_set_effaddr_value(instr_code, size, eamode,
                                            false)
  # short words should be last, so they don't get confused with longer
  # substitutions
  instr_code = instr_code.gsub("OPERAND_SIZE", "#{OPERAND_SIZE[size]}")
  instr_code = instr_code.gsub("MSB_SET", "msbSet#{size_char}")
  instr_code = instr_code.gsub("IS_ZERO", "isZero#{size_char}")
  instr_code = instr_code.gsub("NEGX", "negx#{size_char}")
  instr_code = instr_code.gsub("ADD", "add#{size_char}")
  instr_code = instr_code.gsub("NOT", "not#{size_char}")
  instr_code = instr_code.gsub("ASL", "asl#{size_char}")
  instr_code = instr_code.gsub("ASR", "asr#{size_char}")
  instr_code = instr_code.gsub("LSL", "lsl#{size_char}")
  instr_code = instr_code.gsub("LSR", "lsr#{size_char}")
  instr_code = instr_code.gsub("AND", "and#{size_char}")
  instr_code = instr_code.gsub("NEG", "neg#{size_char}")
  instr_code = instr_code.gsub("EOR", "eor#{size_char}")
  instr_code = instr_code.gsub("OR", "or#{size_char}")
end

#######################################################################
##### Instructions that do not follow the regular scheme
#######################################################################
#######################################################################
##### Branches
#######################################################################

# generated code for a xcc instruction object
def make_xcc_instruction_generic(instr_template, mnemonic, prefix,
                                 condition_codes,
                                 num_cycles_body,
                                 to_string,
                                 indent_level)
  # iterate over all possible condition codes
  result = ""
  condition_codes.each do |cc|
    instr_code = instr_template.gsub('$INSTRUCTION_NAME', "#{prefix}#{cc.to_s}")
    instr_code = instr_code.gsub('$NUM_CYCLES',
                                 num_cycles_body.gsub('$CC', cc.to_s))
    code_lines = INSTR_CODE[mnemonic]
    instr_body = ""
    code_lines.each do |line|
      code_line = line.gsub("CC_CHECK", "check_#{cc.to_s}()")
      instr_body += "      #{code_line}\n"
    end
    instr_code = instr_code.gsub('$CODE', instr_body)
    instr_code = instr_code.gsub('$TO_STRING', to_string.gsub('$CC', cc.to_s))
    result += instr_code
  end
  result
end

def make_dbcc_instruction(instr_template, indent_level)
  to_str  = "      int oldpc = pc;\n"
  to_str += "      int reg = currentInstrWord & 0x07;\n"
  to_str += "      int displacement = getAbsoluteShort();\n"
  to_str += "      int label = oldpc + displacement;\n"
  to_str += "      pc = oldpc;\n"
  to_str += "      return String.format(\"db$CC\ d%d, %04x\", reg, label);"
  # branch when cc is false
  num_cycles_body = "      boolean ccTrue = check_$CC();\n"
  num_cycles_body += "      if (ccTrue) return 12;\n"
  num_cycles_body += "      int regnum = getEffAddrRegnum();\n"
  num_cycles_body += "      short tmp = (short) (dreg[regnum] & 0xffff);\n"
  num_cycles_body += "      boolean branchTaken = (tmp - 1) != -1;\n"
  num_cycles_body += "      return branchTaken ? 10 : 14;\n"
  make_xcc_instruction_generic(instr_template, :dbcc, 'db',
                               ALL_CONDITION_CODES, num_cycles_body,
                               to_str, indent_level)
end
def make_bcc_instruction(instr_template, indent_level)
  to_str  = "      int oldpc = pc;\n"
  to_str += "      int label = oldpc + getDisplacement();\n"
  to_str += "      pc = oldpc;\n"
  to_str += "      return String.format(\"b$CC\ %04x\", label);"
  # branch when cc is true
  num_cycles_body = "      boolean branchTaken = check_$CC();\n"
  num_cycles_body += "      if (branchTaken) return 10;\n"
  num_cycles_body += "      boolean isByte = (currentInstrWord & 0xff) > 0;\n"
  num_cycles_body += "      return isByte ? 8 : 12;\n"
  make_xcc_instruction_generic(instr_template, :bcc, 'b', BCC_CONDITION_CODES,
                               num_cycles_body, to_str, indent_level)
end
# Scc is another "one-off" instruction, where we reuse various helpers
# from the standard generator. Condition codes are too rare so we do
# not integrate them in the standard generator
def make_scc_instruction(instr_template, indent_level)
  # create a specification object for scc, so we can reuse make_tostring()
  scc_spec = InstructionSpec.new(:scc,:none,0x50c0,'',SCC_EAMODES,[])
  scc_spec.out_spec = [:effaddr]
  # iterate over all possible condition codes
  result = ""
  ALL_CONDITION_CODES.each do |cc|
    SCC_EAMODES.each do |eamode|
      instr_code = instr_template.gsub('$INSTRUCTION_NAME',
                                       "s#{cc.to_s}_#{eamode.to_s}")
      # toString()
      to_str = make_tostring(scc_spec, :none, eamode, nil, indent_level)
      instr_code = instr_code.gsub('$TO_STRING', to_str.gsub('cc', cc.to_s))
      # number of cycles
      if eamode == :dn then
        num_cycles_body = "return check_#{cc.to_s}() ? 6 : 4;"
      else
        num_cycles = 8 + EA_CALC_CYCLES[eamode][:bw]
        num_cycles_body = "return #{num_cycles};"
      end
      instr_code = instr_code.gsub('$NUM_CYCLES', num_cycles_body)
      # code, TODO
      code_lines = INSTR_CODE[:scc]
      instr_body = ""
      code_lines.each do |line|
        instr_body += " " * (indent_level + 4)
        instr_row = line.gsub("CC_CHECK", "check_#{cc.to_s}()")
        instr_body += substitute_code_vars(instr_row, :b, eamode, nil)
        instr_body += "\n"
      end
      instr_code = instr_code.gsub('$CODE', instr_body)
      result += instr_code
    end
  end
  result
end

# We made the moveq instruction special, but we might not actually need
# to do so. Pay attention if we will use the combination (reg, 8-bit data)
# again
def make_moveq_instruction(instr_template, indent_level)
  result = instr_template.gsub('$INSTRUCTION_NAME', 'moveq')
  result = result.gsub('$NUM_CYCLES', 'return 4;')
  # execute()
  code_body = ""
  code_lines = INSTR_CODE[:moveq]
  code_lines.each do |code_line|
    code_body += "      #{code_line}\n"
  end
  result = result.gsub('$CODE', code_body)
  # toString()
  to_str  = "      return String.format(\"moveq #\$%02x, d%d\", "
  to_str += "getSignExtended8(currentInstrWord & 0xff), getRegnum());"
  result = result.gsub('$TO_STRING', to_str)
end

def exg_reg_format(opmode)
  if opmode == :dd then
    return 'd%d', 'd%d'
  elsif opmode == :aa
    return 'a%d', 'a%d'
  else
    return 'd%d', 'a%d'
  end
end
def exg_regtypes(opmode)
  if opmode == :dd then
    return 'DataRegister', 'DataRegister'
  elsif opmode == :aa
    return 'AddressRegister', 'AddressRegister'
  else
    return 'DataRegister', 'AddressRegister'
  end
end

EXG_OPMODES = [:dd, :aa, :da]
EXG_OPMODE_NUMS = {:dd => 8, :aa => 9, :da => 17}

def make_exg_instruction(instr_template, indent_level)
  result = ""
  EXG_OPMODES.each do |opmode|
    instr = instr_template.gsub('$INSTRUCTION_NAME', "exg_#{opmode}")
    instr = instr.gsub('$NUM_CYCLES', 'return 6;')
    # execute(), the body is not retrieved from the m68k list
    # because we have the opmodes here in this function and can
    # do substitutions quicker and simpler
    regtype1, regtype2 = exg_regtypes(opmode)
    code_body  = "      int valx = get#{regtype1}Value(getRegnum());\n"
    code_body += "      int valy = get#{regtype2}Value(getEffAddrRegnum());\n"
    code_body += "      set#{regtype1}Value(getRegnum(), valy);\n"
    code_body += "      set#{regtype2}Value(getEffAddrRegnum(), valx);"
    instr = instr.gsub('$CODE', code_body)
    # toString()
    regf1, regf2 = exg_reg_format(opmode)
    to_str  = "      return String.format(\"exg #{regf1}, #{regf2}\", "
    to_str += "getRegnum(), getEffAddrRegnum());"
    instr = instr.gsub('$TO_STRING', to_str)
    result += instr
  end
  result
end

#######################################################################
##### General code generation
#######################################################################

# generate instruction name
def instr_name(spec, size, eamode, eamode_ext)
  retval = "#{spec.mnemonic}_#{size}"
  retval += "_#{eamode}"
  if eamode_ext != nil then
    retval += "_#{eamode_ext}"
  end
  retval
end

# creates the instruction code
def make_instr_code(spec, size, eamode, eamode_ext, indent_level)
  result = ""
  instr_rows = INSTR_CODE[spec.mnemonic]
  skip_row = false
  instr_rows.each do |instr_row|
    # conditional processing: since we have quite a bit of information in
    # the generator, we can do simple static checks and conditionally
    # generate only the code we need. Hopefully that is enough
    match_if_eamode = /IF_EAMODE\((.*)\)/.match(instr_row)
    match_if_size = /IF_SIZE\((.*)\)/.match(instr_row)
    if match_if_eamode != nil then
      tst_mode = match_if_eamode[1].to_sym
      if tst_mode != eamode then
        skip_row = true
      end
      next
    end
    # matching sizes
    if match_if_size != nil then
      # reset the skip_row flag on any new if, we do not support nested IF
      skip_row = false
      tst_size = match_if_size[1].to_sym
      if tst_size != size then
        skip_row = true
      end
      next
    end
    # else matches is the fallback if everything before fails
    if instr_row == 'ELSE' then
      skip_row = !skip_row
      next
    end
    if instr_row == 'ENDIF' then
      skip_row = false
      next
    end
    if skip_row then
      next
    end
    instr_code = substitute_code_vars(instr_row, size, eamode, eamode_ext)
    result += " " * indent_level
    result += "#{instr_code}\n"
  end
  result
end

# These are timing calculations that can not be determined statically
# but have to be calculated at runtime
SPECIAL_CYCLE_BODIES = {
  :asl_dxdy => {
    :bw => 'return 6 + 2 * (getDataRegisterValueL(getRegnum()) & 0x3f);',
     :l => 'return 8 + 2 * (getDataRegisterValueL(getRegnum()) & 0x3f);'
  },
  :asl_imdn => {
    :bw => 'return 6 + 2 * getDataValue();',
     :l => 'return 8 + 2 * getDataValue();'
  },
  :asr_dxdy => {
    :bw => 'return 6 + 2 * (getDataRegisterValueL(getRegnum()) & 0x3f);',
     :l => 'return 8 + 2 * (getDataRegisterValueL(getRegnum()) & 0x3f);'
  },
  :asr_imdn => {
    :bw => 'return 6 + 2 * getDataValue();',
     :l => 'return 8 + 2 * getDataValue();'
  },
  :lsl_imdn => {
    :bw => 'return 6 + 2 * getDataValue();',
     :l => 'return 8 + 2 * getDataValue();'
  },
  :lsr_imdn => {
    :bw => 'return 6 + 2 * getDataValue();',
     :l => 'return 8 + 2 * getDataValue();'
  }
}

FULL_CYCLE_BODIES = {
  :trapv => "return isOverflowSet() ? 34 : 4;",
  :divu  => "\n      int oldpc = pc;\n" +
    "      int divisor = GET_EFFADDR_VALUE;\n" +
    "      pc = oldpc;\n" +
    "      return divisor == 0 ? 38 + EA_TIME : 140;\n  "
}

def has_cycle_body(mnemonic)
  SPECIAL_CYCLE_BODIES[mnemonic] != nil ||
    FULL_CYCLE_BODIES[mnemonic] != nil
end

def get_cycle_body(mnemonic, size, eamode)
  if size == :b or size == :w then
    cycle_size = :bw
  else
    cycle_size = size
  end
  if SPECIAL_CYCLE_BODIES[mnemonic] != nil then
    SPECIAL_CYCLE_BODIES[mnemonic][cycle_size]
  else
    body = FULL_CYCLE_BODIES[mnemonic]
    if mnemonic == :divu then
      body = body.gsub("GET_EFFADDR_VALUE",
                       "#{eaval_getter(eamode)}#{size.to_s.upcase}()")
      body = body.gsub("EA_TIME",
                       "#{EA_CALC_CYCLES[eamode][cycle_size]}")
    end
    body
  end
end

# generate the body of the numCycles() method.
def num_cycles_code(mnemonic, size, eamode, eamode_ext)
  if has_cycle_body(mnemonic) then
    return get_cycle_body(mnemonic, size, eamode)
  elsif SUPERVISOR_INSTRUCTIONS.include?(mnemonic)
    # return time for privilege violation if not in supervisor mode
    num_cycles = calc_cycles(mnemonic, size, eamode, eamode_ext)
    return "return isSupervisorMode() ? #{num_cycles} : 34;"
  elsif mnemonic == :movem_ealistw
    return "int reglistMask = mem.readShort(decodePc + 2); " +
      "return #{MOVEM_MEMREG_CYCLES[:w][eamode]};"
  elsif mnemonic == :movem_ealistl
    return "int reglistMask = mem.readShort(decodePc + 2); " +
      "return #{MOVEM_MEMREG_CYCLES[:l][eamode]};"
  elsif mnemonic == :movem_listeaw
    return "int reglistMask = mem.readShort(decodePc + 2); " +
      "return #{MOVEM_REGMEM_CYCLES[:w][eamode]};"
  elsif mnemonic == :movem_listeal
    return "int reglistMask = mem.readShort(decodePc + 2); " +
      "return #{MOVEM_REGMEM_CYCLES[:l][eamode]};"
  else
    num_cycles = calc_cycles(mnemonic, size, eamode, eamode_ext)
    return "return #{num_cycles};"
  end
end

# generates code for an instruction object
def make_instruction(instr_template, spec, size, eamode, indent_level,
                     eamode_ext)
  result = instr_template.gsub('$INSTRUCTION_NAME',
                               instr_name(spec, size, eamode, eamode_ext))
  result = result.gsub('$NUM_CYCLES',
                       num_cycles_code(spec.mnemonic, size, eamode, eamode_ext))
  result = result.gsub('$CODE', make_instr_code(spec, size, eamode, eamode_ext,
                                                indent_level + 2))
  result = result.gsub('$TO_STRING', make_tostring(spec, size, eamode,
                                                   eamode_ext,
                                                   indent_level))
  result
end

#######################################################################
##### Instruction table initialization
#######################################################################

# returns the mode value according to the spec
# mmmrrr for regular ea specs
# rrrmmm for reverse ea specs
def eamode_num_value(eamode, reverse = false)
  if eamode == :undef then return 0 end # undefined
  eamode_value = EAMODE_PATTERNS[eamode]
  if eamode_value == 7 then
    eamode_value = ((eamode_value & 0xff) << 3) |
      EAMODE_SPECIAL_REGNUM[eamode]
  else
    eamode_value = ((eamode_value & 0xff) << 3) & 0x38
  end
  if reverse then
    eamode_val_old = eamode_value
    eamode_value = ((eamode_value << 3) | (eamode_value >> 3)) & 0x3f
  end
  eamode_value
end

def calc_size_num(spec, size)
  if spec.mnemonic == :move or spec.mnemonic == :movea
    MOVE_SIZE_NUM[size]
  elsif spec.size == :opmode1
    OPMODE1[size]
  elsif spec.size == :opmode2
    OPMODE2[size]
  elsif spec.size == :opmodea
    OPMODEA[size]
  elsif spec.size == :opmodea3
    OPMODEA3[size]
  else
    SIZE_NUM[size]
  end
end

# generates code to initialize the instruction table for a specific
# instruction and adressing mode
def make_instr_array(spec, size, eamode, indent_level, eamode_ext)
  func_name = instr_name(spec, size, eamode, eamode_ext)
  result = " " * indent_level
  eamode_value = eamode_num_value(eamode)
  eamode_ext_value = -1
  # only for move instructions
  if eamode_ext != nil then
    eamode_ext_value = eamode_num_value(eamode_ext, true)
  end
  size_num = calc_size_num(spec, size)
  result += "#{VARPATTERNS_TO_INIT_FUNC[spec.varpatterns]}(#{func_name}, " +
    "#{spec.base_value}, #{size_num}, #{eamode_value}, " +
    "#{eamode_ext_value});\n"
  result
end
