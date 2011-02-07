# disassemble.rb
# Created on October 15, 2009
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

# A module that generates code to disassemble an instruction

def make_tostring(spec, size, eamode, eamode_ext, indent_level)
  to_string = ""
  to_string += " " * indent_level
  # save the program counter in order to reset it before leaving
  to_string += "  int oldpc = pc;\n"

  # special rule: in movem instructions, there is a 16 bit word directly
  # after the initial instruction word, which we need to read in before
  # we read the normal operands in order to have the correct PC value
  if spec.mnemonic.to_s.match(/movem_.*/) then
    # reads the register list mask
    to_string += "      int reglist = mem.readShort(pc); pc += 2;\n"
  end

  # generate the parameter definitions that become the format string
  # arguments
  if spec.out_spec.length > 0
    to_string += make_tostring_param(spec.mnemonic, size, spec.out_spec[0],
                                     eamode, eamode_ext,
                                     'val1', indent_level)
  end
  if spec.out_spec.length > 1
    to_string += make_tostring_param(spec.mnemonic, size, spec.out_spec[1],
                                     eamode, eamode_ext, 'val2', indent_level)
  end

  # build the output string
  if spec.out_spec.length == 0 then
    to_string += "      String str = \"#{spec.mnemonic}\";"
  else
    to_string += make_tostring_formatstring(size, spec, eamode, eamode_ext,
                                            indent_level)
  end
  # restore program counter and exit toString(). In a disassembler,
  # we might just keep the PC as it is
  to_string += " " * indent_level
  to_string += "  pc = oldpc;\n"
  to_string += " " * indent_level
  to_string += "  return str;"
  to_string
end

# Size prefix overrides come in handy to improve output. Sometimes
# sizes and opmodes do not map to an appropriate size string
def overrides_size_suffix?(mnemonic)
  mnemonic == :ext or mnemonic == :lea
end

def size_suffix_override(mnemonic, size)
  if mnemonic == :lea then
    ''
  elsif mnemonic == :ext
    if size == :b then '.w' else '.l' end
  end
end

# builds the code for printing the formatted, disassembled instruction
def make_tostring_formatstring(size, spec, eamode, eamode_ext,
                               indent_level)
  result = ""
  result += " " * indent_level
  # cut off special format name extensions in instructions such as add etc.
  # we also do not print lengths that are specified as ':none'
  mnemonic = spec.mnemonic.to_s.gsub(/_.*/, '')
  if overrides_size_suffix?(spec.mnemonic)
    format = "#{mnemonic}#{size_suffix_override(spec.mnemonic, size)}"
  elsif spec.size == :none then
    format = "#{mnemonic}"
  else
    format = "#{mnemonic}.#{size}"
  end
  result += "  String str = String.format(\"#{format} "
  result += format_str_for_arg(spec.mnemonic, spec.out_spec[0], eamode,
                               eamode_ext)
  if spec.out_spec.length > 1
    result += ", "
    result += format_str_for_arg(spec.mnemonic, spec.out_spec[1], eamode,
                                 eamode_ext)
  end
  # the argument list behind the format string
  result += "\""
  result += make_tostring_add_val(spec.out_spec[0], eamode, eamode_ext, 1)
  if spec.out_spec.length > 1
    result += make_tostring_add_val(spec.out_spec[1], eamode, eamode_ext, 2)
  end
  result += ");\n"
  result
end

# build the value for an instruction operand with possible extension
# values (e.g. displacement and indexed addressing modes), which are
# indicated with an _<number>
def make_tostring_add_val(out_spec, eamode, eamode_ext, pos)
  result = ", val#{pos}"
  if (out_spec == :effaddr and (eamode == :ad or eamode == :px)) or
      (out_spec == :effaddr_rev and eamode_ext == :ad) then
    result += ", val#{pos}_1"
  elsif (out_spec == :effaddr and eamode == :ax) or
      (out_spec == :effaddr_rev and eamode_ext == :ax) then
    result += ", val#{pos}_1, val#{pos}_2"
  end
  result
end

# builds the code to setup parameter values for the instruction's
# address mode and size
def make_tostring_param(mnemonic, size, out_spec, eamode, eamode_ext,
                        valname, indent_level)
  result = ""
  # if address register indirect + displacement, we have an addtional value
  if out_spec == :effaddr and eamode == :ad then
    result += make_displacement_params(result, valname, false, indent_level)
  elsif out_spec == :effaddr_rev and eamode_ext == :ad
    result += make_displacement_params(result, valname, true, indent_level)
  elsif out_spec == :effaddr and (eamode == :ax or eamode == :px)
    result += make_index_params(result, valname, eamode, false, indent_level)
  elsif out_spec == :effaddr_rev and eamode_ext == :ax
    result += make_index_params(result, valname, eamode_ext, true, indent_level)
  else
    result += " " * indent_level
    result += "  String #{valname} = #{strval_for_arg(mnemonic, size, out_spec, eamode, eamode_ext)};\n"
  end
  result
end

# makes the parameters for address register indirect with index
def make_index_params(result, valname, eamode, is_ext, indent_level)
  ext = ""
  if is_ext then ext = "Ext" end
  result += " " * indent_level
  result += "  int extword#{ext} = mem.readShort(pc); pc += 2;\n"
  result += " " * indent_level
  result += "  int displacement8#{ext} = getSignExtended8(extword#{ext}&0xff);\n"
  result += " " * indent_level
  result += "  int regnum#{ext} = (extword#{ext} >> 12) & 0x07;\n"
  result += " " * indent_level
  result += "  boolean isAddrReg#{ext} = (extword#{ext} & 0x8000) == 0x8000;\n"
  result += " " * indent_level
  result += "  boolean isLong#{ext} = (extword#{ext} & 0x800) == 0x800;\n"
  result += " " * indent_level
  result += "  String lstr#{ext} = isLong#{ext} ? \"l\" : \"w\";\n"
  result += " " * indent_level
  result += "  String reg#{ext} = isAddrReg#{ext} ? \"a\" : \"d\";\n"
  result += " " * indent_level
  result += "  String #{valname} = formatSigned(displacement8#{ext});\n"
  if eamode == :ax then
    result += " " * indent_level
    result += "  String #{valname}_1 = String.format(\"%d\", " +
      "getEffAddr#{ext}Regnum());\n"
    result += " " * indent_level
    result += "  String #{valname}_2 = String.format(\"%s%d.%s\", " +
      "reg#{ext}, regnum#{ext}, lstr#{ext});\n"
  elsif eamode == :px
    result += " " * indent_level
    result += "  String #{valname}_1 = String.format(\"%s%d.%s\", " +
      "reg#{ext}, regnum#{ext}, lstr#{ext});\n"
  end
end

def make_displacement_params(result, valname, is_ext, indent_level)
  ext = ""
  if is_ext then ext = "Ext" end
  result += " " * indent_level
  result += "  String #{valname} = formatSigned(getSignExtended16(" +
    "mem.readShort(pc))); pc += 2;\n"
  result += " " * indent_level
  result += "  String #{valname}_1 = String.format(\"%d\", getEffAddr#{ext}Regnum());\n"
  result
end

EAMODE_FORMAT_STRING = {:dn => 'd%s', :an => 'a%s', :al => '%s.l',
  :aw => '%s.w', :im => '%s', :pd => '%s(PC)', :ai => '(a%s)',
  :ad => '%s(a%s)', :ap => '(a%s)+', :ar => '-(a%s)',
  :ax => '%s(a%s, $%s)', :px => '%s(PC, $%s)'
}

def format_str_for_arg(mnemonic, arg, eamode, eamode_ext)
  if mnemonic == :cmpm then
    return "(a%s)+" # cmpm has different output
  elsif arg == :aregnum or arg == :effaddr_aregnum
    return "a%s"
  elsif arg == :dregnum or arg == :effaddr_dregnum
    return "d%s"
  elsif arg == :data or arg == :im_data or arg == :displacement or
      arg == :trap_vector
    return '#$%s'
  elsif arg == :reglist or arg == :usp or arg == :sr or arg == :ccr
    return '%s'
  elsif arg == :effaddr
    return format_str_for_effaddr(eamode)
  else
    return format_str_for_effaddr(eamode_ext)
  end
end

def format_str_for_effaddr(eamode)
  retval = EAMODE_FORMAT_STRING[eamode]
  if retval == nil then retval = eamode.to_s end
  retval
end

def strval_for_arg(mnemonic, size, arg, eamode, eamode_ext)
  if arg == :aregnum or arg == :dregnum then
    return "String.valueOf(getRegnum())"
  elsif arg == :effaddr_dregnum or arg == :effaddr_aregnum
    return "String.valueOf(getEffAddrRegnum())"
  elsif arg == :displacement
    return "String.format(\"%02x\", getDisplacement())"
  elsif arg == :data and (mnemonic == :asr_imdn or mnemonic == :asl_imdn or
                          mnemonic == :lsr_imdn or mnemonic == :lsl_imdn or
                          mnemonic == :addq or mnemonic == :subq)
    return "String.format(\"%02x\", getDataValue() == 0 ? 8 : getDataValue())"
  elsif arg == :data
    return "String.format(\"%02x\", getDataValue())"
  elsif arg == :trap_vector
    return "String.format(\"%02x\", getTrapVector())"
  elsif arg == :im_data and (size == :b or size == :none)
    return "String.format(\"%02x\", mem.readByte(pc + 1)); pc += 2"
  elsif arg == :im_data and size == :w
    return "String.format(\"%02x\", mem.readShort(pc)); pc += 2"
  elsif arg == :im_data and size == :l
    return "String.format(\"%02x\", mem.readLong(pc)); pc += 4"
  elsif arg == :reglist and eamode != :ar
    return "reglistMaskToString(reglist);"
  elsif arg == :reglist and eamode == :ar
    return "reglistMaskRevToString(reglist);"
  elsif arg == :effaddr
    return strval_for_effaddr(size, eamode, false)
  elsif arg == :usp or arg == :sr or arg == :ccr
    return "\"#{arg.to_s}\""
  else # :effaddr_rev
    return strval_for_effaddr(size, eamode_ext, true)
  end
end

def strval_for_effaddr(size, eamode, rev)
  if eamode == :aw then
    return "String.format(\"#\$%02x\", (short) mem.readShort(pc)); pc += 2"
  elsif eamode == :al then
    return "String.format(\"#\$%02x\", mem.readLong(pc)); pc += 4"
  elsif eamode == :im and size == :b then
    return "String.format(\"#\$%02x\", (byte) mem.readByte(pc + 1)); pc += 2"
  elsif eamode == :im and size == :w then
    return "String.format(\"#\$%02x\", (short) mem.readShort(pc)); pc += 2"
  elsif eamode == :im and size == :l then
    return "String.format(\"#\$%02x\", mem.readLong(pc)); pc += 4"
  elsif eamode == :im
    puts "WARNING: im addressing mode, but size is :none (assume bw) !!!"
    return "String.format(\"#\$%02x\", mem.readShort(pc)); pc += 2"
  elsif eamode == :pd
    return "String.format(\"#\$%02x\", (short) mem.readShort(pc)); pc += 2"
  else
    if rev then
      effaddr_regnum = 'getEffAddrExtRegnum()'
    else
      effaddr_regnum = 'getEffAddrRegnum()'
    end
    return "String.format(\"%d\", #{effaddr_regnum})"
  end
  return "\"?\""
end
