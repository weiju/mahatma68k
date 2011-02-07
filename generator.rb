#!/usr/bin/ruby
# generator.rb
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
# Description:
# ------------
# This is a code generator that generates the Java CPU code from an
# input file that defines a 68000 CPU. I follow here the tradition of
# CPU emulators like Musashi or that found in UAE.
# The advantage of the 68000 instruction set is that instructions can be
# entirely identified by the first 16-bit word.
# The code generator primarily builds two main tables of the size 2^16:
# The code table and the timing table.
require 'mahatmarb/m68k'
require 'mahatmarb/timing'
require 'mahatmarb/codegen'
require 'mahatmarb/disassemble'

######################################################################
#### DEFINITIONS
######################################################################
TEMPLATE    = "templates/CpuTemplate.java"
INSTRUCTION_TEMPLATE = "templates/InstructionTemplate.java"
SLIM_INSTRUCTION_TEMPLATE = "templates/InstructionTemplateSlim.java"
OUTPUT_FILE = "src/main/java/org/mahatma68k/Cpu.java"

#######################################################################
##### Table initialization for instructions
#######################################################################
class InstructionTableInitializer
  def initialize
    @init_instr_table = "private void initInstructionTable() {\n"
  end
  def process(instr_spec, size, eamode, indent, eamode_ext)
    @init_instr_table += make_instr_array(instr_spec, size, eamode,
                                          indent, eamode_ext)
  end
  def process_special_instructions
    # Bcc instructions, need to iterate over 8bit displacements and
    # condition codes
    BCC_CONDITION_CODES.each do |cc|
      @init_instr_table += " " * 4
      @init_instr_table += "setInstrToTableForDisplacement(b#{cc.to_s}," +
        " 0x6000 | (#{COND_CODE_NUM[cc]} << 8), -1, -1, -1);\n"
#      @init_instr_table += "setBccToTable(b#{cc.to_s}, #{COND_CODE_NUM[cc]});\n"
    end
    ALL_CONDITION_CODES.each do |cc|
      @init_instr_table += " " * 4
      @init_instr_table += "setDbccToTable(db#{cc.to_s}, #{COND_CODE_NUM[cc]});\n"
    end
    ALL_CONDITION_CODES.each do |cc|
      SCC_EAMODES.each do |eamode|
        eamode_value = eamode_num_value(eamode)
        @init_instr_table += " " * 4
        @init_instr_table += "setSccToTable(s#{cc.to_s}_#{eamode.to_s}, " +
          "#{COND_CODE_NUM[cc]}, #{eamode_value});\n"
      end
    end
    @init_instr_table += " " * 4
    @init_instr_table += "setMoveqToTable(moveq);\n"
    EXG_OPMODES.each do |opmode|
      @init_instr_table += " " * 4
      @init_instr_table += "setExgToTable(exg_#{opmode}, " +
        "#{EXG_OPMODE_NUMS[opmode]});\n"
    end
  end
  def to_s
    @init_instr_table += "  }\n"
    @init_instr_table
  end
end

#######################################################################
##### Creator for instructions
#######################################################################
class InstructionCreator
  def initialize(instr_template)
    @instr_objects = ""
    @instr_template = instr_template
  end
  def process(instr_spec, size, eamode, indent, eamode_ext)
    @instr_objects += make_instruction(@instr_template, instr_spec, size,
                                       eamode, indent, eamode_ext)
  end
  def process_special_instructions
    # generate branch objects
    @instr_objects += make_bcc_instruction(@instr_template, 2)
    @instr_objects += make_dbcc_instruction(@instr_template, 2)
    @instr_objects += make_moveq_instruction(@instr_template, 2)
    @instr_objects += make_exg_instruction(@instr_template, 2)
    @instr_objects += make_scc_instruction(@instr_template, 2)
  end
  def to_s
    @instr_objects
  end
end

# Generic processing loop for iterating over the general instructions
# Used by instruction object creation and table initialization
def process_instr_specs(instr_specs, processor)
  instr_specs.each do |instr_spec|
    if instr_spec.eamodes.length > 0 && instr_spec.size.instance_of?(Array) then
      # regular case: both eamodes and sizes specified
      # iterator over the sizes
      instr_spec.size.each do |size|
        instr_spec.eamodes.each do |eamode|
          # eamode_ext
          if instr_spec.eamodes_ext != nil then
            instr_spec.eamodes_ext.each do |eamode_ext|
              processor.process(instr_spec, size, eamode, 4, eamode_ext)
            end
          else
              processor.process(instr_spec, size, eamode, 4, nil)
          end
        end
      end
    elsif instr_spec.eamodes.length == 0 &&
        instr_spec.size.instance_of?(Array)
      # special case: no eamodes, but sizes defined
      instr_spec.size.each do |size|
        processor.process(instr_spec, size, :undef, 4, nil)
      end
    elsif instr_spec.size == :opmodea3 # opmodea with 3 sizes, no eamode
      [:b, :w, :l].each do |opmode|
        processor.process(instr_spec, opmode, :undef, 4, nil)
      end
    elsif instr_spec.size == :opmodea
      # opmodea defined, no eamodes
      if instr_spec.eamodes.length == 0 then
        [:w, :l].each do |opmode|
          processor.process(instr_spec, opmode, :undef, 4, nil)
        end
      # opmodea defined, has eamodes
      else
        [:w, :l].each do |opmode|
          instr_spec.eamodes.each do |eamode|
            processor.process(instr_spec, opmode, eamode, 4, nil)
          end
        end
      end
    elsif instr_spec.size == :opmode1 or instr_spec.size == :opmode2
      # exception: opmode1/opmode2 defined
      [:b, :w, :l].each do |opmode|
        instr_spec.eamodes.each do |eamode|
          processor.process(instr_spec, opmode, eamode, 4, nil)
        end
      end
    elsif instr_spec.eamodes.length > 0
      # size is pre-defined, eamodes are defined
      instr_spec.eamodes.each do |eamode|
        processor.process(instr_spec, instr_spec.size,
                          eamode, 4, nil)
      end
    elsif instr_spec.eamodes.length == 0
      # no eamodes
      processor.process(instr_spec, instr_spec.size,
                        :undef, 4, nil)
    end
  end
  processor.process_special_instructions
end

# generate code for instruction table initialization
# the simplest might be to map effective address modes/sizes to separate
# functions, but different registers only go to different slots
# unsupported address modes are not filled into slots
def init_instruction_table(instr_specs)
  table_init = InstructionTableInitializer.new
  process_instr_specs(instr_specs, table_init)
  table_init.to_s
end

# generate code for all instruction objects
def create_instruction_objects(instr_specs, instr_template)
  instr_creator = InstructionCreator.new(instr_template)
  process_instr_specs(instr_specs, instr_creator)
  instr_creator.to_s
end

def read_template(template_filename)
	template = ""
	File.open(template_filename, "r") do |file|
		file.each do |line|
			template += line
		end
	end
	template
end

def generate_code(slim)
	template = read_template(TEMPLATE)
	if slim then
    instr_template = read_template(SLIM_INSTRUCTION_TEMPLATE)
	else
    instr_template = read_template(INSTRUCTION_TEMPLATE)
	end
  instr_specs = create_instr_specs
  instr_objects = create_instruction_objects(instr_specs, instr_template)
  init_instr_table = init_instruction_table(instr_specs)
  template.gsub!('$HELPERS', generate_helpers)
  template.gsub!('$INSTRUCTION_OBJECTS', instr_objects)
  template.gsub!('$INIT_INSTRUCTION_TABLE', init_instr_table)
  File.open(OUTPUT_FILE, "w") do |file|
    file.puts("#{template}")
  end
end

######################################################################
#### MAIN
######################################################################
slim = false
if ARGV.length > 0 and ARGV[0] == '-slim' then
	slim = true
end

code = generate_code(slim)

