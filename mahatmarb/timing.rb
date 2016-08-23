# timing.rb
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

# This module contains timing information and functionality about the
# Motorola 68000

# Base cycles, ea times will be added
BASE_CYCLES = {
  :add_dnea  => { :bw =>  8, :l => 12},
  :add_eadn  => { :bw =>  4, :l =>  6},
  :adda      => { :bw =>  8, :l =>  6},
  :addi      => { :bw => 12, :l => 20},
  :addq      => { :bw =>  8, :l => 12},
  :andi      => { :bw => 12, :l => 20},
  :and_dnea  => { :bw =>  8, :l => 12},
  :and_eadn  => { :bw =>  4, :l =>  6},
  :bchg_dnea => { :none =>  8},
  :bchg_imea => { :none => 12},
  :bclr_imea => { :none => 12},
  :bset_dnea => { :none =>  8},
  :bset_imea => { :none => 12},
  :btst_dnea => { :none => 4},
  :btst_imea => { :none => 8},
  :clr       => { :bw =>  8, :l => 12},
  :cmp       => { :bw =>  4, :l =>  6},
  :cmpa      => { :bw =>  6, :l =>  6},
  :cmpi      => { :bw =>  8, :l => 12},
  :eor_dnea  => { :bw =>  8, :l => 12},
  :eori      => { :bw => 12, :l => 20},
  :move_to_ccr  => { :bw => 12},
  :move_from_sr => { :bw =>  8},
  :move_to_sr   => { :bw => 12},
  :muls      => { :bw => 70},  # simplified for now
  :mulu      => { :bw => 70},  # simplified for now
  :neg       => { :bw =>  8, :l => 12},
  :negx      => { :bw =>  8, :l => 12},
  :not       => { :bw =>  8, :l => 12},
  :or_dnea   => { :bw =>  8, :l => 12},
  :or_eadn   => { :bw =>  4, :l =>  6},
  :ori       => { :bw => 12, :l => 20},
  :suba      => { :bw =>  8, :l =>  6},
  :sub_dnea  => { :bw =>  8, :l => 12},
  :sub_eadn  => { :bw =>  4, :l =>  6},
  :subi      => { :bw => 12, :l => 20},
  :subq      => { :bw =>  8, :l => 12},
  :tas       => { :bw =>  14},
  :tst       => { :bw =>  4, :l =>  4},
}

EA_CALC_CYCLES = {
  :an => {:bw =>  0, :l =>  0, :none =>  0},
  :dn => {:bw =>  0, :l =>  0, :none =>  0},
  :ai => {:bw =>  4, :l =>  8, :none =>  8},
  :ap => {:bw =>  4, :l =>  8, :none =>  8},
  :ar => {:bw =>  6, :l => 10, :none => 10},
  :ad => {:bw =>  8, :l => 12, :none => 12},
  :ax => {:bw => 10, :l => 14, :none => 14},
  :aw => {:bw =>  8, :l => 12, :none => 12},
  :al => {:bw => 12, :l => 16, :none => 16},
  :pd => {:bw =>  8, :l => 12, :none => 12},
  :px => {:bw => 10, :l => 14, :none => 14},
  :im => {:bw =>  4, :l =>  8, :none =>  8}
}

# some instructions (e.g. add) have a different base time depending
# on the addressing mode and size. If there is an override found here,
# use this instead
BASE_OVERRIDE_CYCLES = {
  :adda      => { :l => { :an => 8, :dn => 8, :im => 8}},
  :add_eadn  => { :l => { :an => 8, :dn => 8, :im => 8}},
  :and_eadn  => { :l => { :an => 8, :im => 8}},
  :suba      => { :l => { :an => 8, :dn => 8, :im => 8}},
  :sub_eadn  => { :l => { :an => 8, :dn => 8, :im => 8}},
}

######################################################################
###### FULL TIMING INFORMATION
###### Instructions that can not be calculated by 
###### The general format is that there is a mapping from the mnemonic to
###### an array of size 2 (.b.w/.l) in order to map cycles for different data
###### sizes
FULL_CYCLES = {
  :addi      => { :bw => {:dn => 8}, :l => {:dn => 16}},
  :addq      => { :bw => {:dn => 4, :an => 4}, :l => {:dn => 8, :an => 8}},
  :andi      => { :bw => {:dn => 8}, :l => {:dn => 14}},
  :bchg_imea => { :none => {:dn => 12}},
  :bclr_imea => { :none => {:dn => 14}},
  :bset_imea => { :none => {:dn => 12}},
  :btst_dnea => { :none => {:dn => 6}},
  :btst_imea => { :none => {:dn => 10}},
  :clr       => { :bw => {:dn => 4}, :l => {:dn =>  6}},
  :cmpi      => { :bw => {:dn => 8}, :l => {:dn => 14}},
  :cmpm      => { :bw => {:undef => 12}, :l => {:undef => 20}},
  :eor       => { :bw => {:dn => 4},     :l => {:dn => 8}},
  :eori      => { :bw => {:dn => 8}, :l => {:dn => 16}},
  :jmp => {
    :none => {:ai => 8,:ad => 10,:ax => 14,:aw => 10,:al => 12,:pd => 10,
              :px => 14}},
  :jsr => {
    :none => {:ai => 16,:ad => 18,:ax => 22,:aw => 18,:al => 20,:pd => 18,
              :px => 22}},
  :lea => { :l => { :ai => 4, :ad => 8, :ax => 12, :aw => 8, :al => 12,
                    :pd => 8, :px => 12 }},
  :move_to_ccr  => {:bw => {:dn => 12}},
  :move_from_sr => {:bw => {:dn =>  6}},
  :move_to_sr   => {:bw => {:dn => 12}},
  :movea => {
    :bw => {:dn => 4,:an => 4,:ai => 8,:ap => 8,:ar => 10,:ad => 12,
            :ax => 14,:aw => 12,:al => 16,:pd => 12, :px => 14,:im => 8},
    :l => {:dn => 4,:an => 4,:ai => 12,:ap => 12,:ar => 14,:ad => 16,
           :ax => 18,:aw => 16,:al => 20,:pd => 16,:px => 18,:im => 12}
  },
  :neg       => { :bw => {:dn => 4}, :l => {:dn => 6}},
  :negx      => { :bw => {:dn => 4}, :l => {:dn => 6}},
  :not       => { :bw => {:dn => 4}, :l => {:dn => 6}},
  :ori       => { :bw => {:dn => 8}, :l => {:dn => 16}},
  :pea => { :l => { :ai => 12, :ad => 16, :ax => 20, :aw => 16, :al => 20,
                    :pd => 16, :px => 20 }},
  :subi      => { :bw => {:dn => 8}, :l => {:dn => 16}},
  :subq      => { :bw => {:dn => 4, :an => 8}, :l => {:dn => 8, :an => 8}},
  :tas       => { :bw => {:dn => 4}},
  :tst       => { :bw => {:dn => 4, :an => 4}, :l => {:dn => 4, :an => 4}},
}

# for instructions that only have one timing
ONE_CYCLES = {
  :andi_to_ccr => 20, :andi_to_sr => 20,
  :bra => 10, :bsr => 18,
  :eori_to_ccr => 20, :eori_to_sr => 20, :ext => 4,
  :link => 16,
  :move_from_usp => 4, :move_to_usp => 4,
  :nop => 4,
  :ori_to_ccr => 20, :ori_to_sr => 20,
  :reset => 132, :rte => 20, :rtr => 20, :rts => 16,
  :swap => 4, :trap => 34,
  :unlk => 12,
}

# cycle table for move.<size> <ea>, <ea>
# Table is kept separate since it is the only one which is that
# comprehensive
MOVE_CYCLES = {
  :bw => {
    :dn => {:dn => 4,:an => 4,:ai => 8,:ap => 8,:ar => 8,:ad => 12,
            :ax => 12, :aw => 12, :al => 16},
    :an => {:dn => 4,:an => 4,:ai => 8,:ap => 8,:ar => 8,:ad => 12,
            :ax => 12, :aw => 12, :al => 16},
    :ai => {:dn => 8,:an => 8,:ai => 12,:ap => 12,:ar => 12,:ad => 16,
            :ax => 18, :aw => 16, :al => 20},
    :ap => {:dn => 8,:an => 8,:ai => 12,:ap => 12,:ar => 12,:ad => 16,
            :ax => 18, :aw => 16, :al => 20},
    :ar => {:dn => 10,:an => 10,:ai => 14,:ap => 14,:ar => 14,:ad => 18,
            :ax => 20, :aw => 18, :al => 22},
    :ad => {:dn => 12,:an => 12,:ai => 16,:ap => 16,:ar => 16,:ad => 20,
            :ax => 22, :aw => 20, :al => 24},
    :ax => {:dn => 14,:an => 14,:ai => 18,:ap => 18,:ar => 18,:ad => 22,
            :ax => 24, :aw => 22, :al => 26},
    :aw => {:dn => 12,:an => 12,:ai => 16,:ap => 16,:ar => 16,:ad => 20,
            :ax => 22, :aw => 20, :al => 24},
    :al => {:dn => 16,:an => 16,:ai => 20,:ap => 20,:ar => 20,:ad => 24,
            :ax => 26, :aw => 24, :al => 28},
    :pd => {:dn => 12,:an => 12,:ai => 16,:ap => 16,:ar => 16,:ad => 20,
            :ax => 22, :aw => 20, :al => 24},
    :px => {:dn => 14,:an => 14,:ai => 18,:ap => 18,:ar => 18,:ad => 22,
            :ax => 24, :aw => 22, :al => 26},
    :im => {:dn => 8,:an => 8,:ai => 12,:ap => 12,:ar => 12,:ad => 16,
            :ax => 18, :aw => 16, :al => 20}
  },
  :l => {
    :dn => {:dn => 4,:an => 4,:ai => 12,:ap => 12,:ar => 14,:ad => 16,
            :ax => 18, :aw => 16, :al => 20},
    :an => {:dn => 4,:an => 4,:ai => 12,:ap => 12,:ar => 14,:ad => 16,
            :ax => 18, :aw => 16, :al => 20},
    :ai => {:dn => 12,:an => 12,:ai => 20,:ap => 20,:ar => 20,:ad => 24,
            :ax => 26, :aw => 24, :al => 28},
    :ap => {:dn => 12,:an => 12,:ai => 20,:ap => 20,:ar => 20,:ad => 24,
            :ax => 26, :aw => 24, :al => 28},
    :ar => {:dn => 14,:an => 14,:ai => 22,:ap => 22,:ar => 22,:ad => 26,
            :ax => 28, :aw => 26, :al => 30},
    :ad => {:dn => 16,:an => 16,:ai => 24,:ap => 24,:ar => 24,:ad => 28,
            :ax => 30, :aw => 28, :al => 32},
    :ax => {:dn => 18,:an => 18,:ai => 26,:ap => 26,:ar => 26,:ad => 30,
            :ax => 32, :aw => 30, :al => 34},
    :aw => {:dn => 16,:an => 16,:ai => 24,:ap => 24,:ar => 24,:ad => 28,
            :ax => 30, :aw => 28, :al => 32},
    :al => {:dn => 20,:an => 20,:ai => 28,:ap => 28,:ar => 28,:ad => 32,
            :ax => 34, :aw => 32, :al => 36},
    :pd => {:dn => 16,:an => 16,:ai => 24,:ap => 24,:ar => 24,:ad => 28,
            :ax => 30, :aw => 28, :al => 32},
    :px => {:dn => 18,:an => 18,:ai => 26,:ap => 26,:ar => 26,:ad => 30,
            :ax => 32, :aw => 30, :al => 34},
    :im => {:dn => 12,:an => 12,:ai => 20,:ap => 20,:ar => 20,:ad => 24,
            :ax => 26, :aw => 24, :al => 28}
  }
}

# movem has a dynamically calculated cycle times which depend on the
# number of registers specified in the lists. We need to explicitly
# write the expression so that the code generator can replace it
MOVEM_MEMREG_CYCLES = {
  :w => {
    :ai => "12 + 4 * numBitsSet(reglistMask)",
    :ap => "12 + 4 * numBitsSet(reglistMask)",
    :ad => "16 + 4 * numBitsSet(reglistMask)",
    :ax => "18 + 4 * numBitsSet(reglistMask)",
    :aw => "16 + 4 * numBitsSet(reglistMask)",
    :al => "20 + 4 * numBitsSet(reglistMask)",
    :pd => "16 + 4 * numBitsSet(reglistMask)",
    :px => "18 + 4 * numBitsSet(reglistMask)",
  },
  :l => {
    :ai => "12 + 8 * numBitsSet(reglistMask)",
    :ap => "12 + 8 * numBitsSet(reglistMask)",
    :ad => "16 + 8 * numBitsSet(reglistMask)",
    :ax => "18 + 8 * numBitsSet(reglistMask)",
    :aw => "16 + 8 * numBitsSet(reglistMask)",
    :al => "20 + 8 * numBitsSet(reglistMask)",
    :pd => "16 + 8 * numBitsSet(reglistMask)",
    :px => "18 + 8 * numBitsSet(reglistMask)",
  }
}
MOVEM_REGMEM_CYCLES = {
  :w => {
    :ai => "8 + 4 * numBitsSet(reglistMask)",
    :ar => "8 + 4 * numBitsSet(reglistMask)",
    :ad => "12 + 4 * numBitsSet(reglistMask)",
    :ax => "14 + 4 * numBitsSet(reglistMask)",
    :aw => "12 + 4 * numBitsSet(reglistMask)",
    :al => "16 + 4 * numBitsSet(reglistMask)",
  },
  :l => {
    :ai => "8 + 8 * numBitsSet(reglistMask)",
    :ar => "8 + 8 * numBitsSet(reglistMask)",
    :ad => "12 + 8 * numBitsSet(reglistMask)",
    :ax => "14 + 8 * numBitsSet(reglistMask)",
    :aw => "12 + 8 * numBitsSet(reglistMask)",
    :al => "16 + 8 * numBitsSet(reglistMask)",
  }
}

######################################################################
### Helpers
######################################################################

def has_base_cycles(mnemonic, cycle_size)
  BASE_CYCLES[mnemonic] != nil and
    BASE_CYCLES[mnemonic][cycle_size] != nil
end

def has_base_overrides(mnemonic, cycle_size, eamode)
  BASE_OVERRIDE_CYCLES[mnemonic] != nil and
    BASE_OVERRIDE_CYCLES[mnemonic][cycle_size] != nil and
    BASE_OVERRIDE_CYCLES[mnemonic][cycle_size][eamode] != nil
end

def has_full_timing_spec(mnemonic, cycle_size, eamode)
  FULL_CYCLES[mnemonic] != nil and
    FULL_CYCLES[mnemonic][cycle_size] != nil and
    FULL_CYCLES[mnemonic][cycle_size][eamode] != nil
end

def add_eamode_cycles(base_cycles, mnemonic, cycle_size, eamode)
  if EA_CALC_CYCLES[eamode] != nil and
    EA_CALC_CYCLES[eamode][cycle_size] != nil then
    ea_cycles = EA_CALC_CYCLES[eamode][cycle_size]
  else
    puts "WARNING: Could not calc. eacycles for " +
         "#{mnemonic} #{eamode} (#{cycle_size})"
    ea_cycles = 0
  end
  base_cycles + ea_cycles
end

######################################################################
### Service function
### Calculates the time for the specified instruction and
### address mode(s)
### In general, the function tries to first find the instruction
### in the FULL_CYCLES and MOVE_CYCLES tables. If not, the
### calculation is based on the BASE_CYCLES table, adding
### the times for calculating the effective address
######################################################################

# Calculates the number of cycles required by an instruction
def calc_cycles(mnemonic, size, eamode, eamode_ext)
  # cycle size can be :bw, :l, or :none
  if size == :b or size == :w then
    cycle_size = :bw
  else
    cycle_size = size
  end
  num_cycles = 0 # default
  if mnemonic == :move then
    # move cycles are special
    num_cycles = MOVE_CYCLES[cycle_size][eamode][eamode_ext]
  elsif ONE_CYCLES[mnemonic] != nil
    num_cycles = ONE_CYCLES[mnemonic]
  elsif has_full_timing_spec(mnemonic, cycle_size, eamode)
    num_cycles = FULL_CYCLES[mnemonic][cycle_size][eamode]
  elsif has_base_overrides(mnemonic, cycle_size, eamode)
    num_cycles =
      add_eamode_cycles(BASE_OVERRIDE_CYCLES[mnemonic][cycle_size][eamode],
                        mnemonic, cycle_size, eamode)
  elsif has_base_cycles(mnemonic, cycle_size)
    num_cycles = add_eamode_cycles(BASE_CYCLES[mnemonic][cycle_size],
                                   mnemonic, cycle_size, eamode)
  else
    puts "WARNING: Could not calculate # cycles for #{mnemonic} (#{cycle_size})"
  end
  num_cycles
end
