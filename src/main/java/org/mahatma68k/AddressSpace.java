/**
 * Created on September 24, 2009
 * Copyright (c) 2009-2011, Wei-ju Wu
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
 * This is the AddressSpace interface. Systems that use the Mahatma68k
 * CPU must implement this interface to provide the address space of the
 * environment.
 *
 * The following conventions are used:
 *
 * <ul>
 *   <li>int is the data type consistently used throughout for byte, word and long
 *   and the value should always be regarded as signed</li>
 *   <li>reading a value shorter than long from a register results in the value
 *   being sign-extended</li>
 * </ul>
 * @author Wei-ju Wu
 * @version 1.0
 */
public interface AddressSpace {
  /**
   * Returns the start address of this AddressSpace instance.
   * @return the start address
   */
  int start();

  /**
   * Returns the size of this AddressSpace instance.
   * @return size
   */
  int size();

  /**
   * Reads the byte at the specified address. The result is a sign-extended
   * int value.
   * @param address memory address
   * @return the byte value at the specified address
   */
	int readByte(int address);

  /**
   * Reads the short word at the specified address. The result is a sign-extended
   * int value.
   * @param address memory address
   * @return the short value at the specified address
   */
	int readShort(int address);

  /**
   * Reads the long word at the specified address. The result is a signed 32 bit
   * value.
   * @param address memory address
   * @return the long value the specified address
   */
	int readLong(int address);

  /**
   * Writes the byte at the specified address. The value should be in the range
   * [Byte.MIN_VALUE, Byte.MAX_VALUE].
   * @param address memory address
   * @param value byte value
   */
  void writeByte(int address, int value);

  /**
   * Writes the short word at the specified address. The value should be in the
   * range [Short.MIN_VALUE, Short.MAX_VALUE].
   * @param address memory address
   * @param value short value
   */
  void writeShort(int address, int value);

  /**
   * Writes the long word at the specified address. The value should be in the
   * range [Integer.MIN_VALUE, Integer.MAX_VALUE]
   */
  void writeLong(int address, int value);
}
