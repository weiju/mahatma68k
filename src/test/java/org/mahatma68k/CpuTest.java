/**
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

import java.nio.ByteBuffer;
import org.junit.Test;
import org.junit.Before;
import static org.junit.Assert.*;

public class CpuTest {

  private static final int MEMSIZE = 32;
  private ByteBufferMemory memory;
  private Cpu cpu = new Cpu();

  private AddressSpace createMemory(byte[] buffer) {
    ByteBuffer byteBuffer = ByteBuffer.allocateDirect(MEMSIZE);
    byteBuffer.put(buffer);
    memory = new ByteBufferMemory(byteBuffer, 0, MEMSIZE);
    return memory;
  }

  private void assertFlagsSet(int sr, int flags) {
    assertTrue((sr & flags) == flags);
  }

  private void assertFlagsCleared(int sr, int flags) {
    assertTrue((sr & flags) == 0);
  }

  @Before
  public void setUp() {
    cpu.setPC(0);
  }

  // **********************************************************************
  // **** MOVEA
  // **********************************************************************
	@Test public void testMovea4a6() { // movea.l #4.w, a6
      byte[] buffer = { (byte) 0x2c, (byte) 0x78, (byte) 0x00, (byte) 0x04, 0x01, 0x02, 0x03, 0x04 };
    cpu.setAddressSpace(createMemory(buffer));
    Cpu.Instruction movea;
    movea = cpu.nextInstruction();
    movea.execute();
    assertEquals(0x01020304, cpu.getAddressRegisterValue(6));
  }

  // **********************************************************************
  // **** SUBQ
  // **********************************************************************
	@Test public void testSubqL1d0() { // subq.l #1, d0
    byte[] buffer = { (byte) 0x53, (byte) 0x80};
    cpu.setAddressSpace(createMemory(buffer));
    Cpu.Instruction subq;

    // Test case 1: d0 = 1
    cpu.setDataRegisterValue(0, 1);
    subq = cpu.nextInstruction();
    subq.execute();
    assertEquals(0, cpu.getDataRegisterValue(0));
    assertFlagsSet(cpu.getSR(), Cpu.SR_Z);

    // Test case 2: d0 = 0, N, C, X set
    cpu.setPC(0);
    subq = cpu.nextInstruction();
    subq.execute();
    assertEquals(-1, cpu.getDataRegisterValue(0));
    assertFlagsCleared(cpu.getSR(), Cpu.SR_Z);
    assertFlagsSet(cpu.getSR(), Cpu.SR_N | Cpu.SR_C | Cpu.SR_X);
  }

  @Test public void testSubqW1d0() { // subq.w #1, d0
    byte[] buffer = { (byte) 0x53, (byte) 0x40};
    cpu.setAddressSpace(createMemory(buffer));
    Cpu.Instruction subq_w;

    // Test case 1: d0 = 0x8000, V set
    cpu.setDataRegisterValue(0, 0x8000);
    subq_w = cpu.nextInstruction();
    subq_w.execute();
    assertEquals(0x7fff, (cpu.getDataRegisterValue(0) & 0xffff));
    int sr = cpu.getSR();
    assertFlagsSet(cpu.getSR(), Cpu.SR_V);
	}

  @Test public void testSubqB1d0() { // subq.b #1, d0
    byte[] buffer = { (byte) 0x53, (byte) 0x00};
    cpu.setAddressSpace(createMemory(buffer));
    Cpu.Instruction subq_b;

    // Test case 1: d0 = 0x80, V set
    cpu.setDataRegisterValue(0, 0x80);
    subq_b = cpu.nextInstruction();
    subq_b.execute();
    assertEquals(0x7f, (cpu.getDataRegisterValue(0) & 0xff));
    assertFlagsSet(cpu.getSR(), Cpu.SR_V);
	}

	// sub.b d0, d1
  @Test public void testSub_eadn_b_d0_d1() {
    byte[] buffer = { (byte) 0x92, (byte) 0x00};
    cpu.setAddressSpace(createMemory(buffer));
    Cpu.Instruction sub_eadn_b;

    // Test case 1: d1 = 0x1234fffe, N set
    cpu.setDataRegisterValue(0, 0x43210101);
    cpu.setDataRegisterValue(1, 0x1234ffff);
    sub_eadn_b = cpu.nextInstruction();
    sub_eadn_b.execute();
    assertEquals(0x1234fffe, cpu.getDataRegisterValue(1));
    assertFlagsSet(cpu.getSR(), Cpu.SR_N);
  }
  // **********************************************************************
  // ***** ADD
  // ***********************
  @Test public void testAdd_l_a3_d2() { // add.l a3, d2
    byte[] buffer = { (byte) 0xd4, (byte) 0x8b};
    cpu.setAddressSpace(createMemory(buffer));
    Cpu.Instruction add_l_a3_d2;

    // Test case 1: a3 = 42, d2 = 3
    cpu.setAddressRegisterValue(3, 42);
    cpu.setDataRegisterValue(2, 3);
    add_l_a3_d2 = cpu.nextInstruction();
    add_l_a3_d2.execute();
    assertEquals(45, cpu.getDataRegisterValue(2));
  }

  // **********************************************************************
  // **** LSx
  // **********************************************************************
  @Test public void testLsr_b_d1() { // lsr.b #4, d1
    byte[] buffer = { (byte) 0xe8, (byte) 0x09 };
    cpu.setAddressSpace(createMemory(buffer));
    Cpu.Instruction lsr_b_d1;
    cpu.setDataRegisterValue(1, 0xff);
    lsr_b_d1 = cpu.nextInstruction();
    lsr_b_d1.execute();
    assertEquals(0x0f, cpu.getDataRegisterValue(1));
    assertFlagsSet(cpu.getSR(), Cpu.SR_X | Cpu.SR_C);
    assertFlagsCleared(cpu.getSR(), Cpu.SR_Z | Cpu.SR_N | Cpu.SR_V);
  }

  @Test public void testLsr_w_d1() { // lsr.w #4, d1
    byte[] buffer = { (byte) 0xe8, (byte) 0x49 };
    cpu.setAddressSpace(createMemory(buffer));
    Cpu.Instruction lsr_w_d1;
    cpu.setDataRegisterValue(1, 0xffff);
    lsr_w_d1 = cpu.nextInstruction();
    lsr_w_d1.execute();
    assertEquals(0x0fff, cpu.getDataRegisterValue(1));
    assertFlagsSet(cpu.getSR(), Cpu.SR_X | Cpu.SR_C);
    assertFlagsCleared(cpu.getSR(), Cpu.SR_Z | Cpu.SR_N | Cpu.SR_V);
  }

  @Test public void testLsl_b_d1() { // lsl.b #4, d1
    byte[] buffer = { (byte) 0xe9, (byte) 0x09 };
    cpu.setAddressSpace(createMemory(buffer));
    Cpu.Instruction lsl_b_d1;
    cpu.setDataRegisterValue(1, 0xff);
    lsl_b_d1 = cpu.nextInstruction();
    lsl_b_d1.execute();
    assertEquals(0x000000f0, cpu.getDataRegisterValue(1));
    assertFlagsSet(cpu.getSR(), Cpu.SR_X | Cpu.SR_C | Cpu.SR_N);
    assertFlagsCleared(cpu.getSR(), Cpu.SR_Z | Cpu.SR_V);
  }

  @Test public void testLsl_w_d1() { // lsl.w #4, d1
    byte[] buffer = { (byte) 0xe9, (byte) 0x49 };
    cpu.setAddressSpace(createMemory(buffer));
    Cpu.Instruction lsl_w_d1;
    cpu.setDataRegisterValue(1, 0xffff);
    lsl_w_d1 = cpu.nextInstruction();
    lsl_w_d1.execute();
    assertEquals(0xfff0, cpu.getDataRegisterValue(1));
    assertFlagsSet(cpu.getSR(), Cpu.SR_X | Cpu.SR_C | Cpu.SR_N);
    assertFlagsCleared(cpu.getSR(), Cpu.SR_Z | Cpu.SR_V);
  }
}
