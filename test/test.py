# SPDX-FileCopyrightText: © 2024 Tiny Tapeout
# SPDX-License-Identifier: Apache-2.0

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import ClockCycles


MASK32 = 0xFFFF_FFFF

def state_update(s, k, steps):
    for i in range(steps):
        w0, w1, w2, w3 = s
        t1 = ((w1 >> 15) | ((w2 << 17) & MASK32)) & MASK32
        t2 = ((w2 >> 6) | ((w3 << 26) & MASK32)) & MASK32
        t3 = ((w2 >> 21) | ((w3 << 11) & MASK32)) & MASK32
        t4 = ((w2 >> 27) | ((w3 << 5) & MASK32)) & MASK32
        fb = (w0 ^ t1 ^ (~(t2 & t3) & MASK32) ^ t4 ^ k[i % 4]) & MASK32

        s[0], s[1], s[2], s[3] = w1, w2, w3, fb

def tj128_encrypt_ref(key, nonce, ad, pt):
    k = [int.from_bytes(key[4 * i : 4 * i + 4], "little") for i in range(4)]
    s = [0, 0, 0, 0]

    # key setup
    state_update(s, k, 32)

    # nonce setup
    for i in range(3):
        s[1] ^= 0x10
        state_update(s, k, 20)
        n_w = int.from_bytes(nonce[4 * i : 4 * i + 4], "little")
        s[3] ^= n_w

    # associated data
    ad_len = len(ad)
    ad_index = 0

    while ad_len >= 4:
        s[1] ^= 0x30
        state_update(s, k, 20)
        w = int.from_bytes(ad[ad_index : ad_index + 4], "little")
        s[3] ^= w
        ad_index += 4
        ad_len -= 4

    if ad_len > 0:
        s[1] ^= 0x30
        state_update(s, k, 20)

        rem = ad[ad_index:] + bytes(4 - ad_len)
        s[3] ^= int.from_bytes(rem, "little")
        s[1] ^= ad_len

    # plaintext enc
    ct = bytearray()
    pt_len = len(pt)
    pt_index = 0
    while pt_len >= 4:
        s[1] ^= 0x50
        state_update(s, k, 32)
        ks = s[2].to_bytes(4, "little")

        p_block = pt[pt_index : pt_index + 4]
        c_block = bytes(p ^ k_b for p, k_b in zip(p_block, ks))

        ct.extend(c_block)
        s[3] ^= int.from_bytes(p_block, "little")

        pt_index += 4
        pt_len -= 4

    if pt_len > 0:
        s[1] ^= 0x50
        state_update(s, k, 32)
        ks = s[2].to_bytes(4, "little")

        p_block = pt[pt_index:]
        c_block = bytes(p ^ k_b for p, k_b in zip(p_block, ks[:pt_len]))
        ct.extend(c_block)

        rem = p_block + bytes(4 - pt_len)
        s[3] ^= int.from_bytes(rem, "little")
        s[1] ^= pt_len

    s[1] ^= 0x70
    state_update(s, k, 32)
    tag0 = s[2].to_bytes(4, "little")

    s[1] ^= 0x70
    state_update(s, k, 20)
    tag1 = s[2].to_bytes(4, "little")

    return bytes(ct), tag0 + tag1

CMD_KEY = 1
CMD_NONCE = 2
CMD_AD = 3
CMD_ENC = 4
CMD_DEC = 5
CMD_FIN = 6

async def wait_ready(dut):
    while True:
        await ClockCycles(dut.clk, 1)
        if int(dut.uio_out.value) & (1 << 5):
            break

async def send_byte(dut, cmd, val, is_last=False):
    await wait_ready(dut)

    ctrl = (cmd << 2) | ((1 if is_last else 0) << 1) | 1
    dut.ui_in.value = val
    dut.uio_in.value = ctrl

    await ClockCycles(dut.clk, 1)
    dut.uio_in.value = 0

async def read_output(dut, length):
    res = bytearray()
    while len(res) < length:
        await ClockCycles(dut.clk, 1)
        val = int(dut.uio_out.value)
        if val & (1 << 6):
            res.append(int(dut.uo_out.value))

    return bytes(res)

async def hw_load_key(dut, key):
    for b in key:
        await send_byte(dut, CMD_KEY, b)

    await wait_ready(dut)

async def hw_load_nonce(dut, nonce):
    for b in nonce:
        await send_byte(dut, CMD_NONCE, b)

    await wait_ready(dut)

async def hw_proc_ad(dut, ad):
    for i, b in enumerate(ad):
        is_last = (i == len(ad) - 1) and (len(ad) % 4 != 0)
        await send_byte(dut, CMD_AD, b, is_last=is_last)

    await wait_ready(dut)

async def hw_encrypt(dut, pt):
    ct = bytearray()
    for i, b in enumerate(pt):
        is_last = (i == len(pt) - 1) and (len(pt) % 4 != 0)
        await send_byte(dut, CMD_ENC, b, is_last=is_last)

        if (i % 4 == 3) or is_last:
            block_len = (i % 4) + 1 if is_last else 4
            ct_chunk = await read_output(dut, block_len)
            ct.extend(ct_chunk)

    await wait_ready(dut)
    return bytes(ct)

async def hw_decrypt(dut, ct):
    pt = bytearray()
    for i, b in enumerate(ct):
        is_last = (i == len(ct) - 1) and (len(ct) % 4 != 0)
        await send_byte(dut, CMD_DEC, b, is_last=is_last)

        if (i % 4 == 3) or is_last:
            block_len = (i % 4) + 1 if is_last else 4
            pt_chunk = await read_output(dut, block_len)
            pt.extend(pt_chunk)

    await wait_ready(dut)
    return bytes(pt)

async def hw_finalize(dut):
    await wait_ready(dut)

    ctrl = (CMD_FIN << 2) | 1
    dut.ui_in.value = 0
    dut.uio_in.value = ctrl

    await ClockCycles(dut.clk, 1)

    dut.uio_in.value = 0
    tag = await read_output(dut, 8)

    await wait_ready(dut)
    return tag

@cocotb.test()
async def test_chip(dut):
    dut._log.info("Testing crypt")
    clock = Clock(dut.clk, 10, unit="us")
    cocotb.start_soon(clock.start())

    dut.ena.value = 1
    dut.ui_in.value = 0
    dut.uio_in.value = 0
    dut.rst_n.value = 0

    await ClockCycles(dut.clk, 10)
    dut.rst_n.value = 1
    await ClockCycles(dut.clk, 2)

    # test 1
    key = bytes(range(16))
    nonce = bytes(range(12))
    ad = bytes(range(4))
    pt = bytes(range(8))

    ref_ct, ref_tag = tj128_encrypt_ref(key, nonce, ad, pt)

    dut._log.info("test 1, encrypt")
    await hw_load_key(dut, key)
    await hw_load_nonce(dut, nonce)
    await hw_proc_ad(dut, ad)

    hw_ct = await hw_encrypt(dut, pt)
    hw_tag = await hw_finalize(dut)

    assert hw_ct == ref_ct, f"ciphertext doesn't match. expected {ref_ct.hex()}, yet got {hw_ct.hex()}"
    assert hw_tag == ref_tag, f"encrypt tag doesn't match. expected {ref_tag.hex()}, yet got {hw_tag.hex()}"
    dut._log.info("test 1 pass")

    # test 2
    dut._log.info("test 2, decrypt")
    await hw_load_key(dut, key)
    await hw_load_nonce(dut, nonce)
    await hw_proc_ad(dut, ad)

    hw_pt = await hw_decrypt(dut, hw_ct)
    hw_dec_tag = await hw_finalize(dut)

    assert hw_pt == pt, f"plaintext doesn't match. expected {pt.hex()}, yet got {hw_pt.hex()}"
    assert hw_dec_tag == ref_tag, f"decrypt tag doesn't match. expected {ref_tag.hex()}, yet got {hw_dec_tag.hex()}"
    dut._log.info("test 2 pass.")

    # test 3
    ad_part = bytes([0xAA, 0xBB, 0xCC])
    pt_part = bytes([0x10, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70])

    ref_ct2, ref_tag2 = tj128_encrypt_ref(key, nonce, ad_part, pt_part)

    dut._log.info("test 3, partial block encrypt")
    await hw_load_key(dut, key)
    await hw_load_nonce(dut, nonce)
    await hw_proc_ad(dut, ad_part)

    hw_ct2 = await hw_encrypt(dut, pt_part)
    hw_tag2 = await hw_finalize(dut)

    assert hw_ct2 == ref_ct2, f"partial ct doesn't match. expected {ref_ct2.hex()} yet got {hw_ct2.hex()}"
    assert hw_tag2 == ref_tag2, f"partial tag doesn't match. expected {ref_tag2.hex()} yet got {hw_tag2.hex()}"
    dut._log.info("test 3 pass.")

    # test 4
    dut._log.info("test 4, partial block decrypt")
    await hw_load_key(dut, key)
    await hw_load_nonce(dut, nonce)
    await hw_proc_ad(dut, ad_part)

    hw_pt2 = await hw_decrypt(dut, hw_ct2)
    hw_dec_tag2 = await hw_finalize(dut)

    assert hw_pt2 == pt_part, f"partial plaintext doesn't match. expected {pt_part}, yet got {hw_pt2}"
    assert hw_dec_tag2 == ref_tag2, f"partial tag doesn't match. expected {ref_tag2}, yet got {hw_dec_tag2}"

    dut._log.info("all tests pass.")
