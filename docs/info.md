<!---

This file is used to generate your project datasheet. Please fill in the information below and delete any unused
sections.

You can also include images in this folder and reference them in the markdown. Each image must be less than
512 kb in size, and the combined size of all images must be less than 1 MB.
-->

## How it works

This project is an ASIC implementation of TinyJAMBU-128, an encryption cipher that was selected as a finalist in NIST's Lightweight Cryptography competition.

The design is implemented in Clash (a Haskell HDL).

### Architecture

TinyJAMBU holds a 128-bit internal state that's split into four 32-bit register ($s_0, s_1, s_2, s_3$) and uses a 128-bit key split into four 32-bit words ($k_0, k_1, k_2, k_3$).

### Pinout

| Pin | Name | Direction | Description |
| `ui_in[7:0]` | `data_in[7:0]` | Input | input data byte |
| `uo_out[7:0]` | `data_out[7:0]` | Output | output byte |
| `uio[0]` | `in_valid` | Input | set HIGH for 1 cycle when the input command is valid |
| `uio[1]` | `in_last` | Input | set HIGH when feeding the final byte of an unaligned block ($<4b$) |
| `uio[4:2]` | `cmd[2:0]` | Input | command selector (see table) |
| `uio[5]` | `is_ready` | Output | HIGH when the core is idle and ready to receive commands |
| `uio[6]` | `out_valid` | Output | HIGH when valid output data is present on `uo_out` |
| `uio[7]` | `out_last` | Output | HIGH during the final byte of an output burst |

The bidirectional pin output enable `uio_oe` is managed with `0xE0`, wich makes `uio[7:5]` outputs and `uio[4:0]` inputs.

### Commands

| Cmd | Code (`cmd[2:0]`) | Description |
| KEY | `3'd1` | stream in 16 bytes of key |
| NONCE | `3'd2` | stream in 12 bytes of nonce (3x4b) |
| AD | `3'd3` | stream in Associated Data bytes |
| ENC | `3'd4` | stream in Plaintext bytes. Ciphertext is returned by `uo_out` |
| DEC | `3'd5` | stream in Ciphertext bytes. Plaintext is returned by `uo_out` |
| FIN | `3'd6` | finalize message and create an 8-byte auth tag over `uo_out` |
| RESET | `3'd7` | reset chip state to initial state |

## How to test

Connect a microcontroller (e.g. ESP32) to the board header.

1. Reset. Set $rst_n$ to 0 for 10 cycles, set $ena$ to 1, and release reset $rst_n$ to 1.
2. Wait. Poll $uio[5]$ for HIGH.
3. Load key. Send 16 bytes with $cmd$ set to 1, and $in_valid$ set to 1. Wait for $is_ready$ HIGH.
4. Load nonce. Send 12 bytes with $cmd$ set to 2. Wait for $is_ready$ after every 4-byte chunk.
5. Send AD (optional). Send AD bytes with $cmd$ set to 3. If AD length is not divisible by 4, set $in_last$ to 1 on the last byte.
6. (Enc/Dec)rypt. Send data bytes with $cmd$ set to 4 (encrypt) or 5 (decrypt). Set $in_last$ to 1 on the last byte if it's unaligned.
7. Finalize. Send the finalize command with $cmd$ set to 6 and $in_valid$ set to 1. Read 8 bytes from $uo_out$ whenever $out_valid$ is set to get the auth tag.

## External hardware

No specialized hardware is needed, but you do need a microcontroller or some other way to control GPIO pins.

A digital logic analyzer would probably also be helpful.
