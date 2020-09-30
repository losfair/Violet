# Violet

Dual-issue superscalar RISC-V processor written in [Clash](https://github.com/clash-lang/clash-compiler).

## Features

- RV32IM
- 8-stage in-order superscalar pipeline
- BTB + BHT for branch prediction
- 110 MHz on Artix-7
- 2.9 CoreMark/MHz

## Architecture

![Architecture](res/Violet.svg)

## Verification

Violet is verified using cosimulation with a software model written in Rust. Verification isn't extensive though - currently
only a small set of programs are tested. Don't tape out Violet yet :)
