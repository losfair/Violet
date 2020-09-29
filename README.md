# Violet

Dual-issue superscalar RISC-V processor written in [Clash](https://github.com/clash-lang/clash-compiler).

Work in progress and not fully verified yet. Do not use for production.

## Features

- RV32IM
- 8-stage in-order superscalar pipeline
- BTB + BHT for branch prediction
- 110 MHz on Artix-7
- 2.68 CoreMark/MHz

## Architecture

![Architecture](res/Violet.svg)
