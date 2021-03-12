# Violet

Violet is a dual-issue superscalar RISC-V processor for educational purpose, written in [Clash](https://github.com/clash-lang/clash-compiler).

![Build status](https://github.com/losfair/Violet/actions/workflows/main.yaml/badge.svg)

## Features

- RV32IM
- 7-stage superscalar pipeline with in-order issue and "almost-in-order" commit
- BTB and GShare branch predictor
- Runs at 70-110 MHz on Artix-7 depending on configuration
- [3.76 CoreMark/MHz](https://github.com/losfair/Violet/runs/2093355709)
- Configurable

## Architecture

![Architecture](res/Violet.svg)

## Not yet implemented

- Interrupts
- RISC-V Privileded Architecture
- ISA Extensions other than M
- Instruction and data caches

## Verification

Violet is verified using cosimulation with [a software model](https://github.com/losfair/violet-cosim) written in Rust. Verification isn't extensive though - currently
only a small set of programs are tested. Don't tape out Violet yet :)
