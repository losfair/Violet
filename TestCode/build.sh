#!/bin/bash

riscv64-unknown-elf-gcc -march=rv32i -mabi=ilp32 \
    -nostdlib -ffreestanding \
    -Wl,-T -Wl,./linker.ld \
    -o "$1.elf" "$1.S" || exit 1
rust-objcopy \
    "$1.elf" \
    --binary-architecture=riscv32 --strip-all -O binary \
    --only-section=.text \
    "$1.bin" || exit 1
python3 ../Scripts/memory_encode.py < "$1.bin" > "$1.txt" || exit 1
echo "OK"