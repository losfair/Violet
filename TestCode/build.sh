#!/bin/bash

IN_SOURCE="$1"
OUT_DIR="$2"

mkdir "$OUT_DIR"
rm "$OUT_DIR/{image.elf,image.bin,dm.*.txt,im.txt}"

riscv64-unknown-elf-gcc -march=rv32im -mabi=ilp32 \
    -nostdlib -ffreestanding -O2 \
    -Wl,-T -Wl,./linker.ld \
    -o "$OUT_DIR/image.elf" "$IN_SOURCE" || exit 1
riscv64-unknown-elf-gcc -march=rv32im -mabi=ilp32 \
    -nostdlib -ffreestanding -O2 \
    -Wl,-T -Wl,./linker.ld \
    -S -o "$OUT_DIR/image.S" "$IN_SOURCE" || exit 1

rust-objcopy \
    "$OUT_DIR/image.elf" \
    --binary-architecture=riscv32 --strip-all -O binary \
    --only-section=.text \
    "$OUT_DIR/image.bin" || exit 1
rust-objdump --arch-name=riscv32 -d "$OUT_DIR/image.elf" > "$OUT_DIR/image.dump" || exit 1
python3 ../Scripts/im_encode.py < "$OUT_DIR/image.bin" > "$OUT_DIR/im.txt" || exit 1
python3 ../Scripts/dm_encode.py "$OUT_DIR/dm" < "$OUT_DIR/image.bin" || exit 1
echo "OK"