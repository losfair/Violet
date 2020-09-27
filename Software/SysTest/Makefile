all:
	cargo build --release
	rust-objcopy \
		./target/riscv32i-unknown-none-elf/release/SysTest \
		--binary-architecture=riscv32 --strip-all -O binary \
		firmware.bin
	python3 ../../Scripts/im_encode.py < firmware.bin > im.txt
	python3 ../../Scripts/dm_encode.py dm < firmware.bin

.PHONY: all
