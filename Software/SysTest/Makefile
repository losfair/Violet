all:
	xargo build --release --target violet
	rust-objcopy \
		./target/violet/release/SysTest \
		--binary-architecture=riscv32 --strip-all -O binary \
		firmware.bin
	python3 ../../Scripts/im_encode.py < firmware.bin > im.txt
	python3 ../../Scripts/dm_encode.py dm < firmware.bin

dump:
	rust-objdump --arch-name riscv32 --mattr +m -d ./target/violet/release/SysTest > SysTest.dump

.PHONY: all dump
