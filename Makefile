all:

build-core:
	cd src && stack exec -- clash --verilog Violet.Gen.CoreGen

build-verilator: build-core
	./verilator_build.sh

build-iverilog: build-core
	iverilog -c ./simulate.list -o iverilog-sim.elf

.PHONY: build-core build-verilator
