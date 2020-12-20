all: build-local

build-local:
	./gen_local.sh
	./verilator_build.sh

build-local-iverilog:
	./gen_local.sh
	iverilog -c ./simulate.list -o iverilog-sim.elf

.PHONY: build-local
