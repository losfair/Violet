all:

build-core:
	cd src && stack exec -- clash --verilog Violet.Gen.CoreGen

build-verilator: build-core
	./verilator_build.sh

.PHONY: build-core build-verilator
