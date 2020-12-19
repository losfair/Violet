all: build-local

build-local:
	./gen_local.sh
	./verilator_build.sh

.PHONY: build-local
