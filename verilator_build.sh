#!/bin/sh
  
verilator \
    +1364-2001ext+v \
    -Wno-fatal -O3 --threads 1 \
    --cc $(cat simulate_verilator.list) \
    --exe verilator_main.cpp
make -C obj_dir -f Vtop.mk CXXFLAGS=-O2
