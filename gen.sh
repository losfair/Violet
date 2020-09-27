#!/bin/bash

CLASH_DIR="/home/zhy/Projects/clash-compiler"
PROJECT_DIR="$(pwd)"

cd "$CLASH_DIR" || exit 1
stack run --cwd "$PROJECT_DIR" -- clash --verilog Violet.Gen.CoreGen
