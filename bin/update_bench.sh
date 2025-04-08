#!/bin/bash

BENCH_DIR="../sechw-const-time-benchmarks"
ASM_DIR="asm"

set -x

cp $BENCH_DIR/compiler_pass/salsa20/bench_salsa20.s $ASM_DIR
cp $BENCH_DIR/compiler_pass/boringssl_sha512/bench_sha512_plain.s $ASM_DIR
cp $BENCH_DIR/compiler_pass/boringssl_ed25519/bench_ed25519_plain_noinline.s $ASM_DIR

dune exec gen_interface_new
