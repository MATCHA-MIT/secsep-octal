#!/bin/bash

OUT_DIR="out"
BENCH_DIR="../sechw-const-time-benchmarks"

set -x

cp $OUT_DIR/bench_salsa20.tf.asm $BENCH_DIR/src/salsa20/bench_salsa20_tf.s
cp $OUT_DIR/bench_sha512_plain.tf.asm $BENCH_DIR/src/boringssl_sha512/bench_sha512_plain_tf.s
cp $OUT_DIR/bench_ed25519_plain_noinline.tf.asm $BENCH_DIR/src/boringssl_ed25519/bench_ed25519_plain_noinline_tf.s
