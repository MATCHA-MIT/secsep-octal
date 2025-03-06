#!/bin/bash

BENCH=""
INTER_NAME=""
OUT_DIR="out"

# usage
function usage {
    echo "Usage: $0 --bench <input assembly> [--inter <inter product name>] [--out-dir <output directory>]"
    exit 1
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --bench)
            BENCH="$2"
            shift
            shift
            ;;
        --inter)
            INTER_NAME="$2"
            shift
            shift
            ;;
        --out-dir)
            OUT_DIR="$2"
            shift
            shift
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done

if [ -z "$BENCH" ]; then
    echo "Please specify the benchmark name with --bench"
    usage
fi

if [ -z "$INTER_NAME" ]; then
    # set INTER_NAME as the base name of the benchmark, without directory and extension
    INTER_NAME="$(basename $BENCH | sed -E "s/\..*$//g")"
    echo "intermediate name not specified, using $INTER_NAME"
fi

mkdir -p $OUT_DIR

echo
echo "infering single for $BENCH"
time dune exec infer_single 1>$OUT_DIR/$INTER_NAME.single_infer.log 2>&1 -- -name $INTER_NAME -asm $BENCH
if [ $? -ne 0 ]; then
    echo
    echo "failed to infer single for $BENCH"
    exit 1
fi

echo
echo "infering range for $BENCH"
time dune exec infer_range 1>$OUT_DIR/$INTER_NAME.range_infer.log 2>&1 -- -name $INTER_NAME
if [ $? -ne 0 ]; then
    echo
    echo "failed to infer range for $BENCH"
    exit 1
fi

echo
echo "infering taint for $BENCH"
time dune exec infer_taint 1>$OUT_DIR/$INTER_NAME.taint_infer.log 2>&1 -- -name $INTER_NAME
if [ $? -ne 0 ]; then
    echo
    echo "failed to infer taint for $BENCH"
    exit 1
fi

echo
echo "instantiating taint var for $BENCH"
time dune exec instantiate_taint 1>$OUT_DIR/$INTER_NAME.instantiate.log 2>&1 -- -name $INTER_NAME
if [ $? -ne 0 ]; then
    echo
    echo "failed to instantiate taint var for $BENCH"
    exit 1
fi

echo
echo "transforming $BENCH"
time dune exec prog_transform 1>$OUT_DIR/$INTER_NAME.transform 2>&1 -- -name $INTER_NAME -out $OUT_DIR/$INTER_NAME.tf.asm
if [ $? -ne 0 ]; then
    echo
    echo "failed to transform $BENCH"
    exit 1
fi

echo
echo "result: $OUT_DIR/$INTER_NAME.tf.asm"
echo
