#!/bin/bash
if ["$#" -ne 1]; then
	echo "Usage: gen_asm_tests <test case directory>"
	exit 1
fi

find $1 -name "*_suffix.s" -printf "%f\n" | xargs -i sh -c "echo {}; dune exec -- parse_test_prog -asm $1{}"
find "out" -name "*.prog" -printf "%f\n" | xargs -i sh -c "mv out/{} $1{}"
