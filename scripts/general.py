#!/usr/bin/env python3

import subprocess
import logging
from pathlib import Path

SECSEP_DIR = Path(__file__).parent.parent.parent
OCTAL_DIR = SECSEP_DIR / "octal"
OCTAL_WORK_DIR = OCTAL_DIR / "out"
BENCHMARK_DIR = SECSEP_DIR / "benchmark" / "analysis"
GEM5_DIR = SECSEP_DIR / "gem5"
GEM5_DOCKER_BENCH_DIR = "/root/benchmark"

OCTAL_PPC = "preprocess_input"
OCTAL_INFER_SINGLE = "infer_single"
OCTAL_INFER_RANGE = "infer_range"
OCTAL_INFER_TAINT = "infer_taint"
OCTAL_CONVERT = "convert"
OCTAL_CHECK = "check"
OCTAL_TRANSFORM = "prog_transform"
OCTAL_STAT_ASM = "stat_asm"


def build_octal():
    subprocess.run(
        ["dune", "build"],
        cwd=OCTAL_DIR,
        check=True,
    )
    subprocess.run(
        ["dune", "install"],
        cwd=OCTAL_DIR,
        check=True,
    )
