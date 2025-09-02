#!/usr/bin/env python3

import subprocess
import logging
import re
from pathlib import Path
from typing import Optional

BENCHMARKS = [
    "salsa20",
    "sha512",
    "ed25519_sign",
    "chacha20",
    "poly1305",
    "x25519",
]

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
OCTAL_INFER_CHANGE = "infer_change"
OCTAL_CONVERT = "convert"
OCTAL_CHECK = "check"
OCTAL_TRANSFORM = "prog_transform"
OCTAL_STAT_ASM = "stat_asm"

ANSI_ESCAPE = re.compile(r"\x1B\[[0-?]*[ -/]*[@-~]")


class NoColorFormatter(logging.Formatter):
    def format(self, record):
        message = super().format(record)
        return ANSI_ESCAPE.sub("", message)


def setup_logger(level=logging.INFO, log_path: Optional[Path] = None):
    console_handler = logging.StreamHandler()
    console_handler.setFormatter(
        logging.Formatter("[%(levelname)s] <%(processName)s> - %(message)s")
    )
    console_handler.setLevel(logging.DEBUG)

    file_handler = None
    if log_path is not None:
        file_handler = logging.FileHandler(log_path, mode="a")
        file_handler.setFormatter(
            NoColorFormatter(
                "[%(asctime)s] [%(levelname)s] <%(processName)s> - %(message)s"
            )
        )
        file_handler.setLevel(logging.DEBUG)

    logger = logging.getLogger()
    if logger.hasHandlers():
        logger.handlers.clear()
    logger.addHandler(console_handler)
    if file_handler is not None:
        logger.addHandler(file_handler)
    logger.setLevel(level)


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
