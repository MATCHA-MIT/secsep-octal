#!/usr/bin/env python3

from enum import Enum
import argparse
import subprocess
from pathlib import Path
import time
from colorama import Fore

OUT_DIR = "out"


class InferPhase(Enum):
    SingleTypeInfer = 0
    RangeTypeInfer = 1
    TaintTypeInfer = 2
    TaintInstantiation = 3
    NumPhases = 4


InferPhaseDescription = """Run inference on the benchmark:
    0: Single Type Infer
    1: Range Type Infer
    2: Taint Type Infer
    3: Taint Instantiation
"""


class CheckPhase(Enum):
    Conversion = 0
    Check = 1
    NumPhases = 2


CheckPhaseDescription = """Run checker on the infer result:
    0: Conversion
    1: Check
"""


def run(cmd, log_file, msg):
    print(msg, end=" ", flush=True)
    with open(log_file, "w") as f:
        start_time = time.time()
        result = subprocess.run(cmd, stdout=f, stderr=f)
        if result.returncode != 0:
            print(f"({Fore.RED}errcode: {result.returncode}{Fore.RESET})")
            print(f"{Fore.YELLOW}log saved to {log_file}{Fore.RESET}", flush=True)
            exit(1)
        print(f"({round(time.time() - start_time, 2)} seconds)", flush=True)


def infer(benchmark, phase_beg, phase_end):
    name = Path(benchmark).stem

    for i in range(phase_beg, phase_end + 1):
        if i == InferPhase.SingleTypeInfer.value:
            run(
                [
                    "dune",
                    "exec",
                    "infer_single",
                    "--",
                    "-name",
                    name,
                    "-asm",
                    benchmark,
                ],
                f"{OUT_DIR}/{name}.single_infer.log",
                "Running Single Type Infer",
            )
        elif i == InferPhase.RangeTypeInfer.value:
            run(
                ["dune", "exec", "infer_range", "--", "-name", name],
                f"{OUT_DIR}/{name}.range_infer.log",
                "Running Range Type Infer",
            )
        elif i == InferPhase.TaintTypeInfer.value:
            run(
                ["dune", "exec", "infer_taint", "--", "-name", name],
                f"{OUT_DIR}/{name}.taint_infer.log",
                "Running Taint Type Infer",
            )
        elif i == InferPhase.TaintInstantiation.value:
            run(
                ["dune", "exec", "instantiate_taint", "--", "-name", name],
                f"{OUT_DIR}/{name}.instantiate.log",
                "Running Taint Instantiation",
            )
        else:
            print("Invalid phase", i)
            exit(1)


def check(benchmark, phase_beg, phase_end):
    name = Path(benchmark).stem

    for i in range(phase_beg, phase_end + 1):
        if i == CheckPhase.Conversion.value:
            run(
                ["dune", "exec", "convert", "--", "-name", name],
                f"{OUT_DIR}/{name}.conversion.log",
                "Running Conversion",
            )
        elif i == CheckPhase.Check.value:
            run(
                ["dune", "exec", "check", "--", "-name", name],
                f"{OUT_DIR}/{name}.check.log",
                "Running Check",
            )
        else:
            print("Invalid phase", i)
            exit(1)


def transform(benchmark, out=None):
    name = Path(benchmark).stem
    if out is None:
        out = f"{OUT_DIR}/{name}.tf.asm"

    run(
        ["dune", "exec", "prog_transform", "--", "-name", name, "-out", out],
        f"{OUT_DIR}/{name}.transform.log",
        "Transforming program",
    )


def main():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command")
    subparsers.required = True

    parser_infer = subparsers.add_parser(
        "infer",
        description=InferPhaseDescription,
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser_infer.add_argument("benchmark", type=str, help="benchmark to infer")
    parser_infer.add_argument(
        "phase_beg",
        nargs="?",
        type=int,
        help="first phase to run",
        default=0,
    )
    parser_infer.add_argument(
        "phase_end",
        nargs="?",
        type=int,
        help="last phase to run",
        default=InferPhase.NumPhases.value - 1,
    )

    parser_check = subparsers.add_parser(
        "check",
        description=CheckPhaseDescription,
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser_check.add_argument("benchmark", type=str, help="benchmark to check")
    parser_check.add_argument(
        "phase_beg",
        nargs="?",
        type=int,
        help="first phase to run",
        default=0,
    )
    parser_check.add_argument(
        "phase_end",
        nargs="?",
        type=int,
        help="last phase to run",
        default=CheckPhase.NumPhases.value - 1,
    )

    parser_tf = subparsers.add_parser("transform", description="transform program")
    parser_tf.add_argument("benchmark", type=str, help="benchmark to transform")
    parser_tf.add_argument(
        "--out", type=str, help="output path of the transformed program", required=False
    )

    parser_all = subparsers.add_parser("all", description="Run all phases")
    parser_all.add_argument("benchmark", type=str, help="benchmark to run")
    parser_all.add_argument(
        "--out", type=str, help="output path of the transformed program", required=False
    )

    args = parser.parse_args()

    if args.command == "all":
        infer(args.benchmark, 0, InferPhase.NumPhases.value - 1)
        check(args.benchmark, 0, CheckPhase.NumPhases.value - 1)
        transform(args.benchmark, args.out)
    elif args.command == "infer":
        infer(args.benchmark, args.phase_beg, args.phase_end)
    elif args.command == "check":
        check(args.benchmark, args.phase_beg, args.phase_end)
    elif args.command == "transform":
        transform(args.benchmark, args.out)
    else:
        print("Invalid command", args.command)
        exit(1)


if __name__ == "__main__":
    main()
