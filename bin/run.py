from enum import Enum
import argparse
import subprocess
from pathlib import Path
import time

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
            print(f"(Error: {result.returncode})", flush=True)
            exit(1)
        print(f"({round(time.time() - start_time, 2)} seconds)", flush=True)


def infer(args):
    if args.name is None:
        name = Path(args.benchmark).stem
    else:
        name = args.name
    
    for i in range(args.phase_beg, args.phase_end + 1):
        if i == InferPhase.SingleTypeInfer.value:
            run(
                ["dune", "exec", "infer_single", "--", "-name", name, "-asm", args.benchmark],
                f"{OUT_DIR}/{name}.single_infer.log",
                "Running Single Type Infer"
            )
        elif i == InferPhase.RangeTypeInfer.value:
            run(
                ["dune", "exec", "infer_range", "--", "-name", name],
                f"{OUT_DIR}/{name}.range_infer.log",
                "Running Range Type Infer"
            )
        elif i == InferPhase.TaintTypeInfer.value:
            run(
                ["dune", "exec", "infer_taint", "--", "-name", name],
                f"{OUT_DIR}/{name}.taint_infer.log",
                "Running Taint Type Infer"
            )
        elif i == InferPhase.TaintInstantiation.value:
            run(
                ["dune", "exec", "instantiate_taint", "--", "-name", name],
                f"{OUT_DIR}/{name}.instantiate.log",
                "Running Taint Instantiation"
            )
        else:
            print("Invalid phase", i)
            exit(1)


def check(args):
    if args.name is None:
        name = Path(args.benchmark).stem
    else:
        name = args.name
    
    for i in range(args.phase_beg, args.phase_end + 1):
        if i == CheckPhase.Conversion.value:
            run(
                ["dune", "exec", "convert", "--", "-name", name],
                f"{OUT_DIR}/{name}.conversion.log",
                "Running Conversion"
            )
        elif i == CheckPhase.Check.value:
            run(
                ["dune", "exec", "check", "--", "-name", name],
                f"{OUT_DIR}/{name}.check.log",
                "Running Check"
            )
        else:
            print("Invalid phase", i)
            exit(1)


def transform(args):
    if args.out is None:
        out = f"{OUT_DIR}/{args.name}.tf.asm"
    else:
        out = args.out
    run(
        ["dune", "exec", "prog_transform", "--", "-name", args.name, "-out", out],
        f"{OUT_DIR}/{args.name}.transform.log",
        "Transforming program"
    )


def main():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command")
    subparsers.required = True
    
    parser_infer = subparsers.add_parser("infer", description=InferPhaseDescription, formatter_class=argparse.RawTextHelpFormatter)
    parser_infer.add_argument("benchmark", type=str, help="Benchmark to infer")
    parser_infer.add_argument("phase_beg", nargs="?", type=int, help="Start of the phase", default=InferPhase.SingleTypeInfer.value)
    parser_infer.add_argument("phase_end", nargs="?", type=int, help="End of the phase", default=InferPhase.NumPhases.value-1)
    parser_infer.add_argument("--name", type=str, help="Manually specify intermediate name", required=False)

    parser_check = subparsers.add_parser("check", description=CheckPhaseDescription, formatter_class=argparse.RawTextHelpFormatter)
    parser_check.add_argument("benchmark", type=str, help="Benchmark to check")
    parser_check.add_argument("phase_beg", nargs="?", type=int, help="Start of the phase", default=CheckPhase.Conversion.value)
    parser_check.add_argument("phase_end", nargs="?", type=int, help="End of the phase", default=CheckPhase.NumPhases.value-1)
    parser_check.add_argument("--name", type=str, help="Manually specify intermediate name", required=False)

    parser_tf = subparsers.add_parser("transform", description="Transform program")
    parser_tf.add_argument("--name", type=str, help="Manually specify intermediate name", required=True)
    parser_tf.add_argument("--out", type=str, help="Transformed program", required=False)

    args = parser.parse_args()

    if args.command == "infer":
        infer(args)
    elif args.command == "check":
        check(args)
    elif args.command == "transform":
        transform(args)
    else:
        print("Invalid command", args.command)
        exit(1)


if __name__ == '__main__':
    main()
