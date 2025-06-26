#!/usr/bin/env python3

import click
from functools import wraps
from enum import Enum
import subprocess
from pathlib import Path
import time
from colorama import Fore

script_dir = Path(__file__).resolve().parent
proj_dir = script_dir.parent


class InferPhase(Enum):
    PreprocessInput = 0
    SingleTypeInfer = 1
    RangeTypeInfer = 2
    TaintTypeInfer = 3
    NumPhases = 4


InferPhaseDescription = """Run inference on the benchmark:
    0: Preprocess Input
    1: Single Type Infer
    2: Range Type Infer
    3: Taint Type Infer
"""


class CheckPhase(Enum):
    Conversion = 0
    Check = 1
    NumPhases = 2


CheckPhaseDescription = """Run checker on the infer result:
    0: Conversion
    1: Check
"""


@click.group()
def cli():
    pass


def cli_command():
    context_settings = dict(
        help_option_names=["-h", "--help"],
        max_content_width=88,
    )

    def decorator(func):
        @cli.command(context_settings=context_settings)
        @click.option(
            "--name",
            type=click.STRING,
        )
        @wraps(func)
        def wrapped(*args, **kwargs):
            return func(*args, **kwargs)

        return wrapped

    return decorator


def run(cmd, log_file, msg):
    print(msg, end=" ", flush=True)
    with open(log_file, "w") as f:
        start_time = time.time()
        result = subprocess.run(cmd, stdout=f, stderr=f)
        print(
            f"{Fore.BLUE}[time elapsed: {round(time.time() - start_time, 2)} seconds]{Fore.RESET}",
            end="",
        )
        if result.returncode != 0:
            print(f" ({Fore.RED}errcode: {result.returncode}{Fore.RESET})")
            print(f"{Fore.YELLOW}log saved to {log_file}{Fore.RESET}", flush=True)
            exit(1)
        else:
            print("", flush=True)


@cli_command()
@click.option(
    "--input-dir",
    type=Path,
)
@click.option(
    "--phase",
    nargs=2,
    type=click.Tuple([int, int]),
    default=(0, 3),
)
@click.option(
    "--use-cache",
    is_flag=True
)
def infer(name: str, input_dir: Path, phase, use_cache):
    output_dir = proj_dir / "out" / name
    output_dir.mkdir(exist_ok=True, parents=True)

    phase_beg, phase_end = phase

    for i in range(phase_beg, phase_end + 1):
        if i == InferPhase.PreprocessInput.value:
            run(
                [
                    "dune",
                    "exec",
                    "preprocess_input",
                    "--",
                    "-input-dir",
                    str(input_dir),
                    "-name",
                    name,
                ],
                output_dir / f"{name}.preprocess.log",
                "Running Preprocess Input",
            )
        elif i == InferPhase.SingleTypeInfer.value:
            command = ["dune", "exec", "infer_single", "--", "-name", name]
            if use_cache:
                command.append("-use-cache")
            run(
                command,
                output_dir / f"{name}.single_infer.log",
                "Running Single Type Infer",
            )
        elif i == InferPhase.RangeTypeInfer.value:
            run(
                ["dune", "exec", "infer_range", "--", "-name", name],
                output_dir / f"{name}.range_infer.log",
                "Running Range Type Infer",
            )
        elif i == InferPhase.TaintTypeInfer.value:
            run(
                ["dune", "exec", "infer_taint", "--", "-name", name],
                output_dir / f"{name}.taint_infer.log",
                "Running Taint Type Infer",
            )
        else:
            print("Invalid phase", i)
            exit(1)


@cli_command()
@click.option(
    "--phase",
    nargs=2,
    type=click.Tuple([int, int]),
    default=(0, 1),
)
def check(name: str, phase):
    output_dir = proj_dir / "out" / name
    output_dir.mkdir(exist_ok=True, parents=True)

    phase_beg, phase_end = phase

    for i in range(phase_beg, phase_end + 1):
        if i == CheckPhase.Conversion.value:
            run(
                ["dune", "exec", "convert", "--", "-name", name],
                output_dir / f"{name}.conversion.log",
                "Running Conversion",
            )
        elif i == CheckPhase.Check.value:
            run(
                ["dune", "exec", "check", "--", "-name", name],
                output_dir / f"{name}.check.log",
                "Running Check",
            )
        else:
            print("Invalid phase", i)
            exit(1)


def transform_helper(
    name: str, out=None, delta=None, no_push_pop=None, no_call=None, tf_suffix=""
):
    if out is None:
        output_dir = proj_dir / "out" / name
        output_dir.mkdir(exist_ok=True, parents=True)
        out = output_dir / f"{name}.tf{tf_suffix}.asm"

    arg_list = ["dune", "exec", "prog_transform", "--", "-name", name, "-out", out]
    msg = "Transforming program"
    if delta is not None:
        arg_list += ["--delta", delta]
        delta_int = int(delta, 16)
        msg += f" (delta: 0x{delta_int:x})"
    if no_push_pop:
        arg_list.append("--no-push-pop")
        msg += " (push/pop TF disabled)"
    if no_call:
        arg_list.append("--no-call-preservation")
        msg += " (call untaint preservation TF disabled)"

    run(arg_list, output_dir / f"{name}.transform{tf_suffix}.log", msg)


@cli_command()
@click.option(
    "--delta",  # hex format
    type=click.STRING,
)
@click.option(
    "--tf-option",
    is_flag=True,
)
@click.option(
    "--no-push-pop",
    is_flag=True,
)
@click.option(
    "--no-call",
    is_flag=True,
)
def transform(
    name: str,
    delta: str,
    tf_option: bool,
    no_push_pop: bool,
    no_call: bool,
):
    if tf_option is None:
        transform_helper(
            name, delta=delta, no_push_pop=True, no_call=True, tf_suffix="3"
        )
        transform_helper(name, delta=delta, no_call=True, tf_suffix="2")
        transform_helper(name, delta=delta, no_push_pop=True, tf_suffix="1")
        transform_helper(name, delta=delta)
    else:
        if no_push_pop:
            if no_call:
                tf_suffix = "3"
            else:
                tf_suffix = "1"
        else:
            if no_call:
                tf_suffix = "2"
            else:
                tf_suffix = ""
        transform_helper(
            name,
            delta=delta,
            no_push_pop=no_push_pop,
            no_call=no_call,
            tf_suffix=tf_suffix,
        )


if __name__ == "__main__":
    cli()
