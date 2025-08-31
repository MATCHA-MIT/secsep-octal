#!/usr/bin/env python3

import click
from functools import wraps
from enum import Enum
import subprocess
from pathlib import Path
import time
from colorama import Fore
import shutil
from multiprocessing import Pool

from general import *


class InferPhase(Enum):
    PreprocessInput = 0
    SingleTypeInfer = 1
    RangeTypeInfer = 2
    TaintTypeInfer = 3
    NumPhases = 4


InferPhaseDescription = """Run inference on the benchmark:
    0: Preprocess Input
    1: Single Type Infer
    2: Valid Region Infer
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
            help="Name of the benchmark",
        )
        @click.option(
            "--log-file",
            type=Path,
            default=None,
            help="Log terminal outputs into a file",
        )
        @wraps(func)
        def wrapped(*args, **kwargs):
            return func(*args, **kwargs)

        return wrapped

    return decorator


def run(cmd, cmd_output, msg):
    logging.info(f"{msg} started")
    with open(cmd_output, "w") as f:
        start_time = time.time()
        result = subprocess.run(cmd, stdout=f, stderr=f)
        logging.info(
            f"{msg} time elapsed: {Fore.BLUE}{round(time.time() - start_time, 2)} seconds{Fore.RESET}",
        )
        if result.returncode != 0:
            logging.error(f"{msg} result: {Fore.RED}error{Fore.RESET} exit code: {result.returncode}")
            logging.error(f"{msg} {Fore.YELLOW}log saved to {cmd_output}{Fore.RESET}")
            raise RuntimeError(f"Failed when {msg}")
        else:
            logging.info(f"{msg} result: {Fore.GREEN}success{Fore.RESET}")


def infer_core(
    name: str,
    log_file: Optional[Path],
    input_dir: Path,
    phase: tuple[int, int],
    use_cache: bool,
    build: bool,
):
    setup_logger(logging.INFO, log_file)
    if build:
        build_octal()

    output_dir = OCTAL_WORK_DIR / name
    output_dir.mkdir(exist_ok=True, parents=True)

    phase_beg, phase_end = phase

    for i in range(phase_beg, phase_end + 1):
        if i == InferPhase.PreprocessInput.value:
            run(
                [
                    OCTAL_PPC,
                    "-input-dir",
                    str(input_dir),
                    "-name",
                    name,
                ],
                output_dir / f"{name}.preprocess.log",
                f"[{name}] (Preprocess)",
            )
        elif i == InferPhase.SingleTypeInfer.value:
            command = [OCTAL_INFER_SINGLE, "-name", name]
            if use_cache:
                command.append("-use-cache")
            run(
                command,
                output_dir / f"{name}.single_infer.log",
                f"[{name}] (Single Type Infer)",
            )
        elif i == InferPhase.RangeTypeInfer.value:
            run(
                [OCTAL_INFER_RANGE, "-name", name],
                output_dir / f"{name}.range_infer.log",
                f"[{name}] (Valid Region Infer)",
            )
        elif i == InferPhase.TaintTypeInfer.value:
            run(
                [OCTAL_INFER_TAINT, "-name", name],
                output_dir / f"{name}.taint_infer.log",
                f"[{name}] (Taint Type Infer)",
            )
        else:
            logging.error(f"Invalid phase {i}")
            raise ValueError(f"Invalid phase {i}")


@cli_command()
@click.option(
    "--input-dir",
    type=Path,
    required=True,
    help="SecSep benchmark analysis directory",
)
@click.option(
    "--phase",
    nargs=2,
    type=click.Tuple([int, int]),
    default=(0, InferPhase.NumPhases.value - 1),
    help="0: Preprocessing, 1: Dependent type infer, 2: Range type infer, 3: Taint type infer",
)
@click.option("--use-cache", is_flag=True)
def infer(name, log_file, input_dir, phase, use_cache):
    """Infer the given assembly program"""
    infer_core(name=name, log_file=log_file, input_dir=input_dir, phase=phase, use_cache=use_cache, build=True)


def check_core(name: str, log_file: Optional[Path], phase: tuple[int, int], build: bool):
    setup_logger(logging.INFO, log_file)
    if build:
        build_octal()

    output_dir = OCTAL_WORK_DIR / name
    output_dir.mkdir(exist_ok=True, parents=True)

    phase_beg, phase_end = phase

    for i in range(phase_beg, phase_end + 1):
        if i == CheckPhase.Conversion.value:
            run(
                [OCTAL_CONVERT, "-name", name],
                output_dir / f"{name}.conversion.log",
                f"[{name}] (Type Checker Preprocess)",
            )
        elif i == CheckPhase.Check.value:
            run(
                [OCTAL_CHECK, "-name", name],
                output_dir / f"{name}.check.log",
                f"[{name}] (Type Checker)",
            )
        else:
            logging.error(f"Invalid phase {i}")
            raise ValueError(f"Invalid phase {i}")


@cli_command()
@click.option(
    "--phase",
    nargs=2,
    type=click.Tuple([int, int]),
    default=(CheckPhase.Check.value, CheckPhase.Check.value),
)
def check(name, log_file, phase):
    """Check the inference results of the given assembly program"""
    check_core(name=name, log_file=log_file, phase=phase, build=True)


def transform_helper(
    name: str,
    out=None,
    delta=None,
    no_push_pop=None,
    no_call_preservation=None,
    tf_suffix="",
    install_dir=None,
):
    output_dir = OCTAL_WORK_DIR / name
    output_dir.mkdir(exist_ok=True, parents=True)
    if out is None:
        out = output_dir / f"{name}.tf{tf_suffix}.s"

    arg_list = [OCTAL_TRANSFORM, "-name", name, "-out", out]
    msg = f"[{name}] (Transformation)"
    if delta is not None:
        arg_list += ["--delta", delta]
        delta_int = int(delta, 16)
        msg += f" (delta: 0x{delta_int:x})"
    if no_push_pop:
        arg_list.append("--no-push-pop")
        msg += " (push/pop TF disabled)"
    if no_call_preservation:
        arg_list.append("--no-call-preservation")
        msg += " (call untaint preservation TF disabled)"

    run(arg_list, output_dir / f"{name}.transform{tf_suffix}.log", msg)
    if install_dir is not None:
        target = install_dir / name / out.name
        shutil.copy(out, target)
        logging.info(f"{Fore.MAGENTA}Installing {target}{Fore.RESET}")


def transform_core(
    name: str,
    log_file: Optional[Path],
    delta: str,
    all_tf: bool,
    no_push_pop: Optional[bool],
    no_call_preservation: Optional[bool],
    install_dir: Path,
    build: bool,
):
    setup_logger(logging.INFO, log_file)
    if build:
        build_octal()

    if all_tf:
        transform_helper(
            name,
            delta=delta,
            no_push_pop=True,
            no_call_preservation=True,
            tf_suffix="3",
            install_dir=install_dir,
        )
        transform_helper(
            name, delta=delta, no_call_preservation=True, tf_suffix="2", install_dir=install_dir
        )
        transform_helper(
            name, delta=delta, no_push_pop=True, tf_suffix="1", install_dir=install_dir
        )
        transform_helper(name, delta=delta, install_dir=install_dir)
    else:
        if no_push_pop:
            if no_call_preservation:
                tf_suffix = "3"
            else:
                tf_suffix = "1"
        else:
            if no_call_preservation:
                tf_suffix = "2"
            else:
                tf_suffix = ""
        transform_helper(
            name,
            delta=delta,
            no_push_pop=no_push_pop,
            no_call_preservation=no_call_preservation,
            tf_suffix=tf_suffix,
            install_dir=install_dir,
        )


@cli_command()
@click.option(
    "--delta",
    type=click.STRING,
    required=True,
    help="Delta used by SecSep transformation. Must be in hex format, e.g. 0x800000",
)
@click.option(
    "--all-tf",
    is_flag=True,
    help="Generate all transformation variations",
)
@click.option(
    "--no-push-pop",
    is_flag=True,
    help="Disable pass: consecutive push/pop optimization",
)
@click.option(
    "--no-call",
    is_flag=True,
    help="Disable pass: callee-saved registers' taint restoration C_{callee}",
)
@click.option(
    "--install-dir",
    type=Path,
    help="Directory to install transformed assembly, usually the analysis directory"
)
def transform(name, log_file, delta, all_tf, no_push_pop, no_call_preservation, install_dir):
    """Transform the given assembly program using the inference results"""
    transform_core(name=name, log_file=log_file, delta=delta, all_tf=all_tf,
        no_push_pop=no_push_pop, no_call_preservation=no_call_preservation,
        install_dir=install_dir, build=True,
    )


def full_worker(
    name: str,
    log_file: Optional[Path],
    analysis_dir: Path,
    delta: str,
):
    try:
        infer_core(name=name, log_file=log_file, input_dir=analysis_dir, phase=(0, InferPhase.NumPhases.value - 1), use_cache=False, build=False)
        check_core(name=name, log_file=log_file, phase=(CheckPhase.Check.value, CheckPhase.Check.value), build=False)
        transform_core(name=name, log_file=log_file, delta=delta, all_tf=True, no_push_pop=None, no_call_preservation=None, install_dir=analysis_dir, build=False)
        logging.info(f"Successfully processed {name}")
        return True
    except Exception as e:
        logging.error(f"Error processing {name}: {e}")
        return False


@cli_command()
@click.option(
    "--analysis-dir",
    type=Path,
    default=BENCHMARK_DIR,
    help="SecSep benchmark analysis directory",
)
@click.option(
    "--delta",
    type=click.STRING,
    required=True,
    help="Delta used by SecSep transformation. Must be in hex format, e.g. 0x800000",
)
@click.option(
    "-p",
    "--processes",
    default=6,
    required=False,
    type=click.INT,
    help="Number of processes to use for parallel execution",
)
def full(name: Optional[str], log_file: Optional[Path], analysis_dir: Path, delta: str, processes: int):
    """Infer, check, and transform the given assembly program"""

    setup_logger(logging.INFO, log_file)
    build_octal()

    if name is not None:
        if name.find(",") != -1:
            names = name.split(",")
        else:
            names = [name]
        worker_args = [(name.strip(), log_file, analysis_dir, delta) for name in names]
    else:
        worker_args = [(name, log_file, analysis_dir, delta) for name in BENCHMARKS]
    logging.info(f"Working on {len(worker_args)} benchmark(s)...")

    with Pool(processes=processes) as pool:
        worker_results = pool.starmap(full_worker, worker_args)
        if all(worker_results):
            logging.info("All benchmark(s) completed successfully")
        else:
            logging.error("Some worker(s) failed")
            raise RuntimeError("Some worker(s) failed")


if __name__ == "__main__":
    cli()
