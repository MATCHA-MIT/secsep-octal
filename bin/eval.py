#!/usr/bin/env python3

import click
import re
import logging
from enum import Enum
import argparse
import subprocess
import shutil
from pathlib import Path
import pandas as pd


class LevelBasedFormatter(logging.Formatter):
    FORMATS = {
        logging.DEBUG: "[DEBUG] {%(filename)s:%(funcName)s:%(lineno)d} %(message)s",
        logging.INFO: "[INFO]  %(message)s",
        logging.WARNING: "[WARN]  %(message)s",
        logging.ERROR: "[ERROR] {%(filename)s:%(funcName)s:%(lineno)d} %(message)s",
    }

    def format(self, record):
        log_fmt = self.FORMATS.get(record.levelno, self._fmt)
        formatter = logging.Formatter(log_fmt)
        return formatter.format(record)


for handler in logging.root.handlers[:]:
    logging.root.removeHandler(handler)
handler = logging.StreamHandler()
handler.setFormatter(LevelBasedFormatter())
logger = logging.getLogger()
logger.addHandler(handler)
logger.setLevel(logging.INFO)


OCTAL_DIR = Path(__file__).parent.parent
BENCH_DIR = OCTAL_DIR.parent / "sechw-const-time-benchmarks"
GEM5_DIR = OCTAL_DIR.parent / "gem5-mirror"
EVAL_DIR = OCTAL_DIR / "eval"


BENCH_CONFIG = [
    ("salsa20", "bench_salsa20"),
    ("boringssl_sha512", "bench_sha512_plain"),
    ("boringssl_ed25519", "bench_ed25519_plain_noinline"),
]


class TFCode(Enum):
    Origin = 0
    Octal = 1
    OctalNoPushPop = 2
    OctalNoCallPreserv = 3
    OctalNoPushPopNoCallPreserv = 4
    ProspectPub = 5
    ProspectSec = 6


def run_gem5_on_tf(tf: TFCode) -> bool:
    match tf:
        case TFCode.Origin | TFCode.Octal | TFCode.ProspectPub | TFCode.ProspectSec:
            return True
        case _:
            return False


class HWMode(Enum):
    DefenseOff = "NoDefense"
    DefenseOn = "OurDefense"


HW_ENCODE_MAP = {
    HWMode.DefenseOff: "def-off",
    HWMode.DefenseOn: "def-on",
}


def get_bench_tf_name(bench: str, tf: TFCode) -> str:
    match tf:
        case TFCode.Origin:
            return bench
        case TFCode.Octal:
            return f"{bench}_tf"
        case TFCode.OctalNoPushPop:
            return f"{bench}_tf1"
        case TFCode.OctalNoCallPreserv:
            return f"{bench}_tf2"
        case TFCode.OctalNoPushPopNoCallPreserv:
            return f"{bench}_tf3"
        case TFCode.ProspectPub:
            return f"{bench}_annotated_pub_stack"
        case TFCode.ProspectSec:
            return f"{bench}_annotated_sec_stack"
        case _:
            logger.error(f"Unknown TFCode: {tf}")
            exit(1)


def get_bin_asm(app: str, bench: str, tf: TFCode) -> tuple[Path, Path]:
    bench_tf = get_bench_tf_name(bench, tf)
    bin_path = BENCH_DIR / "out" / app / "bin" / bench_tf
    asm_path = BENCH_DIR / "src" / app / f"{bench_tf}.s"

    assert bin_path and asm_path
    if not bin_path.exists():
        logger.error(f"Binary not found: {bin_path}")
        exit(1)
    if not asm_path.exists():
        logger.error(f"Source ASM not found: {asm_path}")
        exit(1)
    logger.debug(
        f"app={app}, bench={bench}, tf={tf}, bin_path={bin_path}, asm_path={asm_path}"
    )
    return bin_path, asm_path


def collect_bin_asm(bin_file: Path, asm_file: Path):
    target_dir = EVAL_DIR / "bench"
    target_dir.mkdir(parents=True, exist_ok=True)
    shutil.copy(bin_file, target_dir / bin_file.name)
    shutil.copy(asm_file, target_dir / asm_file.name)


def get_asm_line_count(collection: dict, asm_file: Path) -> dict:
    line_output_dir = EVAL_DIR / "asm_lines"
    line_output_dir.mkdir(parents=True, exist_ok=True)

    result = subprocess.run(
        [
            "dune",
            "exec",
            "stat_asm",
            "--",
            "-asm",
            str(asm_file),
            "-preview",
            line_output_dir / f"{asm_file.name}.lines",
        ],
        cwd=OCTAL_DIR,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        logger.error(f"Failed to run stat_asm on {asm_file}: {result.stderr}")
        exit(1)

    pattern = re.compile(r"asm line count: (\d+)")
    stat_match = pattern.search(result.stdout)
    if stat_match:
        line_count = int(stat_match.group(1))
        logger.debug(f"ASM line count for {asm_file.name}: {line_count}")
        collection["asm_lines"] = line_count
        return collection
    else:
        logger.error(f"Failed to parse ASM line count from output: {result.stdout}")
        exit(1)


def get_perf(collection: dict, bin_file: Path, repeat=1000) -> dict:
    if repeat <= 1:
        logger.error("repeat must be greater than 1")
        exit(1)

    perf_output_dir = EVAL_DIR / "perf"
    perf_output_dir.mkdir(parents=True, exist_ok=True)

    result = subprocess.run(
        [
            "perf",
            "stat",
            "-r",
            str(repeat),
            "-e",
            "cycles,instructions",
            "--",
            str(bin_file),
        ],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        logger.error(f"Failed to run perf on {bin_file}: {result.stderr}")
        exit(1)
    perf_output = result.stderr

    cycles_pattern = re.compile(r"\s*(\d+)\s+cycles.*\+-\s*(\d+[\d\.]*)%")
    instr_pattern = re.compile(r"\s*(\d+)\s+instructions.*\+-\s*(\d+[\d\.]*)%")

    cycles_match = cycles_pattern.search(perf_output)
    instrs_match = instr_pattern.search(perf_output)

    if not cycles_match:
        logger.error(f"Failed to find cycles in perf output: {perf_output}")
        exit(1)
    if not instrs_match:
        logger.error(f"Failed to find instructions in perf output: {perf_output}")
        exit(1)

    cycles, cycles_std = int(cycles_match.group(1)), float(cycles_match.group(2)) / 100
    instrs, instrs_std = int(instrs_match.group(1)), float(instrs_match.group(2)) / 100
    logger.debug(f"Perf output: {perf_output}")
    logger.debug(f"Cycles: {cycles}, Cycles Std: {cycles_std}")
    logger.debug(f"Instructions: {instrs}, Instructions Std: {instrs_std}")

    collection["cycles"] = cycles
    collection["cycles_std"] = cycles_std
    collection["dyn_instrs"] = instrs
    collection["dyn_instrs_std"] = instrs_std
    return collection


def get_gem5_result(
    collection: dict,
    gem5_docker: str,
    skip_gem5: bool,
    delta: str,
    app: str,
    bench_tf: str,
) -> dict:
    if not skip_gem5:
        # Run gem5 to get result
        # python3 scripts/run.py --apps boringssl_ed25519 --bench-regex "^bench_ed25519_plain_noinline(_tf)?$"
        logger.info(f"Running gem5 for {bench_tf}...")
        result = subprocess.run(
            [
                "docker",
                "exec",
                gem5_docker,
                "python3",
                "scripts/run.py",
                "--apps",
                app,
                "--bench-regex",
                f"^{bench_tf}$",
                "--delta",
                delta,
            ]
        )
        if result.returncode != 0:
            logger.error(f"Failed to run gem5 for {bench_tf}: {result.stderr}")
            exit(1)

    output_dir = EVAL_DIR / "gem5"
    output_dir.mkdir(parents=True, exist_ok=True)
    for hw in HWMode:
        raw_stats_file = (
            GEM5_DIR
            / "results"
            / "raw_data"
            / f"{app}-{bench_tf}"
            / hw.value
            / "stats.txt"
        )
        stats_file = output_dir / f"{bench_tf}-{hw.value}.txt"
        shutil.copy(raw_stats_file, stats_file)

        with open(stats_file, "r") as f:
            content = f.read()

        # system.cpu.numCycles       656420      # Number of cpu cycles simulated (Cycle)
        cycles_pattern = re.compile(r"system.cpu.numCycles\s+(\d+)\s+")
        # system.cpu.committedInsts  1496528     # Number of Instructions Simulated (Count)
        instrs_pattern = re.compile(r"system.cpu.committedInsts\s+(\d+)\s+")
        cycles_match = cycles_pattern.search(content)
        instrs_match = instrs_pattern.search(content)
        if not cycles_match or not instrs_match:
            logger.error(
                f"Failed to find cycles / dyn instrs in gem5 output for {bench_tf} {hw}"
            )
            exit(1)
        cycles = int(cycles_match.group(1))
        instrs = int(instrs_match.group(1))
        logger.debug(f"{bench_tf} {hw}: cycles={cycles}, instrs={instrs}")

        collection[f"gem5_cycles_{HW_ENCODE_MAP[hw]}"] = cycles
        collection[f"gem5_instrs_{HW_ENCODE_MAP[hw]}"] = instrs


def get_overhead(base: dict, curr: dict) -> dict:
    joint_keys = set(base.keys()).intersection(set(curr.keys()))
    overhead = {}
    for key in joint_keys:
        if key.find("_std") == -1:
            overhead[key] = None

    for key in overhead.keys():
        if key.find("_std") != -1:
            continue
        std_key = key + "_std"

        base_val = base[key]
        base_std = base[std_key] if std_key in base else None
        curr_val = curr[key]
        curr_std = curr[std_key] if std_key in curr else None

        growth = (curr_val - base_val) / base_val
        std = (base_std**2 + curr_std**2) ** 0.5 if base_std is not None and curr_std is not None else None

        overhead[key] = (base_val, curr_val, growth, std)

    return overhead


def print_overhead(overhead: dict):
    metrics = list(overhead.keys())
    metrics.sort()
    for key in metrics:
        if overhead[key] is None:
            continue
        base_val, curr_val, growth, std = overhead[key]
        std_str = f"(+-{std:.2%})" if std is not None else ""
        overhead_str = f"{growth:.2%}{std_str}"
        print(
            f"\t{key:>25}: {overhead_str:<20} {base_val:>10} -> {curr_val}"
        )


@click.command()
@click.option("-v", "--verbose", is_flag=True, help="Enable verbose logging")
@click.option(
    "--gem5-docker",
    default="sct-gem5",
    required=False,
    type=click.STRING,
    help="Docker container name running gem5",
)
@click.option(
    "--skip-gem5", is_flag=True, help="Skip running gem5 and use last results"
)
@click.option(
    "--delta",
    default="0x100000",
    required=False,
    type=click.STRING,
    help="Absolute offset between public and secret stack",
)
@click.option("-o", "--out", type=click.Path(), required=False)
def main(verbose, gem5_docker, skip_gem5, delta, out):
    if verbose:
        logger.setLevel(logging.DEBUG)

    df_index = pd.MultiIndex.from_product(
        [
            [bench for _, bench in BENCH_CONFIG],
            [tf.name for tf in TFCode],
        ],
        names=["Benchmark", "TF"],
    )
    df_columns = [
        "asm_lines",
        "cycles",
        "cycles_std",
        "dyn_instrs",
        "dyn_instrs_std",
        "gem5_cycles_def-off",
        "gem5_cycles_def-on",
        "gem5_instrs_def-off",
        "gem5_instrs_def-on",
    ]
    df_columns.extend([f"overhead_{col}" for col in df_columns])
    df = pd.DataFrame(columns=df_columns, index=df_index)

    for app, bench in BENCH_CONFIG:
        results = {}
        for tf in TFCode:
            bench_tf = get_bench_tf_name(bench, tf)
            bin_path, asm_path = get_bin_asm(app, bench, tf)
            collect_bin_asm(bin_path, asm_path)

            result = {}
            get_asm_line_count(result, asm_path)
            get_perf(result, bin_path)
            if run_gem5_on_tf(tf):
                get_gem5_result(result, gem5_docker, skip_gem5, delta, app, bench_tf)
            results[tf] = result

            for key, value in result.items():
                df.loc[(bench, tf.name), key] = value
            

        for tf in TFCode:
            if tf == TFCode.Origin:
                continue
            overhead = get_overhead(results[TFCode.Origin], results[tf])
            for key, (_, _, growth, std) in overhead.items():
                df.loc[(bench, tf.name), f"overhead_{key}"] = growth
                if std is not None:
                    df.loc[(bench, tf.name), f"overhead_{key}_std"] = std
            print(f"Overhead of {bench} / {tf.name}")
            print_overhead(overhead)
            print()
    
    if out:
        out = Path(out)
        out_suffix = out.suffix
        match out_suffix:
            case ".csv":
                logger.info(f"Saving result to CSV file {out}")
                df.to_csv(out)
            case ".json":
                logger.info(f"Saving result to JSON file {out}")
                df.to_json(out)
            case ".xlsx":
                logger.info(f"Saving result to Excel file {out}")
                df.to_excel(out)


if __name__ == "__main__":
    main()
