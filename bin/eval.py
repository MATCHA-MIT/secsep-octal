#!/usr/bin/env python3

import click
import re
import logging
from enum import Enum
import subprocess
import shutil
from pathlib import Path
import pandas as pd
import datetime
import json
import resource
from multiprocessing import Pool


SECSEP_DIR = Path(__file__).parent.parent
SECSEP_WORK_DIR = SECSEP_DIR / "out"
SECSEP_STAT_ASM = SECSEP_DIR / "_build" / "default" / "bin" / "stat_asm.exe"
SECSEP_PHASE_INFER = ["single_infer", "range_infer", "taint_infer"]
SECSEP_PHASE_CHECK = "check"
SECSEP_PHASES = SECSEP_PHASE_INFER + [SECSEP_PHASE_CHECK]
SECSEP_PHASE_METRICS = ["time", "smt_queries", "smt_queries_time"]
PROSPECT_STATS = [
    "loc", "num_all_funcs", "num_local_vars",
    "num_public_var_anno", "num_secret_var_anno",
]
SECSEP_STATS = [
    "num_funcs", "num_args", "num_scale_anno",
]
STATS = [
    *PROSPECT_STATS, *SECSEP_STATS,
    "infer_time", "infer_smt_time", "infer_smt_time_pct", "infer_smt_queries",
    "check_time", "check_smt_time", "check_smt_time_pct", "check_smt_queries",
]
BENCH_DIR = SECSEP_DIR.parent / "sechw-const-time-benchmarks" / "bench"
GEM5_DIR = SECSEP_DIR.parent / "gem5-mirror"
GEM5_DOCKER_BENCH_DIR = "/root/benchmarks/bench"
EVAL_DIR = SECSEP_DIR / "eval" / f"{datetime.datetime.now().strftime('%Y-%m-%d_%H-%M-%S')}"

DEFAULT_RLIMIT_STACK_SIZE_MB = 16

BENCHMARKS = [
    "salsa20",
    "sha512",
    "ed25519_sign",
    "chacha20",
    "poly1305_clean",
    "x25519",
]

BENCHMARK_PAPER_ORDER = {
    "salsa20": "salsa20",
    "sha512": "sha512",
    "chacha20": "chacha20",
    "poly1305": "poly1305_clean",
    "x25519": "x25519",
    "ed25519_sign": "ed25519_sign",
}


class TF(Enum):
    Origin = 0
    Secsep = 1
    SecsepNoPushPop = 2
    SecsepNoCallPreserv = 3
    SecsepNoPushPopNoCallPreserv = 4
    ProspectPub = 5
    ProspectSec = 6


def run_gem5_on_tf(tf: TF) -> bool:
    return True
    # match tf:
    #     case TF.Origin | TF.Secsep | TF.SecsepNoCallPreserv | TF.ProspectPub | TF.ProspectSec:
    #         return True
    #     case _:
    #         return False


class HWMode(Enum):
    DefenseOff = "NoDefense"
    DefenseOn = "OurDefense"


HW_ENCODE_MAP = {
    HWMode.DefenseOff: "def-off",
    HWMode.DefenseOn: "def-on",
}


LOG_PATH = EVAL_DIR / "eval.log"


def setup_logger(level=logging.INFO):
    console_handler = logging.StreamHandler()
    console_handler.setFormatter(logging.Formatter("[%(levelname)s] <%(processName)s> - %(message)s"))
    console_handler.setLevel(logging.DEBUG)

    file_handler = logging.FileHandler(LOG_PATH, mode="a")
    file_handler.setFormatter(logging.Formatter("[%(asctime)s] [%(levelname)s] <%(processName)s> - %(message)s"))
    file_handler.setLevel(logging.DEBUG)

    logger = logging.getLogger()
    if logger.hasHandlers():
        logger.handlers.clear()
    logger.addHandler(console_handler)
    logger.addHandler(file_handler)
    logger.setLevel(level)

    
def build_secsep():
    logging.info("Building Secsep...")
    subprocess.run(
        [
            "dune",
            "build"
        ],
        cwd=SECSEP_DIR,
        check=True,
    )


def get_bench_tf_name(bench: str, tf: TF) -> str:
    match tf:
        case TF.Origin:
            return bench
        case TF.Secsep:
            return f"{bench}.tf"
        case TF.SecsepNoPushPop:
            return f"{bench}.tf1"
        case TF.SecsepNoCallPreserv:
            return f"{bench}.tf2"
        case TF.SecsepNoPushPopNoCallPreserv:
            return f"{bench}.tf3"
        case TF.ProspectPub:
            return f"{bench}.prospect_pubstk"
        case TF.ProspectSec:
            return f"{bench}.prospect_secstk"
        case _:
            logging.error(f"Unknown TF: {tf}")
            raise ValueError()


def print_secsep_stats_latex(df: pd.DataFrame, out: Path):
    with open(out, "w") as f:
        for bench_print, bench in BENCHMARK_PAPER_ORDER.items():
            if bench not in BENCHMARKS:
                logging.warning(f"Skipping {bench_print} when printing paper table")
                continue
            bench_print = bench_print.replace("_", "\\_")
            loc = df.at[bench, "loc"]
            num_all_funcs = df.at[bench, "num_all_funcs"]
            num_local_vars = df.at[bench, "num_local_vars"]
            num_prospect_pubstk_anno = df.at[bench, "num_secret_var_anno"]
            num_prospect_secstk_anno = df.at[bench, "num_public_var_anno"]
            num_funcs = df.at[bench, "num_funcs"]
            num_func_args = df.at[bench, "num_args"]
            num_scale_anno = df.at[bench, "num_scale_anno"]
            infer_time = df.at[bench, "infer_time"]
            infer_smt_pct = df.at[bench, "infer_smt_time_pct"]
            infer_smt_queries = df.at[bench, "infer_smt_queries"]
            check_time = df.at[bench, "check_time"]
            check_smt_pct = df.at[bench, "check_smt_time_pct"]
            check_smt_queries = df.at[bench, "check_smt_queries"]

            f.write(f"\\texttt{{{bench_print:<15}}} ")
            f.write(f"& {loc:>4} &  {num_all_funcs:>3} / {num_local_vars:>3}  ")
            f.write(f"&  {num_prospect_pubstk_anno:>3} / {num_prospect_secstk_anno:>3}  ")
            f.write(f"&  {num_funcs:>3} / {num_func_args:>3}  & {num_scale_anno:>3} ")
            f.write(f"&  {infer_time:>6.1f}s / {infer_smt_pct:>4.1f}\\% / {infer_smt_queries:>5}  ")
            f.write(f"&  {check_time:>6.1f}s / {check_smt_pct:>4.1f}\\% / {check_smt_queries:>5}  ")
            f.write(f"\\\\\n")

            
def collect_secsep_stats():
    target_dir = EVAL_DIR / "stats"
    target_dir.mkdir(parents=True, exist_ok=True)
    df = pd.DataFrame(
        index=BENCHMARKS,
        columns=pd.Index(STATS).append(
            pd.MultiIndex.from_product([SECSEP_PHASES, SECSEP_PHASE_METRICS], names=["Phase", "Metric"])
        )
    )
    for bench in BENCHMARKS:
        bench_work_dir = SECSEP_WORK_DIR / bench
        stat = dict()

        phase_cnt = 0
        for stat_file in bench_work_dir.glob("*.stat"):
            shutil.copy(stat_file, target_dir)
            file_bench, file_phase = stat_file.stem.split(".")[:2]
            if file_bench != bench:
                logging.error(f"Unexpected stat file {stat_file} with name {file_bench} in directory of {stat_file}")
                raise ValueError()
            if file_phase not in SECSEP_PHASES:
                logging.error(f"Unexpected stat file {stat_file} with unknown phase {file_phase}")
                raise ValueError()
            phase_cnt += 1
            with open(stat_file, "r") as f:
                stat[file_phase] = json.load(f)
            time, smt_time, smt_queries = stat[file_phase]["time"], stat[file_phase]["smt_queries_time"], stat[file_phase]["smt_queries"]
            if file_phase in SECSEP_PHASE_INFER:
                stat["infer_time"] = stat.get("infer_time", 0) + time
                stat["infer_smt_time"] = stat.get("infer_smt_time", 0) + smt_time
                stat["infer_smt_queries"] = stat.get("infer_smt_queries", 0) + smt_queries
            elif file_phase == SECSEP_PHASE_CHECK:
                stat["check_time"] = time
                stat["check_smt_time"] = smt_time
                stat["check_smt_queries"] = smt_queries
        if phase_cnt != len(SECSEP_PHASES):
            logging.error(f"Expected {len(SECSEP_PHASES)} phases, but found {phase_cnt} in {bench_work_dir}")
            raise ValueError()
        stat["infer_smt_time_pct"] = stat["infer_smt_time"] / stat["infer_time"] * 100
        stat["check_smt_time_pct"] = stat["check_smt_time"] / stat["check_time"] * 100

        bench_stat_file = BENCH_DIR / bench / f"{bench}.stat"
        if bench_stat_file.exists():
            shutil.copy(bench_stat_file, target_dir)
            with open(bench_stat_file, "r") as f:
                bench_stat = json.load(f)
            for k, v in bench_stat.items():
                if k in SECSEP_STATS:
                    stat[k] = v
        else:
            logging.error(f"Stat file not found for {bench}")
            raise FileNotFoundError()
        bench_stat_file = BENCH_DIR / bench / f"{bench}.stat_noopt"
        if bench_stat_file.exists():
            shutil.copy(bench_stat_file, target_dir)
            with open(bench_stat_file, "r") as f:
                bench_stat = json.load(f)
            for k, v in bench_stat.items():
                if k in PROSPECT_STATS:
                    stat[k] = v
                elif k == "num_funcs":
                    # distinguish between noopt and opt
                    stat["num_all_funcs"] = v
        else:
            logging.error(f"Stat file (noopt) not found for {bench}")
            raise FileNotFoundError()

        for k, v in stat.items():
            if k in SECSEP_PHASES:
                for key, value in v.items():
                    df.at[bench, (k, key)] = value
            else:
                df.at[bench, k] = v

    out = EVAL_DIR / "secsep.csv"
    logging.info(f"Saving secsep stats to CSV file {out}")
    df.to_csv(out)

    out_stat_table = EVAL_DIR / "secsep.tex"
    logging.info(f"Saving secsep stats to LaTeX table {out_stat_table}")
    print_secsep_stats_latex(df, out_stat_table)


def get_bin_asm(bench: str, tf: TF) -> tuple[Path, Path, Path]:
    bench_tf = get_bench_tf_name(bench, tf)
    bin_path          = BENCH_DIR / bench / "build" / bench_tf
    asm_path          = BENCH_DIR / bench / f"{bench_tf}.s"
    compiled_asm_path = BENCH_DIR / bench / "build" / f"{bench_tf}.asm"

    assert bin_path and asm_path and compiled_asm_path
    if not bin_path.exists():
        logging.error(f"Binary not found: {bin_path}")
        raise FileNotFoundError()
    if not asm_path.exists():
        logging.error(f"Source ASM not found: {asm_path}")
        raise FileNotFoundError()
    if not compiled_asm_path.exists():
        logging.error(f"Compiled ASM not found: {compiled_asm_path}")
        raise FileNotFoundError()
    logging.debug(
        f"bench={bench}, tf={tf}, bin_path={bin_path}, asm_path={asm_path}, compiled_asm_path={compiled_asm_path}"
    )
    return bin_path, asm_path, compiled_asm_path


def collect_bin_asm(bin_file: Path, asm_file: Path, compiled_asm_file: Path):
    target_dir = EVAL_DIR / "bench"
    target_dir.mkdir(parents=True, exist_ok=True)
    shutil.copy(bin_file, target_dir)
    shutil.copy(asm_file, target_dir)
    shutil.copy(compiled_asm_file, target_dir)


def get_asm_line_count(collection: dict, asm_file: Path) -> dict:
    line_output_dir = EVAL_DIR / "asm_lines"
    line_output_dir.mkdir(parents=True, exist_ok=True)

    result = subprocess.run(
        [
            SECSEP_STAT_ASM,
            "-asm",
            str(asm_file),
            "-preview",
            line_output_dir / f"{asm_file.name}.lines",
        ],
        cwd=SECSEP_DIR,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        logging.error(f"Failed to run stat_asm on {asm_file}")
        logging.info(result.stderr)
        raise RuntimeError()

    pattern = re.compile(r"asm line count: (\d+)")
    stat_match = pattern.search(result.stdout)
    if stat_match:
        line_count = int(stat_match.group(1))
        logging.debug(f"ASM line count for {asm_file.name}: {line_count}")
        collection["asm_lines"] = line_count
        return collection
    else:
        logging.error(f"Failed to parse ASM line count from output")
        logging.info(result.stdout)
        raise ValueError()


def get_perf(collection: dict, bin_file: Path, repeat=1000) -> dict:
    if repeat <= 1:
        logging.error("repeat must be greater than 1")
        raise ValueError()

    perf_output_dir = EVAL_DIR / "perf"
    perf_output_dir.mkdir(parents=True, exist_ok=True)

    result = subprocess.run(
        [
            "perf",
            "stat",
            "-r",
            str(repeat),
            "-e",
            "cycles,instructions,cache-references,cache-misses,branches,branch-misses",
            "--",
            str(bin_file),
        ],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        logging.error(f"Failed to run perf on {bin_file}")
        logging.info(result.stderr)
        raise RuntimeError()
    perf_output = result.stderr

    cycles_pattern = re.compile(r"\s+([\d,]+)\s+cycles.*\+-\s*(\d+[\d\.]*)%")
    instr_pattern = re.compile(r"\s+([\d,]+)\s+instructions.*\+-\s*(\d+[\d\.]*)%")
    cache_refs_pattern = re.compile(r"\s+([\d,]+)\s+cache-references")
    cache_misses_pattern = re.compile(r"\s+([\d,]+)\s+cache-misses")
    branches_pattern = re.compile(r"\s+([\d,]+)\s+branches")
    branch_misses_pattern = re.compile(r"\s+([\d,]+)\s+branch-misses")

    cycles_match = cycles_pattern.search(perf_output)
    instrs_match = instr_pattern.search(perf_output)
    cache_refs_match = cache_refs_pattern.search(perf_output)
    cache_misses_match = cache_misses_pattern.search(perf_output)
    branches_match = branches_pattern.search(perf_output)
    branch_misses_match = branch_misses_pattern.search(perf_output)

    if not cycles_match:
        logging.error(f"Failed to find cycles in perf output")
        logging.info(perf_output)
        raise ValueError()
    if not instrs_match:
        logging.error(f"Failed to find instructions in perf output")
        logging.info(perf_output)
        raise ValueError()
    if not cache_refs_match:
        logging.error(f"Failed to find cache references in perf output")
        logging.info(perf_output)
        raise ValueError()
    if not cache_misses_match:
        logging.error(f"Failed to find cache misses in perf output")
        logging.info(perf_output)
        raise ValueError()
    if not branches_match:
        logging.error(f"Failed to find branches in perf output")
        logging.info(perf_output)
        raise ValueError()
    if not branch_misses_match:
        logging.error(f"Failed to find branch misses in perf output")
        logging.info(perf_output)
        logging.warning("try:\necho 0 | sudo tee /proc/sys/kernel/nmi_watchdog\n...\necho 1 | sudo tee /proc/sys/kernel/nmi_watchdog")
        raise ValueError()

    cycles, cycles_std = int(cycles_match.group(1).replace(",", "")), float(cycles_match.group(2)) / 100
    instrs, instrs_std = int(instrs_match.group(1).replace(",", "")), float(instrs_match.group(2)) / 100
    cache_refs = int(cache_refs_match.group(1).replace(",", ""))
    cache_misses = int(cache_misses_match.group(1).replace(",", ""))
    branches = int(branches_match.group(1).replace(",", ""))
    branch_misses = int(branch_misses_match.group(1).replace(",", ""))
    logging.debug(f"Perf output: {perf_output}")
    logging.debug(f"Cycles: {cycles}, Cycles Std: {cycles_std}")
    logging.debug(f"Instructions: {instrs}, Instructions Std: {instrs_std}")
    logging.debug(f"Cache References: {cache_refs}")
    logging.debug(f"Cache Misses: {cache_misses}")
    logging.debug(f"Branches: {branches}")
    logging.debug(f"Branch Misses: {branch_misses}")

    collection["cycles"] = cycles
    collection["cycles_std"] = cycles_std
    collection["dyn_instrs"] = instrs
    collection["dyn_instrs_std"] = instrs_std
    collection["cache_refs"] = cache_refs
    collection["cache_misses"] = cache_misses
    collection["cache_miss_rate"] = cache_misses / cache_refs if cache_refs > 0 else 0
    collection["branches"] = branches
    collection["branch_misses"] = branch_misses
    collection["branch_miss_rate"] = branch_misses / branches if branches > 0 else 0
    return collection


def get_gem5_result(
    collection: dict,
    gem5_docker: str,
    skip_gem5: bool,
    delta: str,
    app: str,
    bench_tf: str,
):
    if not skip_gem5:
        # Run gem5 to get result
        # python3 scripts/run.py --apps boringssl_ed25519 --bench-regex "^bench_ed25519_plain_noinline(_tf)?$"
        logging.info(f"Running gem5 for {bench_tf}...")
        result = subprocess.run(
            [
                "docker",
                "exec",
                gem5_docker,
                "python3",
                "scripts/run.py",
                "--bench-dir",
                str(GEM5_DOCKER_BENCH_DIR),
                "--apps",
                app,
                "--bench-regex",
                f"^{bench_tf}$",
                "--delta",
                delta,
                # "--debug-flags=Process"
            ],
            capture_output=True,
            text=True,
        )
        if result.returncode != 0:
            logging.error(f"Failed to run gem5 for {bench_tf}")
            logging.info("Gem5 stdout:\n" + result.stdout)
            logging.info("Gem5 stderr:\n" + result.stderr)
            raise RuntimeError()
        logging.info("Gem5 output:\n" + result.stdout)

        result = subprocess.run(
            [
                "docker",
                "exec",
                gem5_docker,
                "python3",
                "scripts/get_decl.py",
                app,
                bench_tf,
                HWMode.DefenseOn.value,
            ],
            capture_output=True,
            text=True,
        )
        if result.returncode != 0:
            logging.error(f"Failed to run get_decl.py for {bench_tf}")
            logging.info(result.stderr)
            raise RuntimeError()
        out_decl_file = (
            GEM5_DIR
            / "results"
            / "raw_data"
            / f"{app}-{bench_tf}"
            / HWMode.DefenseOn.value
            / "declassify_inst"
        )
        decl_file = EVAL_DIR / "declassification" / f"{bench_tf}.decl.txt"
        decl_file.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy(out_decl_file, decl_file)
    else:
        logging.debug(f"Skipping gem5 for {bench_tf}, using last results")

    for hw in HWMode:
        out_stats_file = (
            GEM5_DIR
            / "results"
            / "raw_data"
            / f"{app}-{bench_tf}"
            / hw.value
            / "stats.txt"
        )
        stats_file = EVAL_DIR / "gem5" / f"{bench_tf}-{hw.value}.txt"
        stats_file.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy(out_stats_file, stats_file)

        with open(stats_file, "r") as f:
            content = f.read()

        # system.cpu.numCycles       656420      # Number of cpu cycles simulated (Cycle)
        cycles_pattern = re.compile(r"system.cpu.numCycles\s+(\d+)\s+")
        # system.cpu.committedInsts  1496528     # Number of Instructions Simulated (Count)
        instrs_pattern = re.compile(r"system.cpu.committedInsts\s+(\d+)\s+")
        cycles_match = cycles_pattern.search(content)
        instrs_match = instrs_pattern.search(content)
        if not cycles_match or not instrs_match:
            logging.error(
                f"Failed to find cycles / dyn instrs in gem5 output for {bench_tf} {hw}"
            )
            raise ValueError()
        cycles = int(cycles_match.group(1))
        instrs = int(instrs_match.group(1))
        logging.debug(f"{bench_tf} {hw}: cycles={cycles}, instrs={instrs}")

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

        if key.find("gem5") == -1:
            base_val = base[key]
            base_std = base[std_key] if std_key in base else None
            curr_val = curr[key]
            curr_std = curr[std_key] if std_key in curr else None

            growth = (curr_val - base_val) / base_val
            std = (
                (base_std**2 + curr_std**2) ** 0.5
                if base_std is not None and curr_std is not None
                else None
            )

        else:
            # special handling for gem5 stats
            # metrics with def on should be compared with original benchmark with def off
            matching = re.compile(r"gem5_(\w+)_def-(\w+)").match(key)
            if not matching:
                logging.error(f"Failed to match gem5 key: {key}")
                raise ValueError()
            metric, mode = matching.groups()
            logging.debug(f"key={key} --> metric={metric}, mode={mode}")
            if mode == "off":
                base_val = base[f"gem5_{metric}_def-off"]
                curr_val = curr[f"gem5_{metric}_def-off"]
            else:
                if mode != "on":
                    logging.error(f"Unknown mode: {mode}")
                    raise ValueError()
                base_val = base[f"gem5_{metric}_def-off"]
                curr_val = curr[f"gem5_{metric}_def-on"]
            growth = (curr_val - base_val) / base_val
            std = None

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
        print(f"\t{key:>25}: {overhead_str:<20} {base_val:>10} -> {curr_val}")

        
def worker(bench: str, tf: TF, log_level: int, gem5_docker: str, skip_gem5: bool, delta: str):
    setup_logger(log_level)

    try:
        bench_name_tf = get_bench_tf_name(bench, tf)
        result = {}
        if run_gem5_on_tf(tf):
            get_gem5_result(result, gem5_docker, skip_gem5, delta, bench, bench_name_tf)
        logging.debug(f"Gem5 result of {bench} - {tf.name} collected successfully")
        return result
    except Exception as e:
        logging.error(f"Error processing {bench} - {tf.name}: {e}")
        return None


@click.command()
@click.option("-v", "--verbose", count=True, help="Enable verbose logging (-v, -vv)")
@click.option(
    "--gem5-docker",
    default="secsep-gem5",
    required=False,
    type=click.STRING,
    help="Docker container name running gem5",
)
@click.option(
    "--skip-perf", is_flag=True, help="Skip running perf"
)
@click.option(
    "--skip-gem5", is_flag=True, help="Skip running gem5 and use last results"
)
@click.option(
    "--print-overhead", is_flag=True, help="Print overheads in console"
)
@click.option(
    "--delta",
#   default="0x800000",
    required=True,
    type=click.STRING,
    help="Absolute offset between public and secret stack",
)
@click.option(
    "-p",
    "--processes",
    default=4,
    required=False,
    type=click.INT,
    help="Number of processes to use for parallel execution",
)
@click.option(
    "-r",
    "--rlimit-stack",
    "rlimit_stack_mb",
    default=DEFAULT_RLIMIT_STACK_SIZE_MB,
    required=False,
    type=click.INT,
    help="Set larger stack size to run transformed benchmarks on host",
)
@click.option("-o", "--out", type=click.Path(), required=False)
def main(verbose, gem5_docker, skip_perf, skip_gem5, print_overhead, delta, processes, out, rlimit_stack_mb):
    resource.setrlimit(resource.RLIMIT_STACK, (rlimit_stack_mb * 1024 * 1024, resource.RLIM_INFINITY))

    EVAL_DIR.mkdir(parents=True, exist_ok=False)
    with open(EVAL_DIR / "config.txt", "w") as f:
        f.write(f"gem5_docker={gem5_docker}\n")
        f.write(f"skip_perf={skip_perf}\n")
        f.write(f"skip_gem5={skip_gem5}\n")
        f.write(f"delta={delta}\n")
        f.write(f"processes={processes}\n")
        f.write(f"out={out}\n")

    if verbose >= 2:
        setup_logger(logging.DEBUG)
    elif verbose == 1:
        setup_logger(logging.INFO)
    else:
        setup_logger(logging.WARN)

    build_secsep()
    collect_secsep_stats()

    if skip_perf:
        logging.info("Will skip perf runs")
    if not skip_gem5:
        print(f"Will run gem5, confirm? (y/n) ", end="")
        confirm = input()
        if confirm.lower() != "y":
            logging.info("Will skip running gem5")
            skip_gem5 = True
    if skip_gem5:
        logging.info("Will skip gem5 runs")

    df_index = pd.MultiIndex.from_product(
        [
            BENCHMARKS,
            [tf.name for tf in TF],
        ],
        names=["Benchmark", "TF"],
    )
    df_columns = [
        "asm_lines",
        "cycles",
        "cycles_std",
        "dyn_instrs",
        "dyn_instrs_std",
        "cache_refs",
        "cache_misses",
        "cache_miss_rate",
        "branches",
        "branch_misses",
        "branch_miss_rate",
        "gem5_cycles_def-off",
        "gem5_cycles_def-on",
        "gem5_instrs_def-off",
        "gem5_instrs_def-on",
    ]
    df_columns.extend([f"overhead_{col}" for col in df_columns])
    df = pd.DataFrame(columns=df_columns, index=df_index)

    results = dict()

    # Run perf and fast procedures using single-thread execution
    for bench in BENCHMARKS:
        for tf in TF:
            try:
                bin_path, asm_path, compiled_asm = get_bin_asm(bench, tf)
                collect_bin_asm(bin_path, asm_path, compiled_asm)
                r = results.setdefault((bench, tf), {})
                get_asm_line_count(r, asm_path)
                if not skip_perf:
                    get_perf(r, bin_path)
                for key, value in r.items():
                    df.loc[(bench, tf.name), key] = value
            except Exception as e:
                logging.warning(f"Failed to process {bench} - {tf.name}, skipping: {e}")
                continue

    # Parallelize gem5 tasks
    worker_args = []
    for bench in BENCHMARKS:
        for tf in TF:
            worker_args.append((bench, tf, logging.getLogger().level, gem5_docker, skip_gem5, delta))
    with Pool(processes=processes) as pool:
        worker_results = pool.starmap(worker, worker_args)
        for args, result in zip(worker_args, worker_results):
            bench, tf, _, _, _, _ = args
            if result is None:
                logging.warning(f"Failed to process {bench} - {tf.name}, skipping")
                continue
            r = results.setdefault((bench, tf), {})
            for key, value in result.items():
                df.loc[(bench, tf.name), key] = value
                r[key] = value

    # Calculate overheads
    for bench in BENCHMARKS:
        for tf in TF:
            if tf == TF.Origin:
                continue
            if (bench, tf) not in results:
                logging.warning(f"Results of {bench} - {tf.name} not found, skipping overhead calculation")
                continue

            overhead = get_overhead(results[(bench, TF.Origin)], results[(bench, tf)])
            for key in overhead:
                if overhead[key] is None:
                    continue
                _, _, growth, std = overhead[key]
                df.loc[(bench, tf.name), f"overhead_{key}"] = growth
                if std is not None:
                    df.loc[(bench, tf.name), f"overhead_{key}_std"] = std
            
            if print_overhead:
                print(f"Overhead of {bench} / {tf.name}")
                print_overhead(overhead)
                print()

    df = df.dropna(axis=1, how='all')
    if out is None:
        out = EVAL_DIR / "eval.csv"
        logging.info(f"No output file specified, saving to {out}")
    out = Path(out)
    out_suffix = out.suffix
    match out_suffix:
        case ".csv":
            logging.info(f"Saving result to CSV file {out}")
            df.to_csv(out)
        case ".json":
            logging.info(f"Saving result to JSON file {out}")
            df.to_json(out)
        case ".xlsx":
            logging.info(f"Saving result to Excel file {out}")
            df.to_excel(out)


if __name__ == "__main__":
    main()
