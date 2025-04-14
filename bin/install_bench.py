#!/usr/bin/env python3

from pathlib import Path
import shutil

out_dir = Path("out")
bench_dir = Path("../sechw-const-time-benchmarks")

# Mapping from source (under out_dir) to destination (under bench_dir)
files_to_copy = [
    ("salsa20", "bench_salsa20"),
    ("boringssl_sha512", "bench_sha512_plain"),
    ("boringssl_ed25519", "bench_ed25519_plain_noinline"),
]


def copy(src_path, dst_path):
    if src_path.exists():
        print(f"Copying {src_path} --> {dst_path}")
        dst_path.parent.mkdir(exist_ok=True)
        shutil.copy(src_path, dst_path)
    else:
        print(f"Skipping {src_path} (does not exist)")

# Perform copying if file exists
for app, bench in files_to_copy:
    for tf_suffix in ["", "1", "2", "3"]:
        src_path = out_dir / f"{bench}.tf{tf_suffix}.asm"
        dst_path = bench_dir / "src" / app / f"{bench}_tf{tf_suffix}.s"
        copy(src_path, dst_path)

    for tf_suffix in ["", "_annotated_pub_stack", "_annotated_sec_stack"]:
        src_path = bench_dir / "compiler_pass" / app / f"{bench}{tf_suffix}.s"
        dst_path = bench_dir / "src" / app / f"{bench}{tf_suffix}.s"
        copy(src_path, dst_path)
