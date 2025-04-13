#!/usr/bin/env python3

from pathlib import Path
import shutil

out_dir = Path("out")
bench_dir = Path("../sechw-const-time-benchmarks")

# Mapping from source (under out_dir) to destination (under bench_dir)
files_to_copy = {
    "bench_salsa20": "src/salsa20",
    "bench_sha512_plain": "src/boringssl_sha512",
    "bench_ed25519_plain_noinline": "src/boringssl_ed25519",
}

# Perform copying if file exists
for bench_name, target_path in files_to_copy.items():
    for tf_suffix in ["", "1", "2", "3"]:
        src_path = out_dir / f"{bench_name}.tf{tf_suffix}.asm"
        dst_path = bench_dir / target_path / f"{bench_name}_tf{tf_suffix}.s"
        if src_path.exists():
            print(f"Copying {src_path} --> {dst_path}")
            dst_path.parent.mkdir(exist_ok=True)
            shutil.copy(src_path, dst_path)
        else:
            print(f"Skipping {src_path} (does not exist)")
