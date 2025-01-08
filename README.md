# Sechw-Const-Time

# Type Inference
```bash
dune build
./_build/default/bin/infer_single.exe -name bench_ed25519_plain_noinline -asm ./asm/bench_ed25519_plain_noinline.s &> out/ed25519.txt
./_build/default/bin/infer_range.exe -name bench_ed25519_plain_noinline &> out/ed25519_range.txt
# infer_taint without conservative taint api to get base taint infer result
./_build/default/bin/infer_taint.exe -name bench_ed25519_plain_noinline &> out/ed25519_taint.txt
# generate conservative taint api based on the first infer_taint result
./_build/default/bin/gen_csvt_taint_api.exe -name bench_ed25519_plain_noinline
# infer taint with conservative taint api
./_build/default/bin/infer_taint.exe -name bench_ed25519_plain_noinline -use-taint-api &> out/ed25519_taint.txt
# instantiate_taint now only filter alive functions
./_build/default/bin/instantiate_taint.exe -name bench_ed25519_plain_noinline &> out/ed25519_instantiate.txt
./_build/default/bin/prog_transform.exe -name bench_ed25519_plain_noinline -out <out-asm-path> &> out/ed25519_transform.txt
```
