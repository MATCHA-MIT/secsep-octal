# SecSep: Securing Cryptographic Software via Typed Assembly Language

This artifact contains two directories:
* `octal/`: inference tool written in OCaml
* `octal-gem5/`: SecSep hardware implemented with gem5

Benchmarks should be placed at `octal-benchmark/` in the same directory.
We omit the convention and structure of it in this README.

## Octal

Prerequisite:
* dune 3.16.0
* ocaml 5.2.0
* opam 2.1.5

Directory Explanation:
* `asm/`: benchmarks to be inferred
* `interface/`: interface specification for benchmarks
* `bin/`: executables
* `checker/`: checker that verifies the inferred result
* `lib/`: Octal

Workflow:
1. Prepare benchmarks to be imported in the benchmark directory.
2. Run `bin/update_bench.sh` to import benchmarks.
3. Run `bin/run.py all asm/{bench_name}.s` to infer, transform, and check the benchmark.
4. Run `bin/install_bench.py` to install transformed program back to benchmark directory.
5. Compile binaries in the benchmark directory.
6. Prepare gem5's container for evaluation.
7. Run `bin/eval.py` to evaluate performance. This optionally runs gem5 simulation.


## Octal Gem5

To setup gem5, use `docker compose` to initialize and start a gem5 container.

Benchmark simulation is initiated by `bin/eval.py` in Octal's directory.
The entry script is `scripts/run.py`.
