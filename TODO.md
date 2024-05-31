# TODO

1. Remove subsitute var's super type with itself, which is heuristic and not good.
2. If a variable is in some condition $c$ and its solution $\tau$ is depends on the condition, then also generate two cases as $\tau_\text{taken}, \tau_\text{not taken}$ when branch is taken and not taken.
3. When replace a variable with its solution, check the target expression's condition set. If contain the condition $c$, consider using $\tau_\text{taken}$ or $\tau_\text{not taken}$ instead of $ \tau$.

(I think these are done.)

# File Description
* `code_type.ml` and `gen_type.ml` are old implementation
* I plan to write a new version called `block_type.ml` and `program_type.ml` to replace them.

