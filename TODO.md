# TODO

1. Remove subsitute var's super type with itself, which is heuristic and not good.
2. If a variable is in some condition $c$ and its solution $\tau$ is depends on the condition, then also generate two cases as $\tau_\text{taken}, \tau_\text{not taken}$ when branch is taken and not taken.
3. When replace a variable with its solution, check the target expression's condition set. If contain the condition $c$, consider using $\tau_\text{taken}$ or $\tau_\text{not taken}$ instead of $ \tau$.

(I think these are done.)

# File Description
* `code_type.ml` and `gen_type.ml` are old implementation
* I plan to write a new version called `block_type.ml` and `program_type.ml` to replace them.

# Instruction to Support

```asm
rep stosq
bswap	
movq	%rdx, %xmm1
punpcklqdq	%xmm1, %xmm0
movups	%xmm0, 192(%rbx)
bswap	%rax
andb	$2, %cl
cmpb	%cl, $0
rorq	$14, %rcx
rolq	$23, %rax
```

# Transformation Taint Var Instantiation

Where is taint expression used?

* In memory operands and push/pop -> program
* Inspecting child's `in_mem` in its interface -> child's FI
* Caller's `rsp` upon call -> `CallAnno/pr_reg`


# Parser Update

* [ ] Go through each instruction, including the ones in uop, bop, top, and the definition of instruction type. See if they are compatible.
* [ ] Extend `operand` with size field.
* [ ] Fix dirty_op_size, use it as least as possible.


* make sure they are supported
  * mov[ua]p[sd], movdq[ua]
  * punpck, pakkus


# Unsure changes

`lib/checker: ArchType.get_src_op_type`: Should we use data size here?
