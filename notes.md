1. Each reg has its type at start of each block
   (regard static stack address as a register first)
   Each register has type of set of single variable ({x}) at the beginning of a function
2. Each block can only do sym exe using its own vars
   * support constants in types
3. When exiting block, generating new subtype relations
   * (a -> b) + (b -> c) -> (a -> c)
4. Focus on ({val} -> var && var + 1 \ {bound} -> var), if so, derive the var into a range

Data structure:
1. set of vars
2. (list of) subtype relations

```
type reg_type =
| Set of single value ({0}, {1}, ..., {x, a register value, no need to solve it})
| Range (beg, end, step)
| Variable (need to solve it; at start, lots of; at the end, none)
| TypeArith (op * reg_type * reg_type)
| Arith (op * reg_type * value) (value is a single constant)
| Top (everything)
| Bottom (nothing)
```


add rax, rbx
* if rax & rbx are variables, set rax to `Top` (maybe warning)
* arithmetics
  rax = {1}
  rbx = kb
  (+, kb, 1)

