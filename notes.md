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


SMT heavy part: the call of `get_update_list` in `loop`
use existing solution to simplify the unknown address list, compare the simplified address with existing ranges in memory layout (`update_offset_all_ptr`)
simple contraints if unknown address resolves to [a, b], generate initial contrainsts (a <? b) (base extraction problem?)
  if found, ok
  if not found, generate constraints by guessing it to be locating in some range (some most possible range)
     memory layout [l1, r1] [l2, r2]

TODO: pass in old constraints in here, and propagation, so that SMT can guess

with OLD, compare boundaries ..., generate possible NEWs if necessary


No address range can cross the zero

Type =

S-BinaryExp (
   Add,
   S-BinaryExp (
      Add,
      S-BinaryExp (
         Add,
         S-BinaryExp (
            Mul,
            S-BinaryExp (
               And,
               S-BinaryExp (
                  Add,
                  S-BinaryExp (Add, SymImm 20, SymImm 2),
                  Const 89
               ),
               Const -8
            ),
            Const -1
         ),
         SymImm 20
      ),
      SymImm 2
   ),
   Const 81
), 

Cond = [16 NotTaken]
