# Sechw-Const-Time

# Type Inference

## Dep Type Infer

Technique details worth documenting:
1. Loop invariant infer for loops that span across multiple blocks.
2. Bwd prop for heuristic generated tmp context (how to handle branch condition during bwd prop)
3. try_solve_single_corr in SingleSubtype
    1. The overall logic is similar to RangeSubtype
    2. We need to break circular dependency! Applied logic similar to SingleBlockInvariance. We need to find subtype values come from outside of the loop, but to find corr, it should not be the rootest subtype (like we used in try_solve_single_sub_val), otherwise it would be harder to find corr. The more direct the subtype is, the easier it is to figure out correlation.
    3. Another key point it to find header and loop step/resume br pc.
4. Use branch block's branch history to simplify ITE in single subtypes. Tradeoff of this simplification:
    1. We only want to apply this to loop infer so that the pattern to match is simpler.  
    2. But over-simplify will also make it harder to apply try_solve_single_corr since this will cause some shared pattern among blocks not shared anymore.
5. Allow bound with block var, but the bound must be resolved. Why? This might cause a circular dependency issue in some tricky cases. Check complex loop examples. (actually bound does not need to be resolved now!!!)
6. Allow base with block var: the challenge is to represent base with the corresponding block's block var. We need to choose it carefully (harder than bound).
    1. We first find base for header block (for now we find the direct subtype whose pc is not the loop step/resume pc)
    2. We then use header block's counter to find other block's base.
7. Tricky case in loop solution:
    1. Length is not aligned with step: we can solve this problem by calculating the accurate bound with mod during inference
    2. End val is cannot be simply represented as a single exp (e.g., each loop do shift right to the counter), then we still have to reresent the loop out sol as a range.
    3. Old dirty fix for gen_other_loop_sol is deprecated since (1) it is not correct; (2) we do not need it after we calculating accurate bound with mod in 1.
    4. We need to resolve some inner loop counter even when its base is not resolved since the base cannot be represented in a nice formula before the outer loop is resolved, while the outer loop need the end_val of the inner loop counter to be resolved. However, in this case we cannot determine alignment, so for now we can only apply this technique when inner loop solution has step = 1.


### When do I represent solution to one block var with other block vars (from the same basic block)?
1. try_solve_loop_cond, represent solution of a var with the actual loop counter (another block var)
2. update_sol_set_correlation
3. try_solve_single_corr

Among the three cases, only try_solve_single_corr will use unresolved block var (e.g., y) to represent sol of other block vars (e.g., x=y+1).

### Logic of simplify block var solution
Overall rule of simplify block var solution:
1. If current solution is represented by a block var, while the var can be represented by some other single exp sol, then substitute the var with its solution.
2. If a solution contains a block var which cannot be representedc by other single exp, then keep it to be there. We want to represent as many solutions with single exp as possible.

### SingleSubtype.is_sol_resolved
1. We only allow represent solution of one var (e.g., `x`) with another unresolved solution to break the circular dependency chain between variables.
2. It seems that we do not need these variables to do other operations such as look up in memory, so is_sol_resolved will consider such var (`x`) as unresolved if its dependency is not resolved. This will prevent several sub_sol function to simplify the solution.
3. It seems that I do not need to check whether a bound is resolved before using it to represent a loop counter's solution. I may added that check because I want to observe some features to help me understand the program behavior (which may be not necessary for inference). (Hopefully removing this dependency will help us avoid more circular dependnecy issues.)

### Complex Loop Examples
```
    k = 0
    i = 0
loop1:
    ...
loop2:
    ...
    i++
    if i < k, jmp to loop2
    ...
    k += 4
    if i < n, jmp to loop1
```
In this example, we need to first solve `i`, and then solve `k`. However, our current rule does not allow us to solve `i` in the inner loop before its boundary `k` is resolved.

### Scale: Default Taint
* taint annotation: `@taint[0/1/?]`
* For memory slots:
  * stack slots: always `? (i.e. taint var)`; no way to specify
  * others: default is `tainted`; specify with `@taint`
* For registers:
  * `rsp`: always `untainted`; no way to specify
  * `rdi, rsi, rdx, rcx, r8, r9`:
    * as data: default is `tainted`; specify with `@taint`
    * as pointer: always `untainted`; no way to specify
  * Other registers: default is `?`; specify with `@taint`


### NOTE
1. We assume no block start with conditional branches (which will make some pc-dependent substitution incorrect in single_subtype)
2. We do not merge subtype with different pc_list
3. Since we substitute subtype solution, when finding var map across blocks, it is better to use supertype (e.g., when unifying base and bound).
4. Solution contains SingleTop (tmp solution to break circular dependency) should be considered as unresolved as SolNone.


### TODO
1. poly1305_clean: support loop infer whose base contains block var
2. Clean up current loop infer logic (remove range exp canonicalize) (done)
3. Further optimize mod eval
4. Check whether it is ok to simply allow block var in range in sub_sol_single_to_range_naive_repl
5. Check range eval in sub_sol_single_to_range_naive_repl (Sub, and step)
6. Check old benchmarks (salsa20, table_select, etc.)
7. Check whether I over-claimed something in inference-related writing.
8. Removing is_sol_resolved requirement for boundary actually make djbsort_simple try to check top. Need to fix this! (done)
9. Some simple cases are not resolved (get solutions by combining subtypes, failed when solution is range)
10. VERY IMPORTANT!!! try_solve_single_sub_val does not deal with block var in range correctly!
11. How to deal with multi-block loop or subtype relation is a very important topic. I should definitely mention it in the paper orzzz.
12. Replace many loop structure with map for faster lookup (e.g., type_rel list).
13. Subtype exp might be eventually simplified to block var, which make try_solve_single_sub_val ignore this subtype -> FIX this!!!
14. We need to clean up subsitute in SingleSubtype to add recursive replacement!
15. Consider to remove merge_set_sol
16. We should probably still apply try_solve_single_corr for var with set sol (otherwise if set sol rule is applied first, there might be problems for us to find loop base)
17. Find step -> can skip if step already found



16. Add test bench:
```
    i = 4, j = 0
loop:
    j++
    jmp to loop if j < i
    i+=4
    jmp to loop if i < n
```
