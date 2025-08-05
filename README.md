# Sechw-Const-Time

# Type Inference

## Benchmarks
We list benchmarks and how we adjusted them for our experiments.
We need to annotate two things:

1. Function interface
2. Special structs on the stack that needs to be splitted.

### salsa20
1. Call with variable length to prevent the compiler from doing some length-dependent optimization.
2. In `salsa20`, we modified the code by initializing `uint8_t block[64]` with zero, to solve the incapability of type system in range infer.

### sha512
1. Additionally initialize `sha512_state_st.p` to make range infer easier (should eventually revert this change).
2. It suddenly works without the above countermeasure. Need double check!!! (Maybe test with different input lengths and check checker's correctness!)
3. TODO: test without the manual memset we added.

### ed25519
1. Currently we only have sign, no verify (due to declassification)
2. Added noinline to specific functions to make infer and check work.

### chacha20
1. The benchmark itself allows `in` and `out` array are the same array, but our type system can only work if we assume they are non-overlapped.
2. We also add an extra assumption to indicate that `in - out >= 32`.
3. Our range infer cannot infer valid region of array that is gradually filled in a nested loop. Hence, our tool only works if we mark out array as initialized (valid).

### poly1305
1. Original bench has goto -> we replaced it with loops and function calls
2. It passes a pointer to an array and later cast the array pointer to a struct with secret and public data.
3. Current main function only call with specific length, which is not good.
4. Need to fix the main function - need to test with different input lengths to prevent us from generating too general constraints!!! (done)

### x25519
1. setb inst is not implemented in infer or checker
2. Need to add X25519_public_from_private to test bench

### djbsort
1. Still need extra data structures: introduce extra block variables to represent variables passed from previous blocks that cannot be represented as single exp
2. Still need to handle shift right and left (multiply by 2) on loop counter.

### kyber

### rsa

### p256
1. It seems that we might be able to infer this one without being worreid about group?
2. We still need to correctly annotate memory for `group->order.N.d` (it's a dynamically allocated memory).


### TODO
1. We need to improve mem_type and scale's annotation to specify may-overlap relation between array and specific fields of struct.
2. We need to check whether each benchmark function is called in a proper way to make sure that the performance numbers make sense.
3. Structs on stack is not annotated properly, which causes the main function of poly1305 to fail.

* general: every benchmark repeats 100 times
* salsa20: initialize a stack allocated array to zero, to help range inference (better explanation?)
* sha512: (!! if we make SHA512\_Update noinline, we need to initialize SHA512\_CTX.p to zero; after we do this, checker ctx check complains, need to fix)
* ed25519_sign: we make some functions noinline so that the infer tool works (@shixin: make it work, or make it work faster?)
* chacha20:
  * mention we implement more than the type system described in paper: we support overlapping memory, and we can cite Andres' paper
  * after removing unnecessary noinline from `chacha20_core`, it is inlined, and checker complains on ctx check, need to fix; after fixing, check sha512's checker problem above and see if they are the same problem
* poly1305: remove irreducible control flow in poly1305\_update;
(@shixin: didn't find this problem ---> pointer stored to array and then cast back. can you clarify?)
* x25519: none

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
    3. **Limitation**: our tool requires that there must exists a corresponding single exp in loop body blocks to represent the base (if the base is a var only in the block before loop, i.e., preheader, and has set solution, then we cannot find the solution).
7. Tricky case in loop solution:
    1. Length is not aligned with step: we can solve this problem by calculating the accurate bound with mod during inference
    2. End val is cannot be simply represented as a single exp (e.g., each loop do shift right to the counter), then we still have to reresent the loop out sol as a range.
    3. Old dirty fix for gen_other_loop_sol is deprecated since (1) it is not correct; (2) we do not need it after we calculating accurate bound with mod in 1.
    4. We need to resolve some inner loop counter even when its base is not resolved since the base cannot be represented in a nice formula before the outer loop is resolved, while the outer loop need the end_val of the inner loop counter to be resolved. However, in this case we cannot determine alignment, so for now we can only apply this technique when inner loop solution has step = 1.
8. We should first canonicalize bound val and then try to find end val and resume val. Otherwise there might be issues such as "mod number with unexpected sign" (e.g., I think I am mod a positive length, but it can be negative after minus step. As a result, the expression does not work as expection.)
9. In the current version single set sol is still only used for sol exp with input var only (during single infer)
10. Propagate br cond:
    1.  SingleInputVarCondSubtype: works well with loop, but can only deal with branch cond that can be converted to input var
    2.  SingleBrCondProp: works with block var, but so far it cannot propagate cond outside a loop body to blocks in the loop body (the propagate logic is relative simple)
    3.  TODO: improve SingleBrCondProp so that it may evetually replace SingleInputVarCondSubtype
11. Problem of downgraded solution to loop counter:
    1. For loop counters of loops that can only be executed once, we will first solve the counter as SolCond (range in, range resume, val out).
    2. Then, we will notice that the loop jump back branch can never be taken, and the sol will be downgraded to SolSimple (Single begin_val). 
    3. The problem is that the solution does not reveal the relation between the counter and the bound val, which is not helpful for us to infer a good fomula of valid region after the loop, which is better to be represented by loop bound.
    4. Hence, we use the old Sol to generated SolCond for loop counter (one side of the SolCond will be useless). (`SingleSubtype.combine_loop_cond_sol_map`)
    5. Benifit:
       1. SingleInfer: help to unify counter's out value with other branches where the counter register's value is directly generated.
       2. RangeInfer: represent the valid region after loop with bound value, which help to unify the valid region formula.
    6. Potential issue of this implementation:
       1. Sol is staled or wrong (since it comes from the old iter)
       2. Sol is not fully simplified by current solution to other var (the new iter may resolve more var)


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


### Why do we want to sub sol even though we already have smt solver?
1. The goal is to enable easy pattern matching.
2. For SingleITE, we somehow do not directly do pattern matching with its condition, so we rely on SMT solver to evaluate the condition and only simplify its left and right expression.


### Summary of different options to sub sol
1. sub_sol_single_var
   1. Only consider SolSimple (Single sol)
   2. Used for SingleInputVarCondSubtype
2. sub_sol_single_to_range_naive_repl
   1. Sub to range if repl_range_sol
   2. Sub to set if repl_set_sol (naive version)
   3. Used in single_block_invariance
3. sub_sol_single_to_offset_opt
   1. Used as sub_sol_func in ArchType.type_prop_block
   2. Specifically used for memory look up
   3. No sub range in these use cases (to avoid making the range too loose, which might leads to unknown addresses)
4. sub_sol_single_to_offset_list
   1. Used as sub_sol_list_func in ArchType.type_prop_block, specifically used for SingleExp.find_base_adv (find base ptr) in mem lookup
   2. Sub to range + sub to set: we want to repl as much as possible to help us get the base pointer
   3. Since we do not rely on this to check memory access, it is fine if this one leads to memory address with wild ranges that does not fit in any mem slot.
5. sub_sol_offset_to_offset
   1. Used in get_heuristic_mem_type to get addresses used for generating extra invariants (added to tmp_context)
   2. Sub to range (no sub to set)
   3. We want to get rid of loop var, but keep loop boundaries in unified formula (instead of each term in set sol) to find invariants shared by all branches jump to the current block
6. sub_sol_offset_to_offset_list
   1. Used in get_heuristic_mem_type to heuristically find the accessed memory slot
   2. Sub to range + sub to set
   3. We want to get rid of loop var (we want to constrain loop boundary), and we only need to use one instance to loop up for the accessed slot
7. sub_sol_single_to_range
   1. range_subtype (repl_range_sol=true):
      1. We rely on whether the var is a loop var to decide which rule to apply. 
      2. I am quite surprised that infer is success even when we do not repl_range_sol here (which should not be the case orzzzzz)
   2. update_mem (repl_range_sol=false): insert new stack slots (likely for the purpose of reserving space for callees), so it should not matter
   3. add_var_bound_constraint
      1. Goal is to find non-ptr, useful input vars and constrain their value to [0, 1<<31)
      2. Why sub to range but not just check sol??? TODO!!!
   4. SingleRetInvariance.gen_ret_invariance (repl_range_sol=false): whether sub to range does not matter since I care about infer invariants from set sol
8. sub_sol_single_to_range_opt
   1. Not used in other places
   2. Used in try_solve_single_sub_val to simplify sub type that contains var with set sol -> no need to consider about range here
9. sub_sol_single_to_single_func_interface
   1. sub for func interface return val

Helper functions:
1. sub_sol_single_set_var
   1. Sub all single set sol, and return list of all possible combinations
2. sub_sol_to_offset_list
   1. Pure helper func

Other sub (used duing infer):
1. substitute_one_exp_single_sol (helper): sub to single (consider pc assoc to the exp, so that loop var on loop out side can take a single value)
2. substitute_one_exp_single_sol_list
3. substitute_one_exp_subtype_list
4. update_subtype_single_sol (TODO: check later!!!)

### Dilemma of sub to range (about boundary invariants infer)
If sub to range, it may make the expression too conservative and lead to unknown address (in nested loop, when there are sth like counter1 - counter2).
If not, then in get_heuristic_mem_type, when we try to generate tmp context, we will use loop counter to generate the constraint. However, tmp_conext infer (invariance infer) does not work with counter. It should be converted to bound.

Here is the current strategy:
1. Most places we do not sub to range
2. In tmp context invariance infer and update_mem we sub to range.

Note: we want to use get_heuristic_mem_type to constrain bound, instead of the counter itself.
If we cannot constrain bound through get_heuristic_mem_type, we can only expect we can find a nice bound expression that already satisfy/carry this constraint.

Tricky case: bound comes from two branches, one has constraint by conditional branch, the other constrained by mod operation. We have to unify this to carry this constraint (prop br cond does not work here since it only constrain the orignal var in the branch block, not the var to be unified in the target block.)

Another decision choice:
1. When find heuristic mem slot, we sub both range and set sol
2. When generate tmp context, we only sub range sol, but not set sol. This is because for var with set sol, we want to directly constrain its range instead of each of its subtype (which may not consistently satisfy the constraint) (actually we only want to constrain its subtype's val in the corresponding branch block's context).

TODO: Potential improvement: 
1. Replace counter vars following their dependency order may solve this problem.
2. Propagagte the branch conditions more smartly so that even the format of the bound is not unified, we can still infer the condition - NOT EASY!


### NOTE
1. We assume no block start with branches 
    1. When we cannot find sub block with sub/sup pc, and sub_pc=sup_pc, we know it is the case that single subtype generate subtype in the same block context after call
    2. If block starts with conditional branches, it will make some pc-dependent substitution incorrect in single_subtype
2. We do not merge subtype with different pc_list
3. Since we substitute subtype solution, when finding var map across blocks, it is better to use supertype (e.g., when unifying base and bound).
4. Solution contains SingleTop (tmp solution to break circular dependency) should be considered as unresolved as SolNone.
5. Use smt solver to judge alignment is very useful (even with naive context setup) (do not need to think about how to simplify mod or judge alignment using my own heuristics)


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
15. Consider to remove merge_set_sol (no, we cannot remove unless we finish the next todo: apply try_solve_single_corr for var with set sol)
16. We should probably still apply try_solve_single_corr for var with set sol (otherwise if set sol rule is applied first, there might be problems for us to find loop base)
17. Find step -> can skip if step already found
18. SingleInvarianceInfer: find loop/non loop can use information extracted from loop info of new tv_rel!
19. Single infer does not need to generate range/taint constraints.
20. Need to be careful about check for top before any call to smt solver! Need to clean up code. Sometimes we may need the check, sometimes we may not.
21. get_branch_cond also only need for the header var (an optimization in single infer)
22. Infer invariance from bwd prop needs to improve how we decide whether to exclude a cond from the assertion or not (loop or not judgement).
23. Consider add slot idx to FullMemAnno
24. How to handle declassification?
25. How to handle pointer in branch condition (used in proof)?
26. Improve sub to range for exp with counters in nested loops
27. add_var_bound_constraint???
28. combine_loop_cond_sol_map need to improve this to avoid generating wrong solution


29. Add test bench:
```
    i = 4, j = 0
loop:
    j++
    jmp to loop if j < i
    i+=4
    jmp to loop if i < n
```

## Range Type Infer

### Summary of rules and use cases

1. Not in a loop
   1. try_solve_full, try_solve_const_slot, try_solve_hybrid_slot
   2. No circular dependency, no subtype removed/ignored, do not need to worried about solution being too strong or weak
2. At loop head
   1. Write to the slot in the loop
      1. Slot is already full before entering the loop:
         1. Derive solution by find subset of the base br's valid region do not make the solution too weak (valid region too large), but may make the solution too strong (valid region too small)
         2. We apply try_solve_full and ignore the subtype corresponding to step back pc. (`get_subtype_without_step_back_pc step_back_pc_set false tv_rel`)
         3. We either get no solution or get full slot solution, so the solution will not be too strong (too small).
      2. Slot is not full before entering the loop:
         1. try_solve_extra_slot is applied to solve valid region for slots that is gradually filled in the loop.
         2. **Limitations**:
            1. Can only handle short loop: if the slot is not written in the same block with step back pc, it does not work
            2. Can not handle nested loop: so far we cannot deal with circular dependency between valid region of outer loop body and inner loop body.
         3. There might be some corner cases that other rules will work...
   2. No write to the slot in the loop
      1. The subtype corresponding to step back pc is a equal var, can be ignored when applying the following three rules. (`get_subtype_without_step_back_pc step_back_pc_set true tv_rel`)
      2. try_solve_full, try_solve_const_slot, try_solve_hybrid_slot
      3. Intuitively, we only need to figure out valid region when entering the loop to get the solution.
3. In loop body
   1. If resolved at loop head, then infer is similar to case 1 (Not in a loop).

try_solve_empty is a special one (we may be able to remove it)

TODO: Double check whether `get_subtype_without_step_back_pc` will make solution too weak...

### Solution template
RangeSolTemplate:
For slot whose input valid region is special (not full nor empty), we generate solution template from its input valid region's pattern.
The intuiation is that this pattern might be some invariants that will be kept through the function (e.g., poly1305, sha512).
Note that all var in the input valid region are input vars.
Then, the key point of generating the solution template is to decide which vars should remained the same across the function, and which vars should be adjusted to corresponding reg/slot's var when generating template.

Intuition:
1. Keep var appeared in mem offset
2. Replace var in mem valid region (range) but not in mem offset.

### Update valid region after func call
Two options:
1. New valid region = valid region before call U valid region derived from callee's out valid region
2. New valid region = valid region before call \ mem region accessed by callee U valid region derived from callee's out valid region
3. New valid region = valid region derived from callee's out valid region

TODO: which one should I use? Need to think about this with type check of taint?
Currently I think I am using 1, but it seems that poly1305's main's range infer only works if I use 2 (otherwise we cannot represent the return block's valid region). This is not a problem since it is return state for stack region, but it is better to fix this to avoid potential problems.

We experienced problem when using solely option 1:
* multiple calls use the same region
* some prior call initializes the largest region, where the boundary is represented using some variable `b`
* all later calls initialize less than the largest region, therefore `b` is still used to represented the region
* however context related to `b` has been lost due to later calls

We now uses a mixture of 1 and 3 to update each memory slot in caller
* when caller's slot is fully mapped into callee, then we use 3
* otherwise (partially mapped), we use 1

### TODO
1. Equal var and equal var constraints: this is very messy, we keep part of it as a double check (but we do not use it to filter subtypes any more, i.e, `RangeSubtype.filter_self_subtype` is deprecated)
2. Double check whether `get_subtype_without_step_back_pc` will make solution too weak...
3. Clean up RangeSubtype's sub solution to use ArchContextMap
4. Which option of update valid region after func call should I use? Change to the cleaner one
5. Allow marking range var that we failed to infer as empty.


# Type Check

## Variable Size Maintainance

Variable size map (vsm):
* `infer_var_size_map`: infer var size by observing vars in registers and memory slot of no more than 8 bytes.
   * called for each arch_type, and each function interface's in/out reg/mem
   * for a variable representing a register at block start, size reflects the full register (8 bytes, 64 bits)

Calculate variable size smartly:
* Only sign-extend as needed when the arguments for an expression mismatch
* If we over sign-extend, we mess up the heuristic guess of the width of meaningful value, and cannot perform zero-extend to place this value correctly into a register (e.g., caller initializes %ecx using 32-bit expression, and then call callee, where callee's %rcx should be the 4-byte expression zero-extended by 32 bits)

Conversion of single variable map always uses zero extension:
* Such map is used for mapping target arch_type variable into some expression in current context
* Those variables are block variables at the beginning of target arch_type.
  Their size represent the width of the GPR register (8-byte).
  If the mapped expression has less then 8 byte, then it suggests that the same register is partially used in current context.
  Therefore, we should use zero extension.


## Dep Type Check

### Func call
1. What do we need to do to update mem content + permission?
2. Check how we update valid region after func call

### Context map (Br Anno and Call Anno)
1. For branch anno, needs to check that the branch context substitute map does not substitute input variables or taint variables (done for taint variables, still needs to be done for input dep variables).
2. We also need to check that the map does not substitute var to top!!!

# Proof

## Type Soundness
TODO: Double check the "is forget" stuff

## Public Noninterference
1. Pub stack is $$(sp+\delta, sp]$$, and sec stack is $$(sp+2\delta, sp+\delta]$$. Pay attention to the boundary!!!

## Pointer Problem
Currently, the type system is not perfect to build simulation relation.
This is because we want reg/slots with top type have consistent values before and after transformation.
However, pointer (which might be changed before/after transformation), can eventually have a top type, which is so bad!
We need a better way to represent pointer and non-pointer types.

## Should we mention transformed program's type?
In the current verion (of the paper), we do implies the type of the transformed program (especially when describing the transformation algorithm).
However, we did not implement type check of the transformed program.
Also, it would be nice if we can do the proof without mentioning the type of the transformed program.

## Limitation on how we handle pointers
1. We do not want to pass pointer in mem (like a struct)
2. We do not want the function to return a pointer in reg or in struct.
This prevents us from handling RSA, where the struct contains pointers...

One improvement:
Transformed pointers are only passed by reg, and never affect status of valid reg/slots in return states!

This statement is not clear enough. What I want to say is that, I want to assume all stack ptrs, that might need to be transformed, are not returned in valid reg/slots.
However for heap/static pointers that will be considered as "transformed ptr" but actually will not be changed before/after transformation, they are ok to be returned...
Too complicated...

## Range check

DONE: make sure call updates initialized region in the same way as range type infer (see above).


# Daily Task

## June 30
1. Fix memory layout of structures on stack - split them when needed (done)


## July 2
1. Finish updating memory overlappings structure for poly1305 
    1. Update memory type read/write, check whether I cover all cases
    2. Update memory type read/write in checker
2. Single infer, buf_used has some problem

## July 3
1. Continue updating memory overlappings structure for poly1305 (infer part is done)


## July 4
1. Continue updating memory overlappings structure for poly1305 (check) 
    1. Copy non overlap logic (2:00 - 2:40)
    2. Mem read write permission check and invalidate 
    2. PtrInfo subtype check

## July 6
1. Test new non-overlap impl! (done)


## July 7


1. Debug single infer for chacha20
    1. Sub to range may cause single infer fail
    2. Find sol other fail?


2. Document read/write at func call
3. Debug range infer for chacha20 (done)

4. Error when call with `poly1305_update(state, in - todo, todo);` -> need debug!
5. Analyze problems of current range infer for chacha20, make a plan (done)


## July 16
1. Check current changes and commit (done)
2. Add missing instructions for X25519 + test
3. Writing!
