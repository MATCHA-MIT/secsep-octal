# Implementation

1. Valid reg check
2. isNonChangeExp check
   1. Branch condition
   2. Call ptr mapped value check
   3. Load/stor addr - base ptr
   4. Load value is top: the slot dep type should be non change exp
   5. Store outcome is top: the original data and store data should be non change exp
   6. Subtype: if sup type is top, then sub exp should be non change exp
   7. Return state (valid reg, valid region of mem slot)
3. Br anno check
   1. No input var
   2. No taint var
   3. No map to top
4. Call check
   1. Rsp type check
   2. Return slot check
   3. Ptr call anno map check (isNonChangeExp)
   4. Call anno check: no top
   5. Ret anno check: map to fresh variable
   6. Is Spill check
   7. Slot map get ptr check - somehow need to define get ptr
   8. udpateMem check (assertions in the algorithm)
5. Prog sanity check (e.g., do not jump to random places)
6. Return slot type check (either remove it from memory type of check its type is not changed!) -- NO!!! Must remove it from memory type and make sure it is empty, otherwise my proof still needs to reason about the simulation relation between return pointers. But should still ensure non-overflow property (generated under the case where the slot is there).
7. Valid region of local stack slots are empty in the beginning and at the end of the function
8. Callee-saved reg check (same before and after executing the function)
9. Type constraints at the function input should be included by other blocks. (Why do we need this?)
10. Check well-formedness of initial state
11. Transformation:
    1.  TransStrategy assertions
    2.  Compiler Ptr assertions
12. Check sim rel of initial states


## Dep Type Infer/Check

1. Single infer does not need to generate range/taint constraints (optimization)
2. How to handle declassification?
3. How to handle pointer in branch condition (used in proof)?
4. add_var_bound_constraint???

## Range Type Infer/Check

1. Check how to update valid region after func call (should be aligned with updateMem in the paper)

## Other

1. What do we need to do to update mem content + permission?
2. For branch anno, needs to check that the branch context substitute map does not substitute input variables or taint variables (done for taint variables, still needs to be done for input dep variables).
3. We also need to check that the map does not substitute var to top!!!
4. Double check the "is forget" stuff
5. Check whether the concerete setup is satisfiable
6. isNonChangeExp (pointer problem)
7. Transformed pointers are only passed by reg, and never affect status of valid reg/slots in return states! (Check transformation)

# Writing

1. Check whether I over-claimed something in inference-related writing.
2. How to deal with multi-block loop or subtype relation is a very important topic. I should definitely mention it in the paper orzzz.
3. Check writing for range type infer against my new notes.
4. Mention that we support overlapped input slots.
5. Explain more about whether each constraint/assumption is reasonable
6. Typing-Func: I may not need isLocalStack slot is empty at the beginning and end of the func.
7. Sim-Mem: consider to remove isLocalStack \/ tau=0,1