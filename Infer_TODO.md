# Dependent Type Inference

## Big TODO
1. Implement block invaraince inference
2. Support bit vector for single dep types.
3. Support write to multiple entries.


## Current Small Tasks
1. Double check which replace function should be used for repl block vars.

## Problem:
1. During block invariance inference, consider block A (branch x, y, z) -> block B.
    Suppose B needs statement S to be true, but x&y&z cannot imply S.
    We would conservatively infer at the beginning of A there is S.
    But it is possible that S is not invariance of A. 
    Instead, A only holds a weaker invariance S' such that S'&x&y&z => S.
    The most accurate statement is add S|(!(x&y&z)) to A's context/invariance.
    We use the conservative for faster inference.
2. How to handle the case where A jumps to A? -> The extra assertion list should be empty.
