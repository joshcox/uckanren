# ucKanren

## What are we looking to do?

Well, I'd like to start from scratch. Take the microKanren represented
in the ICFP paper (which can be found at: https://github.com/jasonhemann/microKanren) and extend it. 

### Let's consider a constraint: (== a b)

Q: What's the op?
A: ==

Q: What are the neighbors (or cells)?
A: a,b
   Well, not necessarily. Our "cells" a and b may actually
   be a member of an equivalence class where the root is the
   actual cell

## What am I doing?

### Starting with the cells module.

What is this? A straightforward modification of the current
state structure. We can keep much of the same structure as-is
and extend it to include a package of information as opposed to
only a value at the payload level.

This structure represents our set of cells, which contain:

* A Value
* A Domain (when needed)
* A Constraint Store

Each time a user runs a constraint (like unification above),
ucKanren is going to extend the constraint stores of the 
applicable cells with the constraint and run it. 

*Relevant Question No. 1 ~ __below__*

### More to come soon.


## Relevant Questions
1. Should I be running all constraints or just the **current** 
   constraint? The answer lies within _"The Art of the Propagator."_
