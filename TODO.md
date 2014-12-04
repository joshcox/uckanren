# TODO List for Parallel MicroKanren

* Create Places farm
** Each Place is in a worker loop
*** Pull work item from channel, evaluate in intepreter
*** How to send it?
**** pdisj :: (g-exp, g-exp) -> (s/c -> __Stream or Future-Id?__)
***** pdisj will send work (pairof g-exp s/c) to work queue (channel)

* Move from Stream representation to some other rep (ex. in PLDI paper from Ryan)
** Continuations represent work to do on an environment (or state)

* Optimize the input program
** Where are recursions?
** Conjunction pipeline? What variables are used in multiple, etc?
