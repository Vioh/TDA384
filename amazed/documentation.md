# Datastructure

The only state that needs to be shared between the threads/solvers is the set of visited nodes,
and thus we need to use ConcurrentSkipListSet in order to be thread-safe.

The map of predecessors doesn't need to be shared, because a solver only needs to reconstruct
the path that it has traversed through. Other nodes visited by other solvers are irrelevant.

The frontier also doesn't need to be shared, since we can simply assume that the frontier
represents the work to be done by a single solver.

# Forking mechanism

Main idea is to fork new solvers only if there are more than one un-visited neighbors. The number
of newly forked solvers is always equal to the number of un-visited neighbors minus 1, since the
current solver will take care of one of the neighbors (for efficiency).

All the forked solvers will be stored in a list, so that we can join them later. If there are no
un-visited neighbors, we simply continue with the next node in the frontier.

# Joining mechanism

Joining is to make sure all solvers terminate properly. It should be done whenever the current
solver has nothing left to do. This happens on 2 occasions. Either when the current solver has
found a solution, or when the frontier is empty.

In order to join, we simply loop through all solvers that have been spawned from the current
solver, and call the join() method. If one of these solvers has found a solution, we simply
return that solution. See the method ForkJoinSolver.waitForOtherSolvers().

# Early stopping

If a solver has found a goal, it has to communicate this to all other threads/solvers, so that
they can terminate as soon as possible. This is done by using the static variable "found", which
is initialized to "false" when the program starts.

This variable is read whenever a solver is processing a new node from the frontier. It doesn't
need any synchronization mechanism, because our intention is only to write "true" to it. So even
if multiple threads write to it at the same time, we won't have data races issues.

# Path reconstruction

If a child solver has found a goal, it will return the path from the child's start node to the
goal node. The parent solver will have to find the path from the parent's start node to the
child's start node, and then join the 2 paths together.
