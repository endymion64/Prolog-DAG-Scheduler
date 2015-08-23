# Prolog-DAG-Scheduler

A DAG scheduler written in SWI-Prolog.
This project was made for my Declarative Programming course.
It allows the user to schedule tasks in parallel on heterogeneous architectures.
Tasks are represented as Directed Acyclic Graphs (DAGs) where vertices represent (atomic) sub-tasks and the edges represent data dependencies between the sub-tasks.
The project contains an optimal and a heuristic scheduler.

## Requirements

SWI-prolog

## Use
Running the project can be done by loading an instance file and the main module in a new SWI-prolog session.

From the source folder the following command allows this:
```
swipl ../data/<file> main.pl
```
## Provided commands
* `find_optimal(-Solution)`
  <br>
  Searches the optimal parallel execution schedule, minimizing execution time.
  <br>
  **IMPORTANT**: use this only on small DAGs, since this is NP-hard!
* `find_heuristically(-Solution)`
  <br>
  Searches for an execution schedule, approximately miniminzing execution time.
  This done via the HEFT algorithm.
* `pretty_print(+Solution)`
  <br>
  Prints a found solution in a readable fashion
* `speedup(+Solution,-Result)`
  <br>
  Calculates the speedup of a found solution.
* `execution_time(+Solution,-Result)`
  <br>
  Calculates the execution time of a founs solution.
