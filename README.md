# Parallel Search in Sliding-tile Puzzles

Research code for exploring parallel search algorithms
applied to sliding-tile puzzles that give rise to
extremely large search spaces.

To date, implemented search methods include Breadth-first Search,
Fringe Breadth-first Search, and A\*.

Parallelism has been tested on a 32-node cluster
using the [Riot planet package](http://planet.racket-lang.org/display.ss?package=riot.plt&owner=gcr).

## Overview

- stp-init.rkt (global parameters, data-definitions,
functions for converting between formats, puzzle initialization data, etc)
- stp-solve-base.rkt (functions that are common to any search method,
whether run on a single computer or on a cluster)
- stp-fringefilerep.rkt (file and I/O functions
for reading and writing fringes from and to files)
- stp-solve-cluster.rkt
(the main file with code for distributing the search across multiple processes;
this is also where you set which puzzle you want to solve --
climb12 is the only one you want to try --
and the maximum depth you're willing to search to)
- (stp-spaceindex.rkt is also used and plays an important role,
but is pretty complex so you should ignore it initially)
- the folder stpconfigs contains configuration files for global variables 
whose settings depend on whether you are running on a cluster or a single machine
(although maybe with multiple cores).
once you run the software, this folder will also contain an 'index' file
for each puzzle that you run.
- the folder riottest contains examples of using the riot package
which is our distribution engine, used for farming out tasks to available processors
- the folder fringefiles will contain stored fringes from searches

## Running

The various processes in the shells below must be run in the source folder; 
for example, `/home/username/puzzle-solver`.
Before starting, you must also create `./stpconfigs/configenv.rkt`.
Usually, you can simply create a symbolic link to `./stpconfigs/configenvlocal.rkt`
but you can create a copy and edit it to your particular situation.

1. In one shell, run `racket -p gcr/riot/server`
2. For each worker process (e.g., the number of available cores), 
open a shell and run `racket -p gcr/riot/worker -- localhost` 
or substitute the hostname of the server process in step 1
if you're using different hosts for workers
3. You can confirm that the workers have registered with the server
by inspecting the output from the shell in step 1.
4. Finally, run `racket stp-solve-cluster.rkt` and watch it churn through the search.
