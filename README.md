puzzle-solver
=============

Research code for exploring parallel search algorithms
applied to sliding-tile puzzles that give rise to
extremely large search spaces.

To date, implemented search methods include Breadth-first Search,
Fringe Breadth-first Search, and A\*.

Parallelism has been tested on a 32-node cluster
using the [Riot planet package](http://planet.racket-lang.org/display.ss?package=riot.plt&owner=gcr).

Running
-------

Run these commands in sequence:

* racket -p gcr/riot/server
* racket -p gcr/riot/worker -- localhost
* drracket stp-solve-cluster.rkt
