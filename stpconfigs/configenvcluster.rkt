#lang racket

(provide (all-defined-out))


#| Run Environment Configuration for CLUSTER

Provide defining values for the following global variables:
  *master-name*  : typically either "localhost" or "wcp"
  *local-store*  : where to write partial expansions, e.g., "/state/partition1/fringefiles/" on cluster
  *share-store*  : where to find current, prev, and proto fringes and where to write segments, e.g., "/share/data2/fringefiles/" on cluster
  *n-slices* : Divide the search space into this many parallelizable slices
  *late-duplicate-removal* : (boolean) remove duplicates after [after what again?]

Create a link named "configenv.rkt" to this file if on cluster

|#

(define *master-name* "wcp")
(define *local-store* "/state/partition1/fringefiles/")
(define *share-store* "/share/data2/fringefiles/")
(define *n-slices* 34)
(define *late-duplicate-removal* #f)

