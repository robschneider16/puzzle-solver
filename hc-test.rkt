#lang racket

(printf "12,13,24...: ~a, and 2,5,19,...: ~a (reported -879673486) with secondary hash codes: ~a and ~a, respectively (reported 942)~%"
        (equal-hash-code #((12 23 24 29) (22) (5) (11) (9 20) (7 17) (2 13 19 21)))
        (equal-hash-code #((2 5 19 28) (21) (7) (9) (10 24) (16 22) (6 11 18 20)))
        (equal-secondary-hash-code #((12 23 24 29) (22) (5) (11) (9 20) (7 17) (2 13 19 21)))
        (equal-secondary-hash-code #((2 5 19 28) (21) (7) (9) (10 24) (16 22) (6 11 18 20))))
