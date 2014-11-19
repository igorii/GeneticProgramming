Genetic Programming
===================

This is the source for the genetic programming assignment for the COMP 5206 - Evolutionary Computation course at Carleton University.

There are three programs in this repository:

1. (`Q1.rkt`) A simple ant colony simulation
2. (`Q2.rkt`) Symbolic Regression using Genetic Programming
3. (`Q3.rkt`) Ant colony optimization using Genetic Programming

To run any of the questions, just run the corresponding file:

```
$ racket Q?.rkt
```

The final question will output the best performing ant behaviour program to an output directory as a scheme file, with a timestamp appended to the filename. Every 10 generations, the last best performer will be written to disk. These output files can be run with:

```
$ racket present.rkt GP-xxxx-xx-xxTxx-xx-xx.s
```

This will open a GUI and run the simulation in the same environment as the first question. These can be used to observe the progress of the GP as it runs, as well as to hold on to the best answer.

# The Generic GP 

The engine for the two genetic programming questions is `generic-gp.rkt`. 

To use the genetic programming engine, first create a function table and a terminal list. `generic-gp.rkt` provides a struct `fn` that holds a function name as a symbol, the function arity and a number, and an optional store for the corresponding procedure. If `R` is provided in the terminal list, it will be interpreted as an ephemeral random number.

```rkt
(define function-table (list (fn 'add 2 +) (fn 'sub 2 -) (fn 'sin 1 sin)))
(define terminals (list 'x 'R))
```

Since fitness can change dramatically, the user must also provide the fitness function, which is called with each individual in each generation.

```rkt
(define (fitness program-tree)
    (evaluate-tree program-tree))  ; User defined evaluate-tree - see Q2.rkt and Q3.rkt for exmaples
```

With these, call `generic-gp` with whichever arguments are required:

```rkt
(generic-gp
    #:population-size 100
    #:max-init-tree-height 6
    #:max-run-program-height 17
    #:function-table function-table
    #:terminals terminals
    #:mutation-rate 0.1
    #:fitness-fn fitness
    #:minimizing #t
    #:generations 50 ; (-1 for infinite)
    #:callback (lambda (best-fitness best-individual generation-number) (displayln best-individual))
```
