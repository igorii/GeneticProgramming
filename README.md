Genetic Programming
===================

This is the source for the genetic programming assignment for the COMP 5206 - Evolutionary Computation course at Carleton University.

There are three programs in this repository:

1) (`Q1.rkt`) A simple ant colony simulation
2) (`Q2.rkt`) Symbolic Regression using Genetic Programming
3) (`Q3.rkt`) Ant colony optimization using Genetic Programming

To run any of the questions, just run the corresponding file:

```
$ racket Q?.rkt
```

The final question will output the best performing ant behaviour program to an output directory as a scheme file, with a timestamp appended to the filename. Every 10 generations, the last best performer will be written to disk. These output files can be run with:

```
$ racket present.rkt GP-xxxx-xx-xxTxx-xx-xx.s
```

This will open a GUI and run the simulation in the same environment as the first question. These can be used to observe the progress of the GP as it runs, as well as to hold on to the best answer.
