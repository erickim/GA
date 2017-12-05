# Stat 243 Final Project: Genetic Algorithms Design Doc

## `select`
The main function is `select(...)` located in the `select.R` file.

It will take arguments:
* `data` - the data, should be of class `data.frame`.
* `formula` - the formula, should be of class `formula`.
* `family` - the type of regression, a string of `'lm'`, `'glm'`, and perhaps `'glmnet'`.
* `objective` - the objective function default `AIC` but can also allow for user implemented functions.
* `fitness` - similar to `objective` but can be different, see Givens/Hoeting for details.
* `selection` - the selection mechanism, probably a string, see Givens/Hoeting for details.
* `crossover` - the crossover rate.
* `mutation` - the mutation rate.
* `P` - the population size.
* `seed` - the seed to start the process, defaulted at `1`.

## `initialize`
There should be a function to initialize the algorithm.

It will take arguments as follows that were passed into `select`
* `P` - the population size.

## `crossover`
There should be a function to implement crossover.


It will take arguments:
* `type` - see page 77 of Givens/Hoeting.
* `parent1` and `parent2` - two candidate solutions.
* `rate` - the crossover rate passed into `select`.

## `mutation`
There should be a function ot implement mutation after crossover.

It will take arguments:
* `offspring` - an output from `crossover`.
* `rate` - the mutation rate passed into `select`.

## `selection`
There should be a function to implement selection and update the candidate solutions at each stage. It will call `crossover` and `mutation`.

It will take arguments:
* `type` - either `'simple'` as described in Page 76 of Givens/Hoeting or `'tournament'` as described in Page 81 of Givens/Hoeting.
* `parents` - all the candidate solutions
* ???

## `optimize`
This will be an iterative function that continues until convergence.

It will take arguments;
* ???

