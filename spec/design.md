# Stat 243 Final Project: Genetic Algorithms Design Doc

## `select`
The main function is `select(...)` located in the `select.R` file.

It will take arguments
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


It will take in arguments
* `type` - either `simple` as described in page 77 of Givens/Hoeting or `tournament` as descibed in page 81 of Givens/Hoeting.
* `parent1` and `parent2` - two candidate solutions.

