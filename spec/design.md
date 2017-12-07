# Stat 243 Final Project: Genetic Algorithms Design Doc

## `select`
The main function is `select(...)` located in the `select.R` file.

It will take arguments:
* `data` - the data, should be of class `data.frame`.
* `reg_type` - the type of regression, a string of `'lm'`, `'glm'`, and perhaps `'glmnet'`.
* `family` - the family to be passed into `'glm'` or perhaps `'glmnet'`.
* `objective` - the objective function default `AIC` but can also allow for user implemented functions.
* `fitness` - similar to `objective` but can be different, see Givens/Hoeting for details.
* `selection` - the selection mechanism, probably a string, see Givens/Hoeting for details.
* `crossover` - the crossover type.
* `mutation` - the mutation rate.
* `P` - the population size.
* `seed` - the seed to start the process, defaulted at `1`.

## `initialize`
There should be a function to initialize the algorithm.

It will take arguments as follows that were passed into `select`
* `Y` - the response vector.
* `X` - the feature matrix
* `P` - the population size.

## `crossover`
There should be a function to implement crossover.

It will take arguments:
* `parent1` and `parent2` - two candidate solutions (a vector of 0's and 1's).
* `type` - see page 77 of Givens/Hoeting (either `single` or `multiple`)
* `num_splits` - if `type` was multiple, then the user supplied number of splits. Will default to 2 if invalid `num_splits` passed.

## `mutate`
There should be a function ot implement mutation after crossover.

It will take arguments:
* `offspring` - an output from `crossover`.
* `rate` - the mutation rate passed into `select`.

## `selection`
There should be a function to implement selection and update the candidate solutions at each stage. It will call `crossover` and `mutation`.

It will take arguments:
* `type` - either `'oneprop'` or `'twoprop'` as described in Page 76 of Givens/Hoeting
* `pop_fitness` - the candidate fitness scores


