# Stat 243 Final Project: Genetic Algorithms Design Doc

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
