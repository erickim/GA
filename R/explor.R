source("./R/utils.R")

### EXAMPLES ###
set.seed(1)

Y <- rnorm(100)
X <- as.data.frame(matrix(rnorm(100*26), nrow = 100))
names(X) <- letters
P <- ncol(X)*2

init_pop <- initialize(Y, X, P)
crossoverSingle <- crossover(init_pop[[1]]$variables,
                             init_pop[[2]]$variables,
                             "single")
# $child1
# [1] 0 1 1 1 1 1 0 0 0 1 0 1 1 0 1 0 0 0 1 0 0 1 1 1 0 1
# $child2
# [1] 1 1 1 0 1 0 0 1 1 1 0 0 1 0 1 1 1 1 0 1 0 0 0 1 1 0
crossoverMultiple <- crossover(init_pop[[1]]$variables,
                               init_pop[[2]]$variables,
                               "multiple",
                               3)
#$child1
#[1] 0 1 1 0 1 0 0 1 1 1 0 1 1 0 1 1 1 0 1 0 0 1 1 1 0 1
#$child2
#[1] 1 1 1 1 1 1 0 0 0 1 0 0 1 0 1 0 0 1 0 1 0 0 0 1 1 0


# ignore below
#xaic <- lapply(x, AIC)