source("./R/utils.R")

### EXAMPLES: DATA GENERATION ###
set.seed(1)

Y <- rnorm(100)
X <- as.data.frame(matrix(rnorm(100*26), nrow = 100))
names(X) <- letters
P <- ncol(X)*2

### EXAMPLES: INITIALIZE ###
initPop <- initialize(Y, X, P)

### EXAMPLES: CROSSOVER ###
crossoverSingle <- crossover(initPop[[1]]$variables,
                             initPop[[2]]$variables,
                             "single")
# $child1
# [1] 0 1 1 1 1 1 0 0 0 1 0 1 1 0 1 0 0 0 1 0 0 1 1 1 0 1
# $child2
# [1] 1 1 1 0 1 0 0 1 1 1 0 0 1 0 1 1 1 1 0 1 0 0 0 1 1 0
crossoverMultiple <- crossover(initPop[[1]]$variables,
                               initPop[[2]]$variables,
                               "multiple",
                               3)
# $child1
# [1] 0 1 1 0 1 0 0 1 1 1 0 1 1 0 1 1 1 0 1 0 0 1 1 1 0 1
# $child2
# [1] 1 1 1 1 1 1 0 0 0 1 0 0 1 0 1 0 0 1 0 1 0 0 0 1 1 0

### EXAMPLES: MUTATION ###
crossoverSingleMutation <- mutate(rate = .05,
                                  offspring = crossoverSingle$child1)
# [1] 0 1 1 1 1 1 0 0 0 1 0 1 1 0 1 0 0 0 1 0 0 1 1 1 0 1

### EXAMPLES: SELECTION ###
initPopAIC <- unlist(lapply(initPop, function(x) AIC(x$fit)))
selection('oneprop', initPopAIC)
# 8 7
selection('twoprop', initPopAIC)
# 34, 11

### EXAMPLES: TOURNAMENT SELECTION ###
# unimplemented, do later 

# ignore below
#xaic <- lapply(x, AIC)