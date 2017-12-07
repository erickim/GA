source("./R/select.R")

### EXAMPLES: DATA GENERATION ###
regType = 'glm'
family = 'gaussian'
fitness = "AIC"
ranked = TRUE
selectionType = "twoprop"
elitism = TRUE
crossoverType = "single"
numCrossover = NA
mutationRate = .01
maxIter = 100
seed = 1

### TEST ###
set.seed(1)
Y <- rnorm(100)
X <- as.data.frame(matrix(rnorm(100*26), nrow = 100))
names(X) <- letters
P <- ncol(X)*2

### BEST SUBSETS VS GA WITH TEST ###
library(leaps)
testBest <- regsubsets(Y ~ ., data = X, nvmax = 26)
testBestWhich <- summary(testBest)$which
testBestAIC <- c()
for (i in 1:26) {
  testBestAIC <- c(testBestAIC,
                   AIC(lm(Y ~ .,
                          data = X[testBestWhich[i,-1]])))
}

testGASelect <- select(Y, X)

# optimized AIC
-1*testGASelect$fitness # GA
min(testBestAIC) # best subsets
# fit
testGASelect$variables
testBestWhich[which.min(testBestAIC),]

### BEST SUBSETS VS GA WITH `swiss` ###
# best subsets
swissBest <- regsubsets(Fertility ~ ., data = swiss, nvmax = 5)
swissBestWhich <- summary(swissBest)$which
swissBestAIC <- c()

for (i in 1:5) {
  swissBestAIC <- c(swissBestAIC,
                    AIC(lm(Fertility ~ .,
                           data = swiss[c(TRUE,
                                          swissBestWhich[i,-1])])))
}

swissGASelect <- select(swiss$Fertility, swiss[,-1])

AIC(swissGASelect$fit)
min(swissBestAIC)


##### MODULAR EXAMPLES #####

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