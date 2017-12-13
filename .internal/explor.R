# eric's tests while implementing

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
                   AIC(glm(Y ~ .,
                          data = X[testBestWhich[i,-1]])))
}

testGASelect <- select(Y, X, regType = "glm")

# optimized AIC
-1*testGASelect$fitness # GA
min(testBestAIC) # best subsets
# fit (same)
which(testGASelect$variables == 1)
which(testBestWhich[which.min(testBestAIC),-1])

### BEST SUBSETS VS GA WITH `swiss` ###
# best subsets
swissBest <- regsubsets(Fertility ~ ., data = swiss, nvmax = 5)
swissBestWhich <- summary(swissBest)$which
swissBestAIC <- c()

for (i in 1:5) {
  swissBestAIC <- c(swissBestAIC,
                    AIC(glm(Fertility ~ .,
                           data = swiss[c(TRUE,
                                          swissBestWhich[i,-1])])))
}

swissGASelect <- select(swiss$Fertility, swiss[,-1], regType = "glm")

# optimized AIC
-1*swissGASelect$fitness # GA
min(swissBestAIC) # best subsets
# fit (same)
which(swissGASelect$variables == 1)
which(swissBestWhich[which.min(swissBestAIC),-1])


##################### james test
set.seed(1)
n <- 500
C <- 40
X <- as.data.frame(matrix(rnorm(n * C), nrow = n))
beta <- c(88, 0.1, 123, 4563, 1.23, 20)
y <- as.matrix(X[ ,1:6]) %*% beta
colnames(X) <- c(paste("real", 1:6, sep = ""),
                 paste("noi", 1:34, sep = ""))

jamesGATest <- select(y, X, "glm", "gaussian")

jamesBest <- regsubsets(y ~ ., data = X, nvmax = 40)
jamesBestWhich <- summary(jamesBest)$which
jamesBestAIC <- c()

for (i in 1:40) {
  jamesBestAIC <- c(jamesBestAIC,
                    AIC(glm(y ~ .,
                            data = X[jamesBestWhich[i,-1]])))
}

min(jamesBestAIC)
jamesBestWhich[which.min(jamesBestAIC),]
#####################


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