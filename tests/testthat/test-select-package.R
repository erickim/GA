## Utils Cross over. assertion function under select.R

## Select While loop if(i == p) break; 
# Flow Chart
# initialized: generate first generation. 
## Crossover 
## mutate input
## selection
# return parents idex 
## Fitness Rank 
## Regfunc 
## Example: 

## Unit Test Case:
library(testthat)

## Initialize
Y <- mtcars$mpg
X <- mtcars[2:11]
P = 2 * ncol(X)
regType = 'lm'
family = 'gaussian'
seed = 1

initialize(Y, X, P, regType, family, seed)

test_that("input is invalid",{
  # Y is not valid
  expect_error(initialize(Y = "a", X = X, P = P, regType = regType,
                          family = family, seed = seed))
  # P needs to be a numeric
  expect_error(initialize(Y = Y, X = X, P = "a", regType = regType,
                          family = family, seed = seed))
  # if bad regType, it will auto default to 'lm'
  expect_is(initialize(Y = Y, X = X, P = P, regType = 1,
                       family = family, seed = seed), "list")
  expect_is(initialize(Y = Y, X = X, P = P, regType = P,
                       family = 1, seed = seed), "list")
  expect_error(initialize(Y = Y, X = X, P = P, regType = regType,
                          family = family, seed = "a"))
  expect_equal(length(initialize(Y, X, P, regType, family, seed)),
               2 * ncol(X) )
}) 

## Selection
test_that("Test if the input of selection is valid", {
  expect_equal(selection("oneprop",  c("a","a","a","a","a")), NA)
  expect_equal(selection("oneprop",  2), NA)
  expect_equal(selection("twoprop", c(2,3, "w" , "w")), NA)
  expect_equal(selection("twoprop", 2), NA)
  
})

test_that("Test if the output of selection is valid", {
  expect_equal(length(selection("oneprop", c(0.9,1.5,3.3,2.2))), 2)
  expect_equal(length(selection("twoprop", c(0.9, 1.5, 3.3, 2.2))), 2)
  expect_equal(length(selection("hehehe", c(0.9, 1.5, 3.3, 2.2))), 2)
  expect_true(is.numeric(selection("oneprop", c(0.9, 1.5, 3.3, 2.2))))
  expect_true(is.numeric(selection("twoprop", c(0.9,0.6,0.1,1.1))))
})

## Crossover 
test_that("input is not valid", { 
  # incorrect inputs are caught
  expect_equal(crossover(c(2,0),1), NA)
  # invalid crossover type
  expect_error(crossover(1, 1, type = "abc"))
})
# create some fake data to test crossover
parent1 <- rbinom(20, 1, 0.5)
parent2 <- rbinom(20, 1, 0.5)
result1 <- crossover(parent1, parent2, type = "single", num_splits = 1)
result2 <- crossover(parent1, parent2, type = "multiple", num_splits = 5)
test_that("ouput is not expected", {
  # makes sure we get correct outputs for single splits
  expect_equal(length(result1$child1), 20)
  expect_equal(length(result1$child2), 20)
  expect_false(any(result1$child1 != 0 & result1$child1 != 1))
  expect_false(any(result1$child2 != 0 & result1$child2 != 1))
  # makes sure we get correct outputs for multiple splits
  expect_equal(length(result2$child1), 20)
  expect_equal(length(result2$child2), 20)
  expect_false(any(result2$child1 != 0 & result2$child1 != 1))
  expect_false(any(result2$child2 != 0 & result2$child2 != 1))
})

## Mutate
rate<-0.2
offspring<-c(1,1,1,1,0,0,0,0)

mutate(rate, offspring)

test_that("input is invalid",{ 
  expect_error(mutate("a", offspring))
  expect_error(mutate(2, offspring))
  expect_equal(length(mutate(rate, offspring)), length(offspring))
}) 

## Fitness Rank
fitness <- 1:10
fitnessRanks(fitness)

test_that("input is invalid",{ 
  expect_equal(fitnessRanks("a"), NA)
  expect_equal(length(fitnessRanks(fitness)),length(fitness))
}) 

## Regression Function
# use the Boston crime dataset to check that the regression function wrapper
# works as it should
Boston <- MASS::Boston
boston.crim = Boston$crim
test_that("Test if the output of regFunc is valid",{
  expect_equal(regFunc("lm",
                       "gaussian",
                       boston.crim ~.,
                       Boston[,-1])$Coefficients,
               lm(crim~., data = Boston)$Coefficients)
  expect_equal(regFunc("glm",
                       "gaussian",
                       boston.crim ~., Boston[,-1])$Coefficients,
               glm(crim~., data = Boston, family = "gaussian")$Coefficients)
})

# Select
library(leaps)
# do a best subsets selection first
bostSubsets <- regsubsets(crim ~ ., data = Boston, nvmax = 13)
bostSubsetsWhich <- summary(bostSubsets)$which
bostSubsetsAIC <- c()

for (i in 1:13) {
  bostSubsetsAIC <- c(bostSubsetsAIC,
                    AIC(glm(crim ~ .,
                            data = Boston[c(TRUE,
                                          bostSubsetsWhich[i,-1])])))
}

bostGA <- select(Boston$crim, Boston[,-1], regType = "lm")
min(bostSubsetsAIC)

test_that("`select` comes close to the best subset selection", {
  expect_equal(-1*bostGA$fitness, min(bostSubsetsAIC),
               tolerance = min(bostSubsetsAIC/1e5))
})
