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

## Initialize
Y <- mtcars$mpg
X <- mtcars[2:11]
P = 2 * ncol(X)
regType = 'lm'
family = 'gaussian'
seed = 1

initialize(Y, X, P, regType, family, seed)

test_that("input is invalid",{ 
  expect_error(initialize("a",X,P,regType,family,seed))
  expect_error(initialize(Y,X,"a",regType,family,seed))
  expect_error(initialize(Y,X,P,1,family,seed))
  expect_error(initialize(Y,X,P,P,1,seed))
  expect_error(initialize(Y,X,P,regType,family,"a"))
  expect_equal(length(initialize(Y,X,P,regType,family,seed)), 2 * ncol(X) )
}) 

## Selection
test_that("Test if the input of selection is valid", {
  expect_error(selection("oneprop",  c("a","a","a","a","a")))
  expect_error(selection("oneprop",  2))
  expect_error(selection("twoprop", c(2,3, "w" , "w")))
  expect_error(selection("twoprop", 2))
  
})

test_that("Test if the output of selection is valid", {
  expect_equal(length(selection("oneprop", c(0.9,1.5,3.3,2.2))), 2)
  expect_equal(length(selection("twoprop", c(0.9, 1.5, 3.3, 2.2))), 2)
  expect_equal(length(selection("hehehe", c(0.9, 1.5, 3.3, 2.2))), 2)
  expect_true(is.numeric(selection("oneprop", c(0.9, 1.5, 3.3, 2.2))))
  expect_true(is.numeric(selection("twoprop", c(0.9,0.6,0.1,1.1))))
})

## Cross Over 
test_that("input is not valid", { 
  expect_error(crossover(c(2,0),1))
  expect_error(crossover(1,a))
  expect_error(crossover(1,1,Type = "abc"))
  expect_error(crossover(1,1,num_splits = "-1"))
})
parent1 <- rbinom(20,1,0.5)
parent2 <- rbinom(20,1,0.5)
result1 <- crossover(parent1,parent2,type = "single", num_splits = 1)
result2 <- crossover(parent1,parent2,type = "multiple", num_splits = 5)
test_that("ouput is not expected", {
  expect_equal(length(result1$child1), 20)
  expect_false(!any(result1$child1 != 0 & result1$child1 !=1))
  expect_equal(length(result2$child2), 20)
  expect_false(!any(result2$child2 != 0 & result1$child2 !=1))
})



##Mutate
rate<-0.2
offspring<-c(1,1,1,1,0,0,0,0)

mutate(rate,offspring)

test_that("input is invalid",{ 
  expect_error(mutate("a",offspring))
  expect_error(mutate(rate,c(1,2,3)))
  expect_error(mutate(2,offspring))
  expect_equal(length(mutate(rate,offspring)),length(offspring))
}) 


## fitness rank
fitness<-1:10
fitnessRanks(fitness)

test_that("input is invalid",{ 
  expect_error(fitnessRanks("a"))
  expect_equal(length(fitnessRanks(fitness)),length(fitness))
}) 


## Regfunc
boston.crim = Boston$crim
test_that("Test if the output of regFunc is valid",{
  expect_equal(regFunc("lm", "gaussian", boston.crim ~., Boston[,-1])$Coefficients, lm(crim~., 
                                                                                       data = Boston)$Coefficients)
  expect_equal(regFunc("glm", "gaussian", boston.crim ~., Boston[,-1])$Coefficients, glm(crim~., 
                                                                                       data = Boston, family = "gaussian")$Coefficients)
})





