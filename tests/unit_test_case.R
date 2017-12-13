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




boston.crim = Boston$crim
test_that("Test if the output of regFunc is valid",{
  expect_equal(regFunc("lm", "gaussian", boston.crim ~., Boston[,-1])$Coefficients, lm(crim~., 
                                                                                       data = Boston)$Coefficients)
  expect_equal(regFunc("glm", "gaussian", boston.crim ~., Boston[,-1])$Coefficients, glm(crim~., 
                                                                                       data = Boston, family = "gaussian")$Coefficients)
})





