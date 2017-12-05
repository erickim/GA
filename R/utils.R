###############################################################################
#                                                                             #
#                    Genetic Algorithms Utility Functions                     #
#                                                                             #
###############################################################################

initialize <- function(Y, X, P) {
  init_vars <- lapply(1:P, function(x) rbinom(ncol(X), 1, .5))
  
  init_pop <- lapply(init_vars, function(x) {
    lm(Y ~ ., data = X[, as.logical(x)])
  })
  
  return(list(variables = init_vars, fit = init_pop))
}

crossover <- function(type, parent1, parent2) {
  N <- 
}
