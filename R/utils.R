###############################################################################
#                                                                             #
#                    Genetic Algorithms Utility Functions                     #
#                                                                             #
###############################################################################

library(roxygen2)
library(docstring)

### The following docstrings don't work with help(...) because we don't have
### a .Rd file yet, but it should work with ?... to access the docstrings
### from the global environment.

initialize <- function(Y, X, P) {
  #' Genetic Algorithms Initial Population Creation
  #'
  #' Arguments:
  #' @param Y The response vector as passed into select.
  #' @param X The feature matrix as passed into select.
  #' @param P The size of the population as passed or determined in select.
  #'  
  #' @return A list of the P generated candidate solutions
  #' where each entry of the list contains
  #' \itemize{
  #' \item \code{variables}: The variables that were
  #' selected for the population.
  #' \item \code{fit}: The \code{lm} or \code{glm} object of the fit with the
  #' above variables.
  #' }
  
  init_pop <- lapply(1:P, function(x) {
    init_var <- rbinom(ncol(X), 1, .5)
    init_sol <- lm(Y ~ ., data = X[,as.logical(init_var)])
    return(list(variables = init_var, fit = init_sol))
    })
  
  return(init_pop)
}

crossover <- function(parent1,
                      parent2,
                      type = "single",
                      num_splits = NA) { 
  #' Genetic Algorithms Crossover Operation
  #'
  #' Arguments:
  #' @param type The type of crossover, either "single" or "multiple".
  #' @param parent1 The \code{variables} output from a parent.
  #' @param parent2 The \code{variables} output from another parent.
  #'  
  #' @return A list of vectors of the children to return.
  
  N <- length(parent1)
  
  if (type == "single"){
    split <- sample(N, 1)
    
    child1 <- c(parent1[1:split], parent2[(split+1):N])
    child2 <- c(parent2[1:split], parent1[(split+1):N])
  } else if (type == "multiple") {
    if (num_splits < 2 | is.na(num_splits)) num_splits = 2
    
    splits <- sort(sample(N, num_splits))
    parent1Split <- list()
    parent2Split <- list()
    
    # first split
    parent1Split[[1]] <- parent1[1:splits[1]]
    parent2Split[[1]] <- parent2[1:splits[1]]
    # middle splits
    for (i in 2:num_splits) {
      parent1Split[[i]] <- parent1[(splits[i - 1] + 1):splits[i]]
      parent2Split[[i]] <- parent2[(splits[i - 1] + 1):splits[i]]
    }
    # last split
    parent1Split[[num_splits + 1]] <- parent1[(splits[num_splits] + 1):N]
    parent2Split[[num_splits + 1]] <- parent2[(splits[num_splits] + 1):N]
    
    child1 <- c()
    child2 <- c()
    
    for (i in 1:(num_splits + 1)) {
      if (i %% 2 == 1) {
        child1 <- c(child1, parent1Split[[i]])
        child2 <- c(child2, parent2Split[[i]])
      } else {
        child1 <- c(child1, parent2Split[[i]])
        child2 <- c(child2, parent1Split[[i]])
      }
    }
  }
  
  children <- list(child1 = child1, child2 = child2)
  return(children)
}
