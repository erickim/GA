###############################################################################
#                                                                             #
#                    Genetic Algorithms Utility Functions                     #
#                                                                             #
###############################################################################

library(roxygen2)
library(docstring)

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

crossover <- function(type, parent1, parent2) { 
  #' Genetic Algorithms Crossover Operation
  #'
  #' Arguments:
  #' @param type The type of crossover, either 
  #' @param parent1 The \code{variables} output from a parent.
  #' @param parent2 The \code{variables} output from another parent.
  #'  
  #' @return A list of vectors of the children to return.
  
  N <- length(parent1)
}
