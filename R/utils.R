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

initialize <- function(Y, X, P, regType, family, seed) {
  #' Genetic Algorithms Initial Population Creation
  #'
  #' Arguments:
  #' @param Y The response vector as passed into select.
  #' @param X The feature matrix as passed into select.
  #' @param P The size of the population as passed or determined in select.
  #' @param regType The type of regression, either "lm" or "glm".
  #' @param family The family for "glm".
  #' @param seed The seed for reproducibility.
  #'  
  #' @return A list of the P generated candidate solutions
  #' where each entry of the list contains
  #' \itemize{
  #' \item \code{variables}: The variables that were
  #' selected for the population.
  #' \item \code{fit}: The \code{lm} or \code{glm} object of the fit with the
  #' above variables.
  #' }
  
  set.seed(seed)
  
  init_pop <- lapply(1:P, function(x) {
  init_var <- rbinom(ncol(X), 1, .5)
  init_sol <- regFunc(regType,
                      family,
                      Y ~ .,
                      data = X[,as.logical(init_var), drop = FALSE])
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
  #' @param parent1 The first candidate solution (a vector of 0's and 1's).
  #' @param parent2 The second candidate solution (a vector of 0's and 1's).
  #' @param type The type of crossover, either "single" or "multiple".
  #' @param num_splits The number of splits for type multiple.
  #' If invalid \code{num_splits} specified, then will default to 2.
  #'  
  #' @return A list of vectors of the children to return.
  
  N <- length(parent1)
  
  if (type == "single"){
    # find where to split
    split <- sample(N, 1)
    
    # create the new children
    if (split == N) {
      child1 <- parent2
      child2 <- parent1
    } else{
      child1 <- c(parent1[1:split], parent2[(split+1):N])
      child2 <- c(parent2[1:split], parent1[(split+1):N])
    }
  } else if (type == "multiple") {
    # if invalid num_splits supplied, set default 2
    if (num_splits < 2 | is.na(num_splits)) {
      message('Invalid `num_splits` provided, defaulting to 2.')
      num_splits = 2
    }
    
    # find the places to split
    splits <- sort(sample(N, num_splits))
    
    # create empty list of splits of parents
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
    
    # create empty children vectors
    child1 <- c()
    child2 <- c()
    
    # alternate parent splits and append to children vectors
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

mutate <- function(rate, offspring) {
  #' Genetic Algorithms Mutation Operation
  #'
  #' Arguments:
  #' @param rate The rate of mutation.
  #' @param offspring The candidate solution (a vector of 0's and 1's)
  #' to potentially mutate.
  #'  
  #' @return The mutated or unmutated candidate.
  
  P <- length(offspring)
  
  # sample from a bernoulli(rate).
  # equals 1 if mutation will occur 0 otherwise
  mutatePos <- rbinom(P, 1, rate)
  
  offspring[as.logical(mutatePos)] <-
    (offspring[as.logical(mutatePos)] + 1) %% 2
  
  return(offspring)
}

selection <- function(type, pop_fitness) {
  #' Genetic Algorithms Selection Operation
  #'
  #' Arguments:
  #' @param type The type of selection mechanism. Either \code{'oneprop'},
  #' \code{'twoprop'}. See page 76 and 81 of G/H.
  #' @param pop_fitness A vector of fitness values for the population
  #'  
  #' @return The mutated or unmutated candidate.
  
  if (!type %in% c('oneprop', 'twoprop', 'tournament')) {
    message('Invalid selection mechanism, defaulting to `twoprop`.')
    type <- 'twoprop'
  }
  
  P <- length(pop_fitness)
  fitnessProbs <- pop_fitness / sum(pop_fitness)
  
  if (type == 'oneprop') {
    # pick first proportional to fitness
    toSelect <- sample(1:P, 1, prob = fitnessProbs)
    # pick second parent at random from the remaining
    toSelect <- c(toSelect,
                  sample((1:P)[-toSelect], 1))
    
  } else if (type == 'twoprop') {
    toSelect <- sample(1:P, 2, prob = fitnessProbs)
  }
  
  return(toSelect)
  
}

selection_tournament <- function(pop_fitness, k, G) {
  #' Genetic Algorithms Tournament Selection Operation
  #' See Page 81 of G/H.
  #'
  #' Arguments:
  #' @param pop_fitness A vector of fitness values for the population
  #' @param k The number of subsets
  #' @param G The proportion of offspring to be replaced by generated offspring
  #'  
  #' @return The mutated or unmutated candidate.
  
  # unimplemented
}

fitnessRanks <- function(fitness) {
  #' Genetic Algorithms Fitness Rank Operation
  #' See Page 80 of G/H.
  #'
  #' Arguments:
  #' @param fitness
  #'  
  #' @return 
  
  P <- length(fitness)
  fitnessRanks <- rank(fitness)
  
  newFitness <- 2*fitnessRanks / (P * (P+1))
  
  return(newFitness)
}

regFunc <- function(regType, family, formula, data) {
  if (!regType %in% c("lm", "glm")) {
    message("Invalid `regType` defaulting to `lm`.")
    regType = "lm"
  }
  
  if (!family %in% c("binomial", "gaussian", "Gamma",
                     "inverse.gaussian", "poisson", "quasi",
                     "quasibinomial", "quasipoisson")) {
    message("Invalid `family` defaulting to `gaussian`.")
  }
  
  if (regType == "lm" & ncol(data) == 0) {
    return(lm(formula))
  } else if (regType == "lm" & ncol(data) > 0) {
    return(lm(formula, data))
  } else if (regType == "glm" & ncol(data) == 0) {
    return(glm(formula, family))
  } else if (regType == "glm" & ncol(data) > 0) {
    return(glm(formula, family, data))
  }
}
