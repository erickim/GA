###############################################################################
#                                                                             #
#                          Genetic Algorithms Main                            #
#                                                                             #
###############################################################################

select <- function(Y,
                   X,
                   regType = 'lm',
                   family = 'gaussian',
                   fitness = 'AIC',
                   ranked = TRUE, 
                   selectionType = 'twoprop', 
                   elitism = TRUE,
                   crossoverType = 'single', 
                   numCrossover = NA,
                   mutationRate = .01, 
                   maxIter = 100, 
                   P = 2 * ncol(X), 
                   seed = 1,
                   VERBOSE = FALSE) {
  #' Genetic Algorithms for Variable Selection in Regression
  #'
  #' @description Performs the genetic algorithm for regression with the 
  #' specified arguments and fitness function to return the optimal
  #' solution to the regression problem.
  #'
  #' @param Y The response vector.
  #' @param X The feature matrix.
  #' @param regType The model of an appropriate class ("lm" and "glm").
  #' This is used as the initial model in the genetic search.
  #' The default model is 'lm'.
  #' @param family The family to be passed into 'glm'.
  #' The family can be 'binomial', 'gaussian', 'gamma', 'inverse.gaussian',
  #' 'possion', 'quasi', 'quasibinomial', 'quasipoisson'.
  #' The default family is "gaussian" family.
  #' @param fitness The fitness function to describe the fitness of all
  #' chromosomes in the generation. The default fitness function is negative
  #' 'AIC'. Please make sure your user inputted fitness function has high
  #' values for good candidates.
  #' @param ranked Logical; if TRUE parents are selected based on the
  #' rank of fitness values. The default is TRUE.
  #' See page 80 in Givens/Hoeting.
  #' @param selectionType the type of selection mechanism
  #' ("oneprop" and "twoprop"), which describes the process by which
  #' parents are chosen to produce offspring.
  #' The default selection mechanism is 'twoprop'.
  #' See page 76 of Givens/Hoeting.
  #' @param elitism Logical; if TRUE the fittest individual to
  #' survive at each generation. The default is 'TRUE'.
  #' @param crossoverType The type of crossover operation
  #' ("single" and "multiple"), which describe the process of
  #' generating offsprings by combing part of the genetic information
  #' from their parents. The default is 'single'.
  #' @param numCrossover The number of splits for type 'multiple'.
  #' The default number is 2.
  #' @param mutationRate The rate of mutation, which indicates the
  #' probability of mutating for each gene. Mutation is a genetic operation
  #' that changes an offspring chromosome by randomly introducing one or
  #' more alleles in loci. The default rate is 0.01.
  #' @param maxIter The maximum number of iterations to run before the
  #' GA search is halted. The defaulte number is 100.
  #' @param P The population size.
  #' @param seed The seed for reproducibility.
  #' @param VERBOSE Logical; if 'TRUE' prints which generation the algorithm
  #' is currently at.
  #'  
  #' @return A list of the final candidate where the list contains
  #' \itemize{
  #' \item \code{variables}: The variables that were
  #' selected in the final population
  #' \item \code{fit}: The \code{lm} or \code{glm} object of the fit with the
  #' above variables.
  #' \item \code{fitness} : The value of the fitness.
  #' \item \code{fitnessType} : The type of fitness.
  #' \item \code{lengthElitism} : If elitism was selected, the length
  #' of elitism (gives an idea for convergence).
  #' }
  #' 
  #' @details The Genetic Algorithms (GAs) are stochastic search
  #' algorithms that mimic the process of Darwinian natural selection.
  #' GAs simulate the biological evolution, where breeding among highly fit
  #' organisms ensures desirable attributes be passed to future
  #' generations, thereby providing a set of increasingly good candidate
  #' solutions to the optimization. 
  #' 
  #' The select function enables the application of genetic algorithms to
  #' problems where the decision variables are encoded as "binary".
  #'  
  #' Selection mechanism mimics the process by which parents are
  #' chosen to produce offspring. Crossover and mutation operations are
  #' used to produce offspring chromosomes from chosen parent chromosomes.
  #' 
  #' Rank-based method is applied here to prevent GAs convergence to
  #' a poor local optimum, and parents are chosen based on the rank of values
  #' of negative AIC function. Any R function, which takes as input an
  #' individual string representing a potential solution, that returns
  #' a numerical value describing its "fitness" is allowable to perform as
  #' a fitness function.
  #' 
  #' The population size is in the range of the
  #' chromosome length to two times of chromosome length, though this can be 
  #' overridden by the user. In this function, the default for
  #' the population size is twice of chromosome length, which is the number
  #' of columns of the feature matrix.
  #' 
  #' @examples
  #' Y <- mtcars$mpg
  #' X <- mtcars[2:11]
  #' 
  #' select(Y, X)
  #' 
  #' @export
  
  # set default P if invalid
  if(!is.numeric(P)) P <- 2 * ncol(X)
  
  # create initial population
  initPop <- initialize(Y, X, P, regType, family, seed)
  
  # set current iteration and population as initial
  currIter <- 1
  currPop <- initPop
  
  # set fitness function
  if (fitness == 'AIC') {
    fitness <- function(x) -1*AIC(x)
    fitnessType <- 'Negative AIC'
  } else {
    fitnessType <- 'User Supplied'
  }
  
  # set elitism lengths
  if (elitism) {
    lengthElite <- 0
  } else {
    lengthElite <- NA
  }
  
  if (VERBOSE) message('Current Generation: ', currIter)
  # start algo
  while (currIter < maxIter) {
    # get current generation's fitness
    currFitness <- sapply(currPop, function(x) fitness(x$fit))
    # rank if requested
    if (ranked) currFitness <- fitnessRanks(currFitness)
    
    # start new population
    i = 1
    newPop <- list()
    
    # if elitism requested, set previous elite as first entry
    if(elitism) {
      newPop[[1]] <- currPop[[which.max(currFitness)]]
      i = 2
    }
    
    # get next generation's population
    while (i < P) {
      # select two parents
      toSelect <- selection(selectionType, currFitness)
      parent1 <- currPop[[toSelect[1]]]$variables
      parent2 <- currPop[[toSelect[2]]]$variables
      # get the children
      children <- crossover(parent1, parent2, crossoverType, numCrossover)
      # mutate
      child1 <- mutate(mutationRate, children$child1)
      child2 <- mutate(mutationRate, children$child2)
      
      # add to new population
      newPop[[i]] <- list(variables = child1,
                          fit = regFunc(regType,
                                        family,
                                        Y ~ .,
                                        X[,as.logical(child1), drop = FALSE]))
      
      # if the population reach, discard the rest
      if(i == P) break
      
      # add next child to new population
      i <- i + 1
      newPop[[i]] <- list(variables = child2,
                          fit = regFunc(regType,
                                        family,
                                        Y ~ .,
                                        X[,as.logical(child2), drop = FALSE]))
      i <- i + 1
    }
    
    # if elitism requested, check how long the previous elite has survived.
    # this gives us a notion of convergence (if lengthElite ends very low,
    # we may not have converged yet and should run with a higher maxIter).
    if (elitism) {
      newFitness  <- sapply(newPop, function(x) fitness(x$fit))
      if (sum(newPop[[which.max(newFitness)]]$variables != 
          currPop[[which.max(currFitness)]]$variables) == 0) {
        lengthElite <- lengthElite + 1
      } else {
        lengthElite <- 0
      }
      
    }
    
    # update for next generation
    currIter <- currIter + 1
    if (VERBOSE) message('Current Generation: ', currIter)
    currPop <- newPop
  }

  # get the final fitness values
  finalFitness <- sapply(currPop, function(x) fitness(x$fit))
  
  # return the best candidate based on fitness
  finalCandidate <- list(variables =
                           currPop[[which.max(finalFitness)]]$variables,
                         fit =
                           currPop[[which.max(finalFitness)]]$fit,
                         fitness =
                           fitness(currPop[[which.max(finalFitness)]]$fit),
                         fitnessType =
                           fitnessType,
                         lengthElite =
                           lengthElite)
  
  return(finalCandidate)
}
