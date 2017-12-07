###############################################################################
#                                                                             #
#                          Genetic Algorithms Main                            #
#                                                                             #
###############################################################################

select <- function(Y,
                   X,
                   regType = 'lm',
                   family = 'gaussian',
                   fitness = "AIC",
                   ranked = TRUE,
                   selectionType = "twoprop",
                   elitism = TRUE,
                   crossoverType = "single",
                   numCrossover = NA,
                   mutationRate = .01,
                   maxIter = 100,
                   P = 2 * ncol(X),
                   seed = 1) {
  source('./R/utils.R')
  
  initPop <- initialize(Y, X, P, regType, family, seed)
  
  currIter <- 1
  currPop <- initPop
  if (fitness == "AIC") {
    fitness <- function(x) -1*AIC(x)
    fitnessType <- "Negative AIC"
  } else {
    fitnessType <- "User Supplied"
  }
  
  message("Current Generation: ", currIter)
  while(currIter < maxIter) {
    currFitness <- sapply(currPop, function(x) fitness(x$fit))
    if(ranked) currFitness <- fitnessRanks(currFitness)
    
    i = 1
    newPop <- list()
    if(elitism) {
      newPop[[1]] <- currPop[[which.max(currFitness)]]
      i = 2
    }
    
    while (i < P) {
      toSelect <- selection(selectionType, currFitness)
      parent1 <- currPop[[toSelect[1]]]$variables
      parent2 <- currPop[[toSelect[2]]]$variables
      children <- crossover(parent1, parent2, crossoverType, numCrossover)
      child1 <- mutate(mutationRate, children$child1)
      child2 <- mutate(mutationRate, children$child2)
      
      newPop[[i]] <- list(variables = child1,
                          fit = regFunc(regType,
                                        family,
                                        Y ~ .,
                                        X[,as.logical(child1), drop = FALSE]))
      i <- i + 1
      newPop[[i]] <- list(variables = child2,
                          fit = regFunc(regType,
                                        family,
                                        Y ~ .,
                                        X[,as.logical(child2), drop = FALSE]))
      i <- i + 1
      # in case P is odd
      if(i == P) break
    }
    
    currIter <- currIter + 1
    message("Current Generation: ", currIter)
    currPop <- newPop
  }

  finalFitness <- sapply(currPop, function(x) fitness(x$fit))
  
  finalCandidate <- list(variables =
                           currPop[[which.max(finalFitness)]]$variables,
                         fit =
                           currPop[[which.max(finalFitness)]]$fit,
                         fitness =
                           fitness(currPop[[which.max(finalFitness)]]$fit),
                         fitnessType =
                           fitnessType)
  
  return(finalCandidate)
}
