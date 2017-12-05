###############################################################################
#                                                                             #
#                          Genetic Algorithms Main                            #
#                                                                             #
###############################################################################

select(X, Y,
       family = c('lm', 'glm'),
       objective,
       fitness,
       selection,
       crossover,
       mutation,
       P = 2 * ncol(X),
       seed = 1) {
  source('./R/utils.R')
  
  init_pop <- initialize(X, P)
}