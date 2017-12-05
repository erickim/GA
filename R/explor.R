source("./R/utils.R")

Y <- rnorm(100)
X <- as.data.frame(matrix(rnorm(100*26), nrow = 100))
names(X) <- letters
P <- ncol(X)*2

x <- initialize(Y, X, P)
xaic <- lapply(x, AIC)