### Learning how to create the OLS estimator by hand 
#using code from https://m-clark.github.io/models-by-example/linear-regression.html

# Load Libraries
library(tidyverse)

# Make some data
set.seed(1000)

#Make our predictor variables and dependent variable 
N = 100 # number of observations 
k = 2 # independent variables
X = matrix(rnorm(N * k), ncol = k)
y = -0.5 + 0.2*X[, 1] + 0.1*X[, 2] + rnorm(N) 
# as we increase N we get closer and closer approximations to this value 

dfXy = data.frame(X, y)

# Creating the OLS function in R 

# Important things to create: 
# par = parameters we need to estimate 
# X = linear predictor matrix 
# y = dependent variable 

lm_ls <- function(par, X, y){
# Coeffiecents: 
beta = par 

# Linear Predictor:
LP = X %*% beta 

# link function (in this case the identity): 
mu = LP 

# The Least Squares Function:
L = crossprod(y - mu)
}

# Estimating the model 
X = cbind(1,X)

# Create the intial values: 
init = c(1, rep(0, ncol(X)))
names(init) = c('intercept', 'b1', 'b2')

fit_LS = optim(
  par = init[-1],
  fn  = lm_ls,
  X   = X,
  y   = y,
  control = list(reltol = 1e-8)
)

fit_lm = lm(y ~ ., dfXy)
