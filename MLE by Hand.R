# Let's make some parameters
set.seed(123) 
b0 <- -1
b1 <- 2
sigma2 <- 2


# Set up our sample size 
N = 1000

# Let's make some data
X = cbind(1,rnorm(N)) 
Y = b0 + b1*X[, 1] + rnorm(N, mean = 0, sd = 2)

# Let's make a log-likelihood function
lnL <- function(par, X, y){
  y <- Y
  X <- X
  k <- ncol(X)
  n <- N
  mu <- X%*%par[1:k]
  sum(-log(sqrt(1/(n-k)*sum((y-mu)^2))) -
        (1/(2*(sqrt(1/(n-k)*sum((y-mu)^2)))^2))*(y - mu)^2)
  }

MLE <- optim(par = c(-1,-1), fn = lnL, 
             method = 'BFGS', X=X, 
             control = list(maxit=1000, fnscale = -1) , hessian = T)

results = data.frame(intercept=rep(NA,1), beta1=rep(NA,1),
                     lm_intercept=rep(NA,1), lm_beta1=rep(NA,1))
results[1:2]<- MLE$par[1:2]
results[3]<- lm(Y~X[,2])[1]
results[4]<- lm(Y~X[,2])[["coefficients"]][["X[, 2]"]]

head(results)
