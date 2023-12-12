### Let's make some data! 
# Ensure replication
set.seed(33)

### Simulate an ordinal variable
# Let's set the number of observations
n <- 1000

# Create two independent variables
x <- rnorm(n=n)
z <- rbinom(n = n, size = 1, prob = 0.5)

# Create our beta-coefficents 
b0 <- 0
b1 <- 2
b2 <- 1

# Let's make our latent variable 
ystar <- b0 + b1*x + b2*z + rnorm(n = n, mean = 0, sd = 1)

# For an ordinal variable, we need to set our cut-points - we'll call them tau
tau_0 <- -Inf
tau_1 <- -1
tau_2 <- 1
tau_3 <- 4
tau_4 <- 10
tau_5 <- Inf

# Generate y-variables based on our cutpoints
y <- vector(mode = "numeric", length = n)
y[tau_0 < ystar & ystar < tau_1] <- 1
y[tau_1 < ystar & ystar < tau_2] <- 2
y[tau_2 < ystar & ystar < tau_3] <- 3
y[tau_3 < ystar & ystar < tau_4] <- 4
y[tau_4 < ystar & ystar < tau_5] <- 5

#generate y = 0
#replace y = 1 if ystar<-1
#replace y = 2 if -1<ystar & ystar<1
#replace y = 3 if 1<ystar & ystar<4
#replace y = 4 if 4<ystar & ystar<10
#replace y = 5 if 10<ystar


###Estimate an Ordinal Logit Regression Model
library(MASS)


model <- polr(as.factor(y) ~ x + z, method = "logistic")
summary(model)

# Time for some Predicted Probability Plots! 
library(marginaleffects)
library(tidyverse)

pred_prob <- avg_predictions(model)

ggplot(data = pred_prob, mapping = aes(y = estimate, x =group )) + 
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  xlab("Level of Ordinal Variable") +
  ylab("Avg. P(Observing our Y-variable)")
  
# Let's make some Marginal Effects Plots! 
marg_fx <- avg_slopes(model)

# Now let's look at the marginal effect of just x on y
marg_fx |>
  filter(term == 'x') |>
  ggplot(mapping = aes(y = estimate, x = group)) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))

# Now let's look at z's effect on y 
marg_fx |>
  filter(term == 'z') |>
  ggplot(mapping = aes(y = estimate, x = group)) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))

### What if we have an interaction? 

# Create a new beta-coefficient
b0 <- 0
b1 <- 2
b2 <- 1
b3 <- 2

# Let's make a new latent variable 
ystar_1 <- b0 + b1*x + b2*z + b3*x*z + rnorm(n = n, mean = 0, sd = 1)

# For an ordinal variable, we need to set our cut-points - we'll call them tau
tau_0 <- -Inf
tau_1 <- -1
tau_2 <- 1
tau_3 <- 4
tau_4 <- 10
tau_5 <- Inf

# Generate y-variables based on our cutpoints
y_1 <- vector(mode = "numeric", length = n)
y_1[tau_0 < ystar_1 & ystar_1 < tau_1] <- 1
y_1[tau_1 < ystar_1 & ystar_1 < tau_2] <- 2
y_1[tau_2 < ystar_1 & ystar_1 < tau_3] <- 3
y_1[tau_3 < ystar_1 & ystar_1 < tau_4] <- 4
y_1[tau_4 < ystar_1 & ystar_1 < tau_5] <- 5


# Create a NEW regression model
model_1 <- polr(as.factor(y_1) ~ x + z + x*z, method = "logistic")

# The Marginal Effect of X on Y conditional on Z! 
marg_fx1 <- avg_slopes(model_1, variables = 'x', by = 'z')

marg_fx1 |>
  ggplot(mapping = aes(y = estimate, x = group, color = as.factor(z))) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  labs(x="Levels of Ordinal Variable", y="Marginal Effect of X on Y") +
  scale_color_manual(labels = c("z = 0", "z = 1"), values = c("blue", "red")) +
  theme_bw() +
  guides(color=guide_legend("Condition of Z"))


# Some other cool stuff! 

# The Marginal Effect of Z going from 0 to 1! 
marg_fx2 <- avg_comparisons(model_1, variables = 'z')

marg_fx2 |>
  ggplot(mapping = aes(y = estimate, x = group)) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))

# The Marginal Effect of x going from 1 to 2, 2 to 3, 3 to 4, and 4 to 5!
marg_fx3 <- avg_comparisons(model_1, variables = 'x')

marg_fx3 |>
  ggplot(mapping = aes(y = estimate, x = group)) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))

