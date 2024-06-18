library(tidyverse)
library(marginaleffects)
library(MASS)


# Ensure replication
set.seed(33)

### Simulate an ordinal variable
# Set the number of observations
n <- 1000

# Create two independent variable
y <- rbinom(n=n,size = 1,prob = 0.5)
x <- rnorm(n=n)
z <- rbinom(n = n, size = 1, prob = 0.5)

# Create our beta-coefficients 
b0 <- 0
b1 <- 2
b2 <- 1

#latent variable 
ystar <- b0 + b1*x + b2*z + rnorm(n = n, mean = 0, sd = 1)

y_logit <- vector(mode = "numeric", length = n)
y_logit[ystar>0] <- 1
y_logit[ystar<=0] <- 0

# estimate a model
model <- glm(y_logit ~ x + as.factor(z), 
             family = binomial(link = 'logit'), data=sample_data)

### Predicted Probability Plots

# N.B. type  = 'response' is the default for predictions 
# but not for avg_predictions
pred_prob <- avg_predictions(model, variables = 'z', type = 'response')

# pred_prob <- predictions(model, variables = 'z')

pred_prob |>
  ggplot(mapping = aes(x=z, y=estimate)) +
  geom_point() + 
  geom_pointrange(mapping = aes(ymin = conf.low, ymax = conf.high)) + 
  theme_bw()

### Marginal Effects 
# The default type here is 'response' for both slopes and avg_slopes
marg_fx <- avg_slopes(model, variables = 'x', by='y_logit') 

#marg_fx1 <- slopes(model)

marg_fx |> 
  group_by(y_logit) |> 
  filter(term == 'x') |> 
  mutate(mfx = mean(estimate)) |>
  mutate(ci_high = mean(conf.high)) |> 
  mutate(ci_low = mean(conf.low)) |>
  ggplot(mapping = aes(x=y_logit, y=mfx)) +
  geom_point() + 
  scale_y_continuous(limits = c(0.0,0.5)) + 
  scale_x_discrete(limits = c(0,1)) + 
  geom_pointrange(mapping = aes(ymin = ci_low, ymax = ci_high)) + 
  theme_bw()

### Some Other Cool Stuff

# This is the effect of increasing x by one unit 
marg_fx2 <- comparisons(model, variables = 'x')

marg_fx2 |>
  ggplot(mapping = aes(y = estimate, x = x, color = factor(z))) +
  geom_point() + 
  #geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
  scale_y_continuous(limits = c(0,0.75)) + 
  theme_bw()

# The effect of z going from 0 to 1
marg_fx3 <- avg_comparisons(model, variables = 'z')

marg_fx3 |>
  ggplot(mapping = aes(y = estimate, x = term)) +
  geom_point() + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
  scale_y_continuous(limits = c(0,0.25)) + 
  theme_bw()
