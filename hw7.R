library(cmdstanr)
library(ggplot2)
library(psych)
library(bayesplot)
library(posterior)
library(mcmcse)
library(tidyverse)
library(ggdist)
library(rstan)
library(rstanarm)

df <- read.csv('dataset.csv')
summary(df)
ggplot(df, aes(x=Angle, y=Distance, color=Made)) + geom_point() +
  geom_hline(yintercept = 6.75, color ='green', size=1.25)


mean(df$Made)

# Correlation analysis
pairs.panels(df, 
             method = "pearson", # correlation method
             hist.col = "skyblue",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


# we know that 3pt shots are from distance 6.75m or more, so we will explore
# whether shots for 3 points are more succesfull or
df$is_3pt_shot <- as.integer(df$Distance > 6.75)
df$big_angle <- as.integer(df$Angle > 45)

tapply(df$Made, df$is_3pt_shot, mean)
# results
# 0         1 
# 0.5230596 0.4004149 
# So, 2 point shots are more succefful, therefore, it makes sense that with distance the probability of shot being succesfull decreases


# Modeling
model <- cmdstan_model("logistic_hw7.stan")

y <- df$Made
stan_data <- list(n = length(y), xa = df$Angle, xd = df$Distance, y = y)


fit <- model$sample(
  data = stan_data,
  chains= 1,
  iter_warmup = 5000,
  iter_sampling = 5000,
)

mcmc_trace(fit$draws())
fit$summary()


# scatterplot with contours
samples_df <- as_draws_df(fit$draws(c("b_angle", "b_dist")))

ggplot(samples_df, aes(x = b_angle, y = b_dist)) + geom_point() +
  geom_density_2d_filled(alpha = 0.4) +
  geom_density_2d(color='black') +
  theme(
    axis.text.x = element_text(size=14),
    axis.title.x = element_text(size=16),
    axis.text.y = element_text(size=14),
    axis.title.y = element_text(size=16)
  )


ggplot(samples_df, aes(x=b_dist)) + geom_density()


# Answer the question: Which is more important for shot success, angle or distance?
mcse(abs(samples_df$b_angle) > abs(samples_df$b_dist))

# Does shot success increase or decrease with increasing angle (the further on the sides we are)?
mcse(samples_df$b_angle > 0)

# sample 50 shots, get MCMC samples again from our bayesian model and plot scatterplot again
set.seed(27)
ids <- sample(1:nrow(df), size=50, replace = F)
rand_df <- df[ids,]
summary(rand_df)

y <- rand_df$Made
stan_data <- list(n = length(y), xa = rand_df$Angle, xd = rand_df$Distance, y = y)


fit <- model$sample(
  data = stan_data,
  chains= 1,
  iter_warmup = 5000,
  iter_sampling = 5000,
)

mcmc_trace(fit$draws())
fit$summary()


samples_df <- as_draws_df(fit$draws(c("b_angle", "b_dist")))

ggplot(samples_df, aes(x = b_angle, y = b_dist)) + geom_point() +
  geom_density_2d_filled(alpha = 0.4) +
  geom_density_2d(color='black') +
  theme(
    axis.text.x = element_text(size=14),
    axis.title.x = element_text(size=16),
    axis.text.y = element_text(size=14),
    axis.title.y = element_text(size=16)
  )


# Now, let's check values of coefficients when we normalize data
ndf <- df
ndf$Angle <- (df$Angle - min(df$Angle)) / (max(df$Angle) - min(df$Angle)) 
ndf$Distance <- (df$Distance - min(df$Distance)) / (max(df$Distance) - min(df$Distance)) 

y <- ndf$Made
stan_data <- list(n = length(y), xa = ndf$Angle, xd = ndf$Distance, y = y)


fit <- model$sample(
  data = stan_data,
  chains= 1,
  iter_warmup = 5000,
  iter_sampling = 5000,
)

mcmc_trace(fit$draws())
fit$summary()

samples_norm_df <- as_draws_df(fit$draws(c("b_angle", "b_dist")))
mcse(abs(samples_norm_df$b_angle) > abs(samples_norm_df$b_dist))


