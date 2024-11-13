
# Exercise 1 --------------------------------------------------------------

xbar <- NULL
sd <- NULL
p <- NULL
for (i in 1:1000) {
  x <- rnorm(mean = 0, sd = 1, n = 50)
  xbar[i] <- mean(x)
  p[i] <- t.test(x)$p.value
  sd[i] <- sd(x)
}
bias <- mean(xbar)
sdbias <- sum(sd - 1)/1000
typeI <- mean(p < .05)

# For each iteration, a sample of 50 is drawn from a standard normal distribution.
# Then, we compute the mean of these 5 values, test if the mean of this distribution
# is different than zero with a t-test, save the p-value and the sample standard deviation.
# 
# The outcomes are bias of the mean, standard deviation bias and type I error rate.
#
# MC standard errors for all three outcomes.
bias_se <- sqrt((1 / (1000 * ( 1000 - 1))) * sum((xbar - mean(xbar))^2))

sd_bias_se <- sqrt((1 / (1000 * ( 1000 - 1))) * sum((sd - mean(sd))^2))

typeI_se <- sqrt((typeI * (1 - typeI))/1000)


# Exercise 2 --------------------------------------------------------------

metrics <- data.frame(
  bias = rep(NA, 1000),
  sd = rep(NA, 1000),
  typeI = rep(NA, 1000)
)

for (j in 1:1000) {
  xbar <- NULL
  sd <- NULL
  p <- NULL
  for (i in 1:1000) {
    x <- rnorm(mean = 0, sd = 1, n = 50)
    xbar[i] <- mean(x)
    p[i] <- t.test(x)$p.value
    sd[i] <- sd(x)
  }
  metrics$bias[j] <- mean(xbar)
  metrics$sd[j] <- mean(sd) - 1
  metrics$typeI[j] <- mean(p < .05)
}

# Histograms
metrics |> 
  tidyr::pivot_longer(cols = everything(), names_to = "metric", values_to = "value") |> 
  ggplot(aes(x = value)) +
  #geom_histogram() +
  geom_density() +
  facet_wrap(~ metric, scales = "free", ncol = 2) +
  theme_minimal()

# MC standard errors

sd(metrics$bias)
sd(metrics$sd)
sd(metrics$typeI)
