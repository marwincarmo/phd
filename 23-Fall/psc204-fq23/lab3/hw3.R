sat <- readr::read_csv("sat_act.csv")

sat$education[1:3]
sat$SATV[4:6]

hist(sat$ACT,main = "Histogram of ACT scores",
     xlab = "ACT")

mean(sat[sat$education == "graduate/professional", "ACT"])

sat |> 
  dplyr::filter(education == "graduate/professional",
                gender == "female") |> 
  dplyr::summarise(average_act = mean(ACT, na.rm=TRUE))

# standard error

se_satq <- sd(sat$SATQ / sqrt(length(sat)))

# sampling dist

demo_sampling_dist <- function(num_samples = 20,
                               sample_size = 10, 
                               pop_mu = 0, 
                               pop_sigma = 1) {
  # num_samples: the number of samples to take
  # sample_size: the sample size for each sample
  # pop_mu: the mean of the *population*
  # pop_sigma: the standard deviation of the *population*
  means_of_samples <- rep(NA, num_samples)  
  for (i in 1:num_samples) {
    sample_i <- rnorm(sample_size, pop_mu, pop_sigma)
    means_of_samples[i] <- mean(sample_i)
  }
  
  xlims <- c(pop_mu - (3*pop_sigma), pop_mu + 3*pop_sigma)
  hist(means_of_samples, 
       main = "Sampling Distribution of Means", 
       xlab = "Values", 
       xlim = xlims)
  abline(v = pop_mu, col = "red", lwd = 2)
  abline(v = mean(means_of_samples), col = "steelblue", lwd = 2)
  
}

lapply(list(10, 100, 500), demo_sampling_dist)

demo_sampling_dist(20, 1000)

# AS the number of samples the histogram distribution get closer to the normal distribution
# As the sample size increases the distribution gets narrowed around the mean


