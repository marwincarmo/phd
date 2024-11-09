## PSC 290 (Rhemtulla) Week 3 code ##

#generate random normal univariate data and estimate mean 5 ways: 
#1. sample mean
#2. median
#3. 10% trimmed mean
#4. 20% trimmed mean
#5. a value drawn at random

getMeans <- function(n){
  df <- rnorm(n, mean = 0, sd = 1)
  x1 <- mean(df)
  x2 <- median(df)
  q10 <- quantile(df, probs = c(.1, .9))
  x3 <- mean(df[df > q10[1] & df < q10[2]])
  q20 <- quantile(df, probs = c(.2, .8))
  x4 <- mean(df[df > q20[1] & df < q20[2]])
  x5 <- sample(df, 1)
  return(c(x1, x2, x3, x4, x5))
}

#run a condition
results <- matrix(NA, 1000, 5)
set.seed(2783983)
for (i in 1:1000){
  results[i,] <- getMeans(n = 100)
}

results <- data.frame(results)
colnames(results) <- c("mean", "med", "t10", "t20", "rand")
head(results)

#plot the distributions: 
library(tidyverse)
results_long <- results %>%
  pivot_longer(cols = mean:rand, 
               names_to = "estimator", 
               values_to = "estimate")


p <-  ggplot(results_long, aes(factor(estimator), estimate))
p + geom_violin(aes(fill = estimator)) + geom_jitter(height = 0, width = 0.1)

#compute absolute bias
colMeans(results)
apply(results, 2, mean)
#compute relative bias
#can't divide by zero!

#compute raw efficiency
eff <- apply(results, 2, var)

#compute relative efficiency
eff[1]/eff[2]
eff[1]/eff[3]
eff[1]/eff[4]
eff[1]/eff[5]

#compute MSE: 
MSE <- apply(results^2, 2, mean)
MSE

#compute RMSE: 
RMSE <- sqrt(MSE)
RMSE

#replicate simulation for 5 sample sizes: 
nlist <- c(100, 200, 500, 1000, 10000)
results <- matrix(NA, 5000, 6)
set.seed(2783983)
for (j in 1:5){
  n <- nlist[j]
    for (i in 1:1000){
    results[((1000*(j-1))+i),1] <- n
    results[((1000*(j-1))+i),2:6] <- getMeans(n = n)
    }  
}

colnames(results) <- c("n", "mean", "med", "t10", "t20", "rand")
head(results)


#for each estimator, compute RMSE for each sample size:
results <- data.frame(results)
results$n <- as.factor(results$n)

#compute MSE: 
MSE.mean <- tapply(results[,2]^2, results$n, mean)
MSE.med <- tapply(results[,3]^2, results$n, mean)
MSE.t10 <- tapply(results[,4]^2, results$n, mean)
MSE.t20 <- tapply(results[,5]^2, results$n, mean)
MSE.rand <- tapply(results[,6]^2, results$n, mean)

plot(MSE.mean, type = "l", lty = 1, ylim = c(0, .015))
lines(MSE.med, lty = 2)
lines(MSE.t10, lty = 3)
lines(MSE.t20, lty = 4)

plot(MSE.rand, type = "l", ylim = c(0, 1))

#compute RMSE: 
RMSE <- sqrt(MSE)
RMSE

