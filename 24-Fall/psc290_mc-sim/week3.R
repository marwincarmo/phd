
# Exercise 1 --------------------------------------------------------------

estimators <- function(n) {
  x <- rnorm(n, 0, 1)
  list(
    x_bar = mean(x),
    M =  median(x),
  # high10 <- sort(x)[n * .1]
  # low10 <- sort(x, decreasing = TRUE)[n * 0.1]
  # mean(x[!x %in% c(high10, low10)])
    trimmed10 = mean(x, trim = .1),
    trimmed20 = mean(x, trim = .2),
    random = sample(x, 1)
  )
}

estimators(10)

# Exercise 2  -------------------------------------------------------------

set.seed(75888)
reps <- 10000
res <- matrix(data = NA, nrow = reps, ncol = 5)
colnames(res) <- c("Mean", "Median", "Trimmed10", "Trimmed20", "Random Value")

for (i in 1:reps) {
  
  y <- estimators(100)
  
  for (j in 1:ncol(res)) {
    res[i, j] <- y[[j]]
  }

}

par(mfrow=c(3,2))

for (j in 1:ncol(res)) {
  hist(res[, j], main = paste0("Histogram of the ", colnames(res)[j]))
}

# Exercise 3 --------------------------------------------------------------


abs_bias <- abs(colMeans(res - 0))

# Exercise 4 --------------------------------------------------------------

raw_efficency <- #1/(reps-1) * colMeans(res)^2 
apply(res, 2, var)

# Mean vs Median
raw_efficency[1]/raw_efficency[2]
# Mean vs T10
raw_efficency[1]/raw_efficency[3]
# Mean vs T20
raw_efficency[1]/raw_efficency[4]
# Mean vs Random
raw_efficency[1]/raw_efficency[5]
# Median vs T10
raw_efficency[2]/raw_efficency[3]
# Median vs T20
raw_efficency[2]/raw_efficency[4]
# Median vs Random
raw_efficency[2]/raw_efficency[5]
# T10 vs T20
raw_efficency[3]/raw_efficency[4]
# T10 vs Random
raw_efficency[3]/raw_efficency[5]
# T20 vs Random
raw_efficency[4]/raw_efficency[5]


# Exercise 5 --------------------------------------------------------------

mse <- #raw_efficency + abs_bias
apply(res^2, 2, mean)
rmse <- sqrt(mse)

# Exercise 6 --------------------------------------------------------------

n <- c(100, 200, 500, 1000, 10000)
reps <- 1000
res <- data.frame(sample_size = NA,
                  Mean = NA,
                  Median = NA,
                  T10 = NA,
                  T20 = NA,
                  Random = NA)

for (ss in n) {
  
  for (i in 1:reps)
    
    est <- estimators(n)
    res$sample_size <- ss
  
    for (j in 2:ncol(res))
    
      res[i, j] <- est[[j-1]]
}
