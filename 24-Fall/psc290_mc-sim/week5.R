
source("nortaraFunctions.R")



generate_cor <- function(n) {
  
  #create a vector of distribution families for each variable
  quant_margs <- c("qt", "qf")
  
  #create a list of distribution parameters for each variable
  param_margs <- list(
    mt = list(df=5),
    mf = list(df1=1, df2=30))
  
  R <- matrix(c(1, .4,
                .4, 1), 2, 2)
  
  df <- data.frame(gennortaRA(n = n, cor_matrix = R, invcdfnames =
                                quant_margs, paramslists = param_margs)) 
  return(cor(df)[2])
}

generate_cor(100)

x <- replicate(1000, generate_cor(100))

hist(x)


# Exercise 2 --------------------------------------------------------------


e2fun <- function() {
  
  ## Simulate 2 variables, 𝑥 and 𝑦, that have a MVN distribution with 𝜌 = .5
  
  R <- matrix(.5, 2, 2)
  diag(R) <- 1
  
  xy <- MASS::mvrnorm(n = 100, mu = c(0, 0), Sigma = R)
  
  ## Categorize them into 2, 3, 4, or 5 categories (pick one!)
  
  q <- c(quantile(xy, .25), quantile(xy, .5), quantile(xy, .75))
  
  
  for (i in 1:nrow(xy)) {
    
    for (j in 1:2) {
      
      if (xy[i, j] <= q[1]) {
        xy[i, j] <- 0
      } else if (xy[i, j] <= q[2] & xy[i, j] > q[1]) {
        xy[i, j] <- 1
      } else if (xy[i, j] <= q[3] & xy[i, j] > q[2]) {
        xy[i, j] <- 2
      } else if (xy[i, j] >= q[3]) {
        xy[i, j] <- 3 }
    }
  }
  
  ## On each dataset, compute the Pearson correlation between 𝑥 and 𝑦
  
  return(xy_cor <- cor(xy)[2])
}


## Outcome: absolute and relative bias

res <- replicate(1000, e2fun())

bias <- mean(res - .5)
