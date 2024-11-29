library(ivd)

# 1 Define the Data Structure ---------------------------------------------

# # Define the number of students and schools
# n_students <- 1500
# n_schools <- 50
# 
# # Create the data dataframe
# student_data <- data.frame(
#   student_id = 1:n_students,
#   school_id = sample(1:n_schools, n_students, replace = TRUE)
# )
# 
# school_data <- data.frame(
#   school_id = 1:n_schools,
#   within_sd = sample(c(0.5, 1, 2), n_schools, replace= TRUE, prob = c(.15, .7, .15))
# )
# 
# data <- merge(student_data, school_data, by = "school_id")
# 
# data$math <- rnorm(1, 0, )
# 
# head(data)
# 
# 
# 
# 
# for (i in unique(data$school_id)) {
#   # Define the within-cluster SD for each school. 30% will have either
#   # lower or higher than average variation
#   
#   s <- sample(c(0.5, 1, 2), 1, replace= TRUE, prob = c(.15, .7, .15))
#   data[data$school_id == i, "within_sd"] <- s
#   # Simulate math achievement scores
#   m <- rnorm(nrow(data[data$school_id == i,]), 0, s)
#   data[data$school_id == i, "math"] <- m
# }

# 2 Simulation design -----------------------------------------------------

# Define the number of students and schools
n_students <- 12000
n_schools <- 160

# Create the student data dataframe
student_data <- data.frame(
  student_id = 1:n_students,
  school_id = sample(1:n_schools, n_students, replace = TRUE)
)

u_sd <- 0.33
t_sd <- 0.09

Sigma <- matrix(c((u_sd^2), -0.003861,
                  -0.003861, (t_sd^2)), 2, 2)


cov2cor(Sigma)
v <- MASS::mvrnorm(n = n_schools,
                   mu = c(0,0), Sigma)

school_data <- data.frame(
  school_id = 1:n_schools,
  u = v[,1],
  t = v[,2],
  y = NA
)

data <- merge(student_data, school_data, by = "school_id")
data$mu <- 0 + data$u
data$sigma <- exp(0 + data$t)
data$y <- rnorm(n = nrow(data), mean = data$mu, sd = data$sigma)

hist(data$t)

# for (i in 1:nrow(data)) {
#   data[i, "y"] <- rnorm(1, data[i, "mu"], data[i, "sigma"])
# }

## Run ivd

fit <- ivd(location_formula = y ~ 1 + (1|school_id),
           scale_formula = ~ 1 + (1|school_id),
           data = data,
           niter = 2000, nburnin = 8000, workers = 4)

summary(fit, pip = "model")
plot(fit, "funnel")


# Metrics -----------------------------------------------------------------

# Capture Key Metrics: Record posterior means, credible intervals, and PIPs, 
# focusing on how well the model can recover the true parameters across 
# varying sample sizes and cluster sizes.


# Evaluate Model Recovery and Accuracy ------------------------------------

# Parameter Recovery: Compare estimated parameters to the known true values from the simulations. 
# Track:
#   Bias: Check if estimates systematically deviate from true values.
#   Coverage: Confirm if the credible intervals for parameters cover the true 
#   values at an acceptable rate (e.g., 95%).
# Inclusion Probabilities: Evaluate the model’s ability to accurately identify 
# clusters with high variance (if applicable in your simulation setup).
# Effective Sample Size: For each scenario, assess the effective sample sizes 
# and model convergence metrics (e.g., R-hat) to identify the minimum data 
# requirements for reliable estimates. Number of burnin and iterations?
# 
# Minimum Data Requirements: Determine the minimum sample size and observations 
# per cluster needed for stable parameter estimates and high inclusion accuracy,
# and document where model performance starts to decline.


# Function ----------------------------------------------------------

sim_schools <- function(n_students, n_schools, sd_t) {
  
  # n_students is the  total number of students in the sample
  # n_schools is the total number of schools in the sample
  # sd_t is the scale random effect standard deviation
  
  # Create the student data dataframe
  # The average number of students per school is n_students/n_schools
  student_data <- data.frame(
    student_id = 1:n_students,
    school_id = sample(1:n_schools, n_students, replace = TRUE)
  )
  
  # Create variance-covariance matrix
  Sigma <- matrix(c((0.33^2), -0.0224,
                    -0.0224, (sd_t^2)), 2, 2)
  
  # Create a matrix of random effects
  v <- MASS::mvrnorm(n = n_schools,
                     mu = c(0,0), Sigma)
  
  # Create the school data dataframe
  school_data <- data.frame(
    school_id = 1:n_schools,
    u = v[,1], # location random effects
    t = v[,2], # scale random effects
    y = NA # initialize a column for the outcome
  )
  
  # Merge student and school dataframes
  data <- merge(student_data, school_data, by = "school_id")
  
  # Create the mean achievement for each j school
  data$mu <- 0 + data$u
  # Create the standard deviation for each j school
  data$sigma <- exp(0 + data$t)
  # Sample the achievement for each student following a normal
  # distribution with mean mu_j and standard deviation sigma_j
  data$y <- rnorm(n = nrow(data), mean = data$mu, sd = data$sigma)
  
  return(data)
}

data <- sim_schools(12000, 160, .09)

## Run ivd

fit <- ivd(location_formula = y ~ 1 + (1|school_id),
           scale_formula = ~ 1 + (1|school_id),
           data = data,
           niter = 2000, nburnin = 8000, workers = 4)

s <- summary(fit)
plot(fit, "funnel")
