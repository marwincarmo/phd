
# 0 Load libraries --------------------------------------------------------

library(MASS)
library(dplyr)
library(lme4)
library(ivd)


# Step 1: Define Simulation Parameters ------------------------------------


set.seed(3413)

# Load necessary libraries
library(MASS)        # For multivariate normal data generation
library(dplyr)       # For data manipulation
library(lme4)        # For mixed-effects models, if needed for initial checks

# Step 1: Define parameters
set.seed(123)  # For reproducibility

sample_sizes <- c(500, 1000, 5000, 20000)    # Define different sample sizes
cluster_sizes <- c(50, 100, 160, 250)        # Define different cluster (school) sizes

# Define parameter values for fixed effects and variances
fixed_intercept <- 0.5
fixed_ses_effect <- 0.3   # Example effect size for SES on math achievement
school_random_var <- 0.2  # Variance for random intercepts at school level
student_residual_var <- 1  # Residual variance at student level

# Step 2: Data generation function
generate_data <- function(n_students, n_schools, fixed_intercept, fixed_ses_effect,
                          school_random_var, student_residual_var) {
  # Generate random intercepts for each school
  school_effects <- rnorm(n_schools, mean = 0, sd = sqrt(school_random_var))
  
  # Generate data frame to hold students' data
  data <- data.frame(
    student_id = 1:n_students,
    school_id = sample(1:n_schools, n_students, replace = TRUE),
    SES = rnorm(n_students, mean = 0, sd = 1)  # Simulate SES scores
  )
  
  # Calculate outcome (math achievement) based on fixed and random effects
  data <- data %>%
    mutate(
      school_effect = school_effects[school_id],
      math_achievement = fixed_intercept + fixed_ses_effect * SES + school_effect +
        rnorm(n_students, mean = 0, sd = sqrt(student_residual_var))
    )
  
  return(data)
}

# Step 3: Generate data for one scenario
# Example: 1000 students in 100 schools
sim_data <- generate_data(1000, 100, fixed_intercept, fixed_ses_effect, 
                          school_random_var, student_residual_var)
head(sim_data)




# Step 2: Set Up the Basic Simulation Framework in R ----------------------


# Extend the data generation function to include random slopes
generate_data <- function(n_students, n_schools, fixed_intercept, fixed_ses_effect,
                          school_random_var, student_residual_var, include_random_slope = FALSE) {
  
  # Generate random intercepts and optionally random slopes for each school
  school_intercepts <- rnorm(n_schools, mean = 0, sd = sqrt(school_random_var))
  school_slopes <- if (include_random_slope) rnorm(n_schools, mean = 0, sd = sqrt(0.1)) else rep(0, n_schools)
  
  # Create data frame for students
  data <- data.frame(
    student_id = 1:n_students,
    school_id = sample(1:n_schools, n_students, replace = TRUE),
    SES = rnorm(n_students, mean = 0, sd = 1)
  )
  
  # Calculate outcome (math achievement) based on fixed and random effects
  data <- data %>%
    mutate(
      school_intercept = school_intercepts[school_id],
      school_slope = school_slopes[school_id],
      math_achievement = fixed_intercept + fixed_ses_effect * SES + 
        school_intercept + school_slope * SES + 
        rnorm(n_students, mean = 0, sd = sqrt(student_residual_var))
    )
  
  return(data)
}

# Step 3: Run simulations for multiple scenarios
results <- list()  # List to store data for each scenario

# Loop over each sample and cluster size combination
for (n_students in sample_sizes) {
  for (n_schools in cluster_sizes) {
    
    # Run the simulation without random slope
    data_no_slope <- generate_data(n_students, n_schools, fixed_intercept, fixed_ses_effect,
                                   school_random_var, student_residual_var, include_random_slope = FALSE)
    
    # Run the simulation with random slope
    data_with_slope <- generate_data(n_students, n_schools, fixed_intercept, fixed_ses_effect,
                                     school_random_var, student_residual_var, include_random_slope = TRUE)
    
    # Store each scenario in results list
    results[[paste0("n_students_", n_students, "_n_schools_", n_schools, "_no_slope")]] <- data_no_slope
    results[[paste0("n_students_", n_students, "_n_schools_", n_schools, "_with_slope")]] <- data_with_slope
  }
}

# Inspect one of the scenarios
head(results[["n_students_1000_n_schools_100_no_slope"]])


# Step 4: Fit Initial Mixed-Effects Models --------------------------------

# Load necessary library for mixed-effects models
library(lme4)

# Select one simulated dataset for testing
test_data <- results[["n_students_1000_n_schools_100_with_slope"]]

# Step 4: Fit a random intercept and random slope model using lmer
# Model includes SES as a fixed effect and allows both random intercepts and slopes for SES at the school level
model_test <- lmer(math_achievement ~ SES + (1 + SES | school_id), data = test_data)

# Check model summary
summary(model_test)


# Step 5: Test with ivd ---------------------------------------------------

library(ivd)

fit <- ivd(location_formula = math_achievement ~ SES + (1 | school_id),
           scale_formula = math_achievement ~ SES + (1 | school_id),
           data = test_data,
           niter = 2000, nburnin = 4000, WAIC = TRUE, workers = 6)
s <- summary(fit)

plot(fit)
