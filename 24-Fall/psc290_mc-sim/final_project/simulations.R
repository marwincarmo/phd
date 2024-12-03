
# 0 Load packages ---------------------------------------------------------

library(ivd)
source("final_project/helpers.R")

# 1 Define a function to simulate school data -----------------------------

sim_schools <- function(n_students, n_schools, sd_t = 0.09) {
  
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
  sd_u <- 0.33
  cov_u_t <- -0.003861
  Sigma <- matrix(c((sd_u^2), cov_u_t,
                    cov_u_t, (sd_t^2)), 2, 2)
  
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


# 2 Simulation design -----------------------------------------------------

# Define the number of students and schools

# scenarios <- data.frame(
#   scenario = 1:8,
#   n_schools = rep(c(50, 100, 200, 500), each = 2),
#   n_students = c(1000, 2500, 3000, 10000, 6000, 20000, 25000, 50000)
# )

scenarios <- data.frame(
  scenario = 8,
  n_schools = 500,
  n_students = 50000
)

reps <- 100


for (j in scenarios$scenario) {
  for (i in 1:reps) {
    
    # Print progress message
    message(paste("Scenario", j,
                  "Replication:", i))
    
    data <- sim_schools(n_students = scenarios[scenarios$scenario == j, "n_students"], 
                        n_schools = scenarios[scenarios$scenario == j, "n_schools"])
    
    # Try to fit the model
    fit <- try(ivd(location_formula = y ~ 1 + (1|school_id),
                   scale_formula = ~ 1 + (1|school_id),
                   data = data,
                   niter = 2000, nburnin = 8000, workers = 4), silent = TRUE)
    
    # Check if the fit was successful
    if (inherits(fit, "try-error")) {
      message(paste("Error in scenario", j, "replication", i, "skipped."))
      next  # Skip to the next iteration
    }
    
    # Compute the summary only if the fit succeeded
    s <- summary.ivd(fit)
    saveRDS(s, paste0("final_project/out/S", j, "R", i, ".rds"))
    
    gc()
  }
}


# 3 Results ---------------------------------------------------------------

library(purrr)

# List all files in the directory
files <- list.files("final_project/out")

# Filter files that start with "S1"
s1_files <- files[grepl("^S1", files)]

# Combine the files into a single dataframe with a new column for file index
tab <- purrr::map_dfr(seq_along(files), function(i) {
  file_path <- paste0("final_project/out/", files[i])
  data <- readRDS(file_path)
  data$scenario <- sub("^[^0-9]*([0-9]+).*", "\\1", files[i])
  data$rep <- sub(".*[^0-9]([0-9]+)[^0-9]*$", "\\1", files[i])
  #data$file_index <- i  # Add a column for the file index
  return(data)
})


# Tests -------------------------------------------------------------------


dat <- tab[tab$Parameter == "sd_scl_Intc", ]

## Convergence rates

convergence <- dat |> 
  dplyr::with_groups(scenario,
                     dplyr::summarise,
                     rate = dplyr::n()/100)
# ifelse(cis[names(tval),1] < betas[names(tval)] & cis[names(tval),2] > betas[names(tval)], 1, 0)
res <- dat |> 
  dplyr::mutate(bias = Mean - 0.09,
                mse = (Mean - 0.09)^2,
                coverage = ifelse(`2.5%` < 0.09 & `97.5%` > 0.09, 1, 0)) |> 
 dplyr::with_groups(scenario,
                     dplyr::summarise,
                     bias = mean(bias),
                     mse = mean(mse),
                     coverage = mean(coverage),
                     Rhat = mean(`R-hat`),
                    n_eff = mean(n_eff))
