
# 0 Load packages ---------------------------------------------------------

library(ivd)
library(purrr)
library(dplyr)
library(stringr)
library(ggplot2)

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

scenarios <- data.frame(
  scenario = 1:8,
  n_schools = rep(c(50, 100, 200, 500), each = 2),
  n_students = c(1000, 2500, 3000, 10000, 6000, 20000, 25000, 50000)
)

reps <- 100


for (j in scenarios$scenario) {
  for (i in 1:reps) {
    
    # Progress message
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
    
    s <- summary.ivd(fit)
    saveRDS(s, paste0("final_project/out/S", j, "R", i, ".rds"))
    
    gc()
  }
}


# 3 Results ---------------------------------------------------------------

files <- list.files("final_project/out")

tab <- purrr::map_dfr(seq_along(files), function(i) {
  file_path <- paste0("final_project/out/", files[i])
  data <- readRDS(file_path)
  data$scenario <- sub("^[^0-9]*([0-9]+).*", "\\1", files[i])
  data$rep <- sub(".*[^0-9]([0-9]+)[^0-9]*$", "\\1", files[i])
  #data$file_index <- i  # Add a column for the file index
  return(data)
})

# Subset only scale random effects SD rows
dat <- tab[tab$Parameter == "sd_scl_Intc", ]

# Convergence rates
convergence <- dat |> 
  dplyr::with_groups(scenario,
                     dplyr::summarise,
                     rate = dplyr::n()/100)

# Results of Bias, MSE, Coverage and efficiency metrics
res <- dat |> 
  dplyr::mutate(bias = Mean - 0.09,
                mse = (Mean - 0.09)^2,
                coverage = ifelse(`2.5%` < 0.09 & `97.5%` > 0.09, 1, 0)) |> 
  dplyr::with_groups(scenario,
                     dplyr::summarise,
                     mean_bias = mean(bias),
                     mcse_bias = sd(bias),
                     mean_mse = mean(mse),
                     mcse_mse = sd(mse),
                     coverage = mean(coverage),
                     Rhat = mean(`R-hat`),
                     n_eff = mean(n_eff))

## Computing PIPs

pips <- tab |> 
  dplyr::filter(str_detect(Parameter, "^ss") ) |> 
  dplyr::mutate(delta = ifelse(Mean >= 0.75, 1, 0)) |> 
  dplyr::with_groups(c(scenario, rep),
                     dplyr::summarise,
                     mean_delta_rep = mean(delta)) |> 
  dplyr::with_groups(c(scenario),
                     dplyr::summarise,
                     mean_delta = mean(mean_delta_rep),
                     CIlow = quantile(mean_delta_rep, 0.025),
                     CIhigh = quantile(mean_delta_rep, 0.975)) |> 
  dplyr::mutate(schools = c(50, 50, 100, 100, 200, 200, 500),
                students = c(20, 50, 30, 100, 30, 100, 50))


model <- lm(mean_delta ~ schools * students, data = pips)
summary(model)



# Plots -------------------------------------------------------------------


# Reshape the data to long format
res_long <- res %>%
  tidyr::pivot_longer(cols = bias:n_eff, names_to = "metric", values_to = "value")

# Define custom colors for scenarios
scenario_colors <- c(
  "1" = "grey",
  "2" = "#df91a3",
  "3" = "#edbb8a",
  "4" = "#39b185",
  "5" = "#ca562c",
  "6" = "#009392",
  "7" = "#d46780"
)

# Create the plot
ggplot(res_long, aes(x = factor(scenario), y = value, color = factor(scenario))) +
  geom_segment(aes(xend = factor(scenario), yend = 0), size = 1) +
  geom_point(size = 3) +
  coord_flip() +
  facet_wrap(~metric, scales = "free_x") +
  scale_color_manual(values = scenario_colors) +
  labs(x = "Scenario", y = "Value", color = "Scenario", title = "Lollipop Plots of Metrics Across Scenarios") +
  theme_minimal() +
  
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())



dat |>
  dplyr::mutate(bias = Mean - 0.09,
                mse = (Mean - 0.09)^2,
                coverage = ifelse(`2.5%` < 0.09 & `97.5%` > 0.09, 1, 0)) |> 
  dplyr::filter(`R-hat` <= 1.1) |>
  #dplyr::filter(Mean < 3) |> 
  ggplot(aes(x  = mse)) +
  geom_density() +
  facet_wrap(~scenario)



# Bias and MSE ------------------------------------------------------------


theme_set(theme_void(base_family = "Roboto"))
theme_update(
  axis.text.x = element_text(color = "black", size = 12, 
                             margin = margin(t = 6)),
  axis.text.y = element_text(color = "black", size = 12, hjust = 1, 
                             margin = margin(r = 6), family = "Roboto Mono"),
  axis.line.x = element_line(color = "black", size = 0.7),
  panel.grid.major.y = element_line(color = "grey90", size = .6),
  plot.background = element_rect(fill = "white", color = "white"),
  plot.margin = margin(rep(20, 4))
)


dat_long <- dat |> 
  dplyr::mutate(bias = Mean - 0.09,
                mse = (Mean - 0.09)^2,
                coverage = ifelse(`2.5%` < 0.09 & `97.5%` > 0.09, 1, 0)) |> 
  tidyr::pivot_longer(cols = bias:coverage, names_to = "metric", values_to = "value") 

# Bias and MSE boxplots
dat_long |> 
  dplyr::filter(metric != "coverage") |> 
  dplyr::mutate(
    metric = dplyr::recode(metric,
                           "bias" = "Bias of Estimate",
                           "mse" = "Mean Squared Error",
                           "coverage" = "Coverage Rate")) |> 
  ggplot(aes(x = scenario, y = value, color = scenario, fill = scenario)) +
  geom_boxplot(
    aes(fill = scenario, fill = after_scale(colorspace::lighten(fill, .7))),
    size = .75, outlier.shape = NA, outliers = FALSE
  ) +
  scale_fill_manual(values = scenario_colors, name = "Scenario:",
                    labels = c(
                      "1: 50x20", "2: 50x50", "3: 100x30", "4: 100x100", 
                      "5: 200x30", "6: 200x100", "7: 500x50")) +
  scale_color_manual(values = scenario_colors, name = "Scenario:",
                     labels = c(
                       "1: 50x20", "2: 50x50", "3: 100x30", "4: 100x100", 
                       "5: 200x30", "6: 200x100", "7: 500x50")) +
  guides(fill = guide_legend(override.aes = list(color = "transparent"))) +
  facet_wrap(~metric, scales = "free_x") +
  coord_flip() +
  theme(
    strip.text = element_text(size = 12, family = "Roboto") # Customize text size, face, and family
  )


# Coverage ----------------------------------------------------------------

## Mean distribution

dat |>
  dplyr::filter(`R-hat` <= 1.1) |>
  dplyr::mutate(
    scenario = dplyr::recode(scenario,
                           "1" = "S1 95% Coverage: 0.93",
                           "2" = "S2 95% Coverage: 0.72",
                           "3" = "S3 95% Coverage: 0.80",
                           "4" = "S4 95% Coverage: 0.32",
                           "5" = "S5 95% Coverage: 0.53",
                           "6" = "S6 95% Coverage: 0.09",
                           "7" = "S7 95% Coverage: 0.02")) |> 
  #dplyr::filter(Mean < 3) |> 
  ggplot(aes(x  = Mean, fill = as.factor(scenario))) +
  geom_density(alpha = 0.7) +  # Adjust alpha for transparency
  scale_fill_manual(values = c("grey", "#df91a3", "#edbb8a", "#39b185", "#ca562c", "#009392", "#d46780" )) +
  facet_wrap(~scenario)  +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, family = "Roboto"))

