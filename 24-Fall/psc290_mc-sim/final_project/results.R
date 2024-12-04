library(purrr)
library(dplyr)
library(stringr)
library(ggplot2)

# List all files in the directory
files <- list.files("final_project/out")

# Combine the files into a single dataframe with a new column for file index
tab <- purrr::map_dfr(seq_along(files), function(i) {
  file_path <- paste0("final_project/out/", files[i])
  data <- readRDS(file_path)
  data$scenario <- sub("^[^0-9]*([0-9]+).*", "\\1", files[i])
  data$rep <- sub(".*[^0-9]([0-9]+)[^0-9]*$", "\\1", files[i])
  #data$file_index <- i  # Add a column for the file index
  return(data)
})


dat <- tab[tab$Parameter == "sd_scl_Intc", ]

scenarios <- data.frame(
  scenario = 1:8,
  n_schools = rep(c(50, 100, 200, 500), each = 2),
  n_students = c(1000, 2500, 3000, 10000, 6000, 20000, 25000, 50000)
)

## Convergence rates

# convergence <- dat |> 
#   dplyr::with_groups(scenario,
#                      dplyr::summarise,
#                      rate = dplyr::n()/100)


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

tab |> 
  dplyr::filter(str_detect(Parameter, "^ss") ) |> 
  dplyr::mutate(delta = ifelse(Mean >= 0.75, 1, 0)) |> 
  dplyr::with_groups(c(scenario, rep),
                     dplyr::summarise,
                     mean_delta_rep = mean(delta)) |> 
  dplyr::with_groups(c(scenario),
                     dplyr::summarise,
                     mean_delta = mean(mean_delta_rep)*100)

# Plots -------------------------------------------------------------------

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

# Reshape the data to long format
res_long <- res  |> 
  tidyr::pivot_longer(cols = mean_bias:n_eff, names_to = "metric", values_to = "value")

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

# Bias and MSE ------------------------------------------------------------



dat_long <- dat |> 
  dplyr::mutate(bias = Mean - 0.09,
                mse = (Mean - 0.09)^2,
                coverage = ifelse(`2.5%` < 0.09 & `97.5%` > 0.09, 1, 0)) |> 
  tidyr::pivot_longer(cols = bias:coverage, names_to = "metric", values_to = "value") 

bias_plot <- dat_long |> 
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

ggsave("bias_plot.png",
       path = "final_project/", width = 12, height = 6, dpi = 300)

## Mean distribution

mean_dist <- dat |>
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

ggsave("mean_dist.png",
       path = "final_project/", width = 12, height = 6, dpi = 300)

## Efficiency
# Reshape the data to long format
res_long <- res %>%
  tidyr::pivot_longer(cols = bias:n_eff, names_to = "metric", values_to = "value")

efficiency <- res_long |> 
  dplyr::filter(metric %in% c("Rhat", "n_eff")) |> 
  dplyr::mutate(
    metric = dplyr::recode(metric,
                           "n_eff" = "Effective Sample Size",
                           )
  ) |> 
ggplot( aes(x = factor(scenario), y = value, color = factor(scenario))) +
  geom_segment(aes(xend = factor(scenario), yend = 0), size = 1) +
  geom_point(size = 3) +
  coord_flip() +
  facet_wrap(~metric, scales = "free_x") +
  scale_color_manual(values = scenario_colors) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, family = "Roboto"))

ggsave("efficiency.png",
       path = "final_project/", width = 12, height = 6, dpi = 300)
