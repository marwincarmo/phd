library(dplyr)
library(ggplot2)

hw7_2023 <- readr::read_csv("23-Fall/psc204-fq23/lab7/hw7_2023.csv")

# 2 -----------------------------------------------------------------------

hw_long <- hw7_2023 |> 
  tidyr::pivot_longer(
    cols = t1:t3,
    names_to = "time",
    values_to = "step_count"
  ) |> 
  dplyr::mutate(
    dplyr::across(id:time, factor)
  )

# 3 -----------------------------------------------------------------------

hw_long |> 
  dplyr::with_groups(c(time, category),
                     summarise,
                     mean_steps = mean(step_count, na.rm = TRUE),
                     n = n(),
                     se = sd(step_count, na.rm=TRUE)/sqrt(n)) |> 
  ggplot(aes(x = time, y = mean_steps, color = category)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_steps - se, 
                    ymax = mean_steps + se), 
                width = 0.1) +
  scale_x_discrete(labels = paste("Time", 1:3)) +
  labs(x = "Time", y = "Mean Steps") +
  theme_bw() 

# 4 -----------------------------------------------------------------------
library(rstatix)
rma <- aov(step_count ~ time + category + time * category + Error(id/ (time)), data = hw_long)  
summary(rma)

rma2 <- anova_test(data = hw_long,
                            dv = step_count, wid = id,
                            within = c(time), between = c(category))

get_anova_table(rma2)

# 5 -----------------------------------------------------------------------

# create contrasts - 3 is the number of time points
poly_contrasts <- contr.poly(3)

contrasts(hw_long$time) <- poly_contrasts

split_list <- list(
  time = list("linear" = 1, "quadratic" = 2)
)

std_trends <- aov(step_count ~ time + Error(id/ (time)), data = hw_long,
                  subset = (category == "standard"))
summary(std_trends, split = split_list)

trn_trends <- aov(step_count ~ time + Error(id/ (time)), data = hw_long,
                  subset = (category == "training"))
summary(trn_trends, split = split_list)
