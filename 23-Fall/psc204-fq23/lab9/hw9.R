
# HOMEWORK 9 --------------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(psych)
library(car)
library(dplyr)
library(ggplot2)

data <- readr::read_csv("23-Fall/psc204-fq23/lab9/Age Religion Health.csv")
# Q1 ----------------------------------------------------------------------

cor_q1 <- data |> 
  dplyr::select(Depression_04, Satisfied_04, SelfWorth_04, FearDeath_04) |> 
  psych::corr.test()
cor.plot(cor_q1$r)

# Q4 ----------------------------------------------------------------------
get_line_color <- function(lm_model) {
  summary(lm_model)$coefficients[2, 4] < 0.05
}

long_data <- data |> 
  dplyr::select(Depression_04, Satisfied_04, SelfWorth_04, FearDeath_04) |> 
  tidyr::pivot_longer(cols = -FearDeath_04,
                      names_to = "variable",
                      values_to = "value")

long_data_plot <- long_data |> 
  tidyr::nest(data = -variable) |>
  dplyr::mutate(
    fit = purrr::map(data, ~lm(FearDeath_04 ~ value, data=.) |> broom::tidy())
  ) |> 
  tidyr::unnest(fit) |> 
  dplyr::filter(term != "(Intercept)") |> 
  tidyr::unnest(data)

long_data_plot |>   
  ggplot(aes(x = value, y = FearDeath_04)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~variable) +
  geom_smooth(method = "lm", fullrange = T) +
  labs(x = NULL) +
  xlim(0, 40) +
  theme_bw()

# Q5 ----------------------------------------------------------------------

mod5 <- lm(FearDeath_04 ~ Depression_04 + Satisfied_04 + SelfWorth_04, data = data)
summary(mod5)$r.squared
