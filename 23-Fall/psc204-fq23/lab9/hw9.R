
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

data |> 
  dplyr::select(Depression_04, Satisfied_04, SelfWorth_04, FearDeath_04) |> 
  tidyr::pivot_longer(cols = -FearDeath_04,
                      names_to = "variable",
                      values_to = "value") |> 
  ggplot(aes(x = value, y = FearDeath_04)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~variable) +
  geom_smooth(method = "lm", fullrange = T, color = "green") +
  labs(x = NULL) +
  xlim(0, 40) +
  theme_bw()
