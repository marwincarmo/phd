
# packages ----------------------------------------------------------------

library(dplyr)

# load data ---------------------------------------------------------------

hw_data <- readr::read_csv("Homework5Data.csv")


# One-way ANOVA -----------------------------------------------------------

aov(condition ~ score, data = hw_data)
