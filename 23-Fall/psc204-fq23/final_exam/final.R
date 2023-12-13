
# Final exam --------------------------------------------------------------


# Packages ----------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)

# Question 1 --------------------------------------------------------------

tempmood <- readr::read_csv("23-Fall/psc204-fq23/final_exam/tempmood.csv")
head(tempmood)

## A ----

temp_long <- tempmood |> 
  tidyr::pivot_longer(cols = c(part1:part4),
                      names_to = "participant",
                      values_to = "mood") 

ggplot(data = temp_long, aes(x = temp, y = mood, color = participant)) +
  geom_line(linewidth = 0.8) +
  theme_minimal(12) +
  labs(x = "Temperature", colour = "Participant") +
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        legend.position = c(.9,.8))

## B ----

psych::describe(tempmood)

## C ----

## D ----

cor.test(temp_long$temp, temp_long$mood)

## E ----

summary(lm(mood ~ temp, data = temp_long))
  
## F ----

# Participant 1
summary(lm(part1 ~ temp, data = tempmood))

# Participant 2
summary(lm(part2 ~ temp, data = tempmood))
  
# Participant 3
summary(lm(part3 ~ temp, data = tempmood))

# Participant 4
summary(lm(part4 ~ temp, data = tempmood))

## G ----

tempmood |> 
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~(scale(.) %>% as.vector)
    )
  ) |> 
  tidyr::pivot_longer(cols = c(part1:part4),
                      names_to = "participant",
                      values_to = "mood") |> 
  tidyr::nest(data = -participant) |> 
  dplyr::mutate(
    fit = purrr::map(data, ~lm(mood ~ temp, data=.) |> broom::tidy())
  ) |> 
  tidyr::unnest(fit)

# Question 3 --------------------------------------------------------------

social <- readr::read_csv("23-Fall/psc204-fq23/final_exam/socialacceptance.csv")
head(social)

## B ----

aov_social <- aov(psa ~ sports + female + sports:female, data = social)
summary(aov_social)
