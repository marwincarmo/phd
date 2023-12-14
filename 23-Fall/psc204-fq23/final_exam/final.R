
# Final exam --------------------------------------------------------------


# Packages ----------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(car)

# Question 1 --------------------------------------------------------------

tempmood <- readr::read_csv("23-Fall/psc204-fq23/final_exam/tempmood.csv")
head(tempmood)

## A ----

temp_long <- tempmood |> 
  tidyr::pivot_longer(cols = c(part1:part4),
                      names_to = "participant",
                      values_to = "mood") 

ggplot(data = temp_long, aes(x = temp, y = mood, color =participant, fill = participant)) +
  geom_smooth(method = "lm", fullrange = T) +
  geom_point(alpha=.3)+
  theme_minimal(12) +
  labs(x = "Temperature (F)", y="Mood", colour = "Participant", fill = "Participant") +
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        legend.position = c(.9,.8))

## B ----

psych::describe(tempmood)

## C ----
crossprod(temp_long$temp)

cp <- function(x, y) {
  sum((x-mean(x))*(y-mean(y)))
}

cp(temp_long$temp, temp_long$mood)
cov(temp_long$temp, temp_long$mood)
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
Anova(aov_social, type=2)

# Simple effects

# Males
Anova(aov(psa ~ sports, data = dplyr::filter(social, female == 0)))

# Females
Anova(aov(psa ~ sports, data = dplyr::filter(social, female == 1)))

# Other activities
Anova(aov(psa ~ female, data = dplyr::filter(social, sports == 0)))

# Sports
Anova(aov(psa ~ female, data = dplyr::filter(social, sports == 1)))

## Plot

graph_data <- social |> 
  dplyr::with_groups(c(female, sports), summarise,
                     mean = mean(psa, na.rm=TRUE),
                     sd = sd(psa, na.rm=TRUE),
                     N = length(psa),
                     se = sd / sqrt(N)) |> 
  dplyr::mutate(female = ifelse(female == 0, "Male", "Female"),
                sports = ifelse(sports == 0, "Other", "Sports"))

ggplot(data = graph_data, 
       aes(x = female, y = mean, color = sports)) + 
  geom_point(size=2.5) +
  #geom_bar(stat="identity", color = "black", position = position_dodge()) + 
  geom_errorbar(width = .1, size=1,
                aes(ymin = mean - se, ymax = mean + se),
                #position = position_dodge(.9)
                ) + 
  theme_bw() + 
  labs(y = "Social Acceptance", 
       color = "Extracurricular Activities",
       x = "Biological sex", 
       title = "Social Acceptance by Extracurricular Activities and Sex")
