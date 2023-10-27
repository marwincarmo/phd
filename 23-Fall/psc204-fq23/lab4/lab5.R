library(dplyr)
library(ggplot2)

data <- readr::read_csv("Big5_Countries.csv")

head(data)

# Descriptive statistics --------------------------------------------------

# Extraversion mean, sd, number of people in each continent

table(data$Continent_Name)

data |> 
  tidyr::pivot_longer(cols = E, names_to = "trace", values_to = "value") |> 
  dplyr::with_groups(c(Continent_Name, trace),
                     dplyr::summarise,
                     mean = mean(value),
                     sd = sd(value),
                     n = n())

z95 <- qnorm(.05/2, lower.tail = FALSE)
no_people <- 100

data |> 
  ggplot() +
  stat_summary(aes(x = Continent_Name, y = E,
                   color = Continent_Name),
               fun = mean, geom = "pointrange",
               fun.min = function(x) (mean(x) - z95*(sd(x)/sqrt(no_people))),
               fun.max = function(x) (mean(x) + z95*(sd(x)/sqrt(no_people)))) +
  theme_classic() +
  ylim(1,5) +
  xlab("Continent") + 
  ylab("extraversion")


# ANOVA -------------------------------------------------------------------

# H0: The mean extraversion level across the five countries are equivalent
# Ha: The mean extraversion level across the five countries is different

extra_out <- aov(formula = E ~ Continent_Name, data = data)

summary(extra_out)

# Conclusion: We can reject the null hypothesis that all extraversion means are equal.

# 1) Use the p-value: 4.06e-06 < 0.05, reject H0
# 2) Use the F statistic:

qf(.05, 5, 594, lower.tail = FALSE)
# F statistic is greater than the critical value, so we reject H0

# post-hoc analysis

pairwise.t.test(x = data$E,
                g = data$Continent_Name,
                p.adjust.method = "bonferroni")

# One-way ANOVA without South America
summary(aov(formula = E ~ Continent_Name, data = data[data$Continent_Name != "South America",]))

# tukey's HSD

TukeyHSD(extra_out)

# ANOVA on consciouscesness

cons_out <- aov(C ~ Continent_Name, data = data)
summary(cons_out)

qf(.05, 5, 594, lower.tail = FALSE)

pairwise.t.test(data$C, data$Continent_Name, p.adjust.method = "bonferroni")
