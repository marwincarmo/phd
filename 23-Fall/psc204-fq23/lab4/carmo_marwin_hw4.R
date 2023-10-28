
# Load packages -----------------------------------------------------------

library(ggplot2)
library(dplyr)

# Load data ---------------------------------------------------------------


dat <- readr::read_csv("Age Religion Health _ HW 4.csv")

# Question 3 - A ----------------------------------------------------------


church <- dat |> 
  dplyr::filter(!is.na(HowOftenChurch_04)) |> 
  dplyr::with_groups(Smoke_04,
                     summarise,
                     mean = mean(HowOftenChurch_04, na.rm = T),
                     sd = sd(HowOftenChurch_04, na.rm = T),
                     N = length(HowOftenChurch_04)) |> 
  dplyr::mutate(se = sd/sqrt(N))

relationship <- dat |> 
  dplyr::filter(!is.na(RelationshipGod_04)) |> 
  dplyr::with_groups(Smoke_04,
                     summarise,
                     mean = mean(RelationshipGod_04, na.rm = T),
                     sd = sd(RelationshipGod_04, na.rm = T),
                     N = length(RelationshipGod_04)) |> 
  dplyr::mutate(se = sd/sqrt(N))


# Question 3 - B ----------------------------------------------------------

t.test(HowOftenChurch_04 ~ Smoke_04, data = dat, var.equal=FALSE)

effectsize::cohens_d(HowOftenChurch_04 ~ Smoke_04, data = dat, var.equal=FALSE)

t.test(RelationshipGod_04 ~ Smoke_04, data = dat, var.equal=FALSE)

effectsize::cohens_d(RelationshipGod_04 ~ Smoke_04, data = dat, var.equal=FALSE)

# Question 3 - C ----------------------------------------------------------

long_data <- dat |> 
  dplyr::select(Smoke_04, HowOftenChurch_04, RelationshipGod_04) |> 
  tidyr::pivot_longer(cols = -Smoke_04, values_to = "score", names_to = "variable") |> 
  dplyr::with_groups(c(Smoke_04, variable), summarise,
                     mean = mean(score, na.rm = T),
                     sd = sd(score, na.rm = T),
                     N = sum(!is.na(score))) |> 
  dplyr::mutate(se = sd/sqrt(N))

long_data |> 
  ggplot(aes(x = variable, y = mean, color = Smoke_04)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                color = "black", size = 0.5, width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Mean and Error Bars for outcome variables by smoking status",
    x = "Outcome",
    y = "Mean",
    color = "Smoking status"
  ) +
  scale_x_discrete(labels = c(
    "HowOftenChurch_04" = "Church attendance",
    "RelationshipGod_04" = "Relationship with God"
  )) +
  theme(
    text = element_text(size = 12),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

