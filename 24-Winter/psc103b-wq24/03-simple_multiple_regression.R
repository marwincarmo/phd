## ----packages, echo=FALSE----------------------------------------------
library(dplyr)
library(ggplot2)
library(ggpubr)


## ----echo=FALSE--------------------------------------------------------
my_theme <- theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        # Bold, bigger title
        plot.title = element_text(face = "bold", size = rel(1.7)),
        # Plain, slightly bigger subtitle that is grey
        plot.subtitle = element_text(face = "plain", size = rel(1.3), color = "grey70"),
        # Italic, smaller, grey caption that is left-aligned
        plot.caption = element_text(face = "italic", size = rel(0.7), 
                                    color = "grey70", hjust = 0),
        legend.position = c("top"),
        legend.text = element_text(size=12),
        # Bold legend titles
        legend.title = element_text(face = "bold"),
        # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
        strip.text = element_text(face = "bold", size = rel(1.1), hjust = 0),
        # Bold axis titles
        axis.title = element_text(face = "bold"),
        # Add some space above the x-axis title and make it left-aligned
        axis.title.x = element_text(margin = margin(t = 10), hjust = 0),
        axis.text.x = element_text(color = "grey70"),
        # Add some space to the right of the y-axis title and make it top-aligned
        axis.title.y = element_text(margin = margin(r = 10), hjust = 1),
        axis.text.y = element_text(color = "grey70"),
        # Add a light grey background to the facet titles, with no borders
        strip.background = element_rect(fill = "grey90", color = NA))


## ----echo=TRUE---------------------------------------------------------
reading <- read.csv("data/Lab3Data.csv", header = TRUE)


## ----echo=FALSE--------------------------------------------------------

literacy_age <- lm(OverallLiteracy ~ ChildAge, data = reading)


## ----message=FALSE-----------------------------------------------------
ggplot(data = reading, aes(y = OverallLiteracy, x = ChildAge)) + 
  geom_point(alpha = .5) +
  theme_classic() +
  ylab('Literacy') +
  xlab('Child age (month)') +
  geom_smooth(method = 'lm', se = F, fullrange = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_segment(aes(xend = ChildAge, yend = literacy_age$fitted.values), color = "red",linetype = "dashed", alpha=.3 ) +
  coord_cartesian(xlim = c(0, 66))


## ----echo=TRUE---------------------------------------------------------
literacy_age <- lm(OverallLiteracy ~ ChildAge, data = reading)


## ----echo=TRUE---------------------------------------------------------
summary(literacy_age)


## ----------------------------------------------------------------------
literacy_parent <-  lm(OverallLiteracy ~ ParentChildAct, data = reading)


## ----echo=TRUE---------------------------------------------------------
confint(object = literacy_parent, level = 0.95)


## ----------------------------------------------------------------------
#| fig-align: "center"

ggplot(data = reading, aes(y = OverallLiteracy, x = ParentChildAct)) + 
  geom_point() +
  theme_minimal() +
  xlab('Parent-Child Activities') +
  ylab('Overall Literacy') +
  geom_smooth(method = 'lm', se = TRUE)


## ----echo=TRUE---------------------------------------------------------
pred_literacy <- 13.27 + 0.21*20
pred_literacy


## ----echo=TRUE---------------------------------------------------------
sqrt( (1/(nrow(reading) - 2)) * sum((reading$OverallLiteracy - predict(literacy_parent))^2) )
# OR
s_epsilon <- summary(literacy_parent)$sigma


## ----echo=TRUE---------------------------------------------------------
ss_x <- sum((reading$ParentChildAct - mean(reading$ParentChildAct))^2)


## ----echo=TRUE---------------------------------------------------------
s_y <- s_epsilon * sqrt( 1+(1/nrow(reading)) + (( (20 - mean(reading$OverallLiteracy))^2 ) / ss_x))
s_y


## ----echo=TRUE---------------------------------------------------------
t_crit <- qt(.025, nrow(reading) - 2, lower.tail = FALSE)



## ----echo=TRUE---------------------------------------------------------
pred_literacy - (t_crit*s_y)


## ----echo=TRUE---------------------------------------------------------
pred_literacy + (t_crit*s_y)


## ----------------------------------------------------------------------
pred_literacy_age <- 10.33 + 0.20 * 36
s_epsilon <- summary(literacy_age)$sigma
ss_x <- sum((reading$ChildAge - mean(reading$ChildAge))^2)
s_y <- s_epsilon * sqrt( 1+(1/nrow(reading)) + (( (20 - mean(reading$OverallLiteracy))^2 ) / ss_x))
t_crit <- qt(.025, nrow(reading) - 2, lower.tail = FALSE)

upper_limit <- pred_literacy_age + (t_crit*s_y)
lower_limit <- pred_literacy_age - (t_crit*s_y)


## ----echo=TRUE---------------------------------------------------------
predict(object = literacy_age, newdata = data.frame( ChildAge=36), level = 0.95, interval = "predict")


## ----echo=TRUE---------------------------------------------------------
literacy_multiple <- lm(OverallLiteracy ~ ChildAge + ParentChildAct, data = reading)


## ----------------------------------------------------------------------
broom::tidy(literacy_multiple) |>
  kableExtra::kbl()


## ----echo=TRUE---------------------------------------------------------
summary(literacy_multiple)$r.squared


## ----echo=TRUE---------------------------------------------------------
summary(literacy_age)$r.squared

summary(literacy_parent)$r.squared


## ----message=FALSE-----------------------------------------------------
#| fig-align: "center"
#| out-width: 40%

ggplot(data = reading, aes(y = OverallLiteracy, x = ChildAge)) + 
  geom_point(alpha = .5) +
  theme_minimal() +
  ylab('Literacy') +
  xlab('Child age (month)') +
  geom_smooth(method = 'lm', se = F, fullrange = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_segment(aes(xend = ChildAge, yend = literacy_age$fitted.values), color = "red",linetype = "dashed", alpha=.3 ) +
  coord_cartesian(xlim = c(0, 66))


## ----------------------------------------------------------------------
reading$ChildAge_c <- scale(reading$ChildAge, scale = FALSE)


## ----echo=TRUE---------------------------------------------------------
reading$ChildAge_c <- reading$ChildAge - mean(reading$ChildAge)
# OR with the scale() function
# reading$ChildAge_c <- scale(reading$ChildAge, scale = FALSE)


## ----echo=TRUE---------------------------------------------------------
head(reading$ChildAge)
head(reading$ChildAge_c)


## ----------------------------------------------------------------------
#| fig-align: "center"

g1 <- ggplot(data = reading, aes(y = OverallLiteracy, x = ChildAge)) + 
  geom_point(alpha = .5) +
  theme_minimal() +
  ylab('Literacy') +
  xlab('Child age (month)') +
  geom_smooth(method = 'lm', se = F) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  coord_cartesian(xlim = c(-23, 66))

# Mean-centered ChildAge
g2 <- ggplot(data = reading, aes(y = OverallLiteracy, x = ChildAge_c)) + 
  geom_point(alpha = .5) +
  theme_minimal() +
  ylab('Literacy') +
  xlab('Centered Child age (month)') +
  geom_smooth(method = 'lm', se = F) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  coord_cartesian(xlim = c(-23, 66))
  
ggarrange(g1, g2, nrow = 2)


## ----eval=FALSE--------------------------------------------------------
#| fig-align: "center"
#| out-width: 70%

## p_age1 <- ggplot(reading, aes(x = ChildAge)) +
##   geom_histogram(#bins = 30,
##                  color = "skyblue", fill = "lightblue") +
##   labs(title = "Histogram of Child Age",
##        x = "Child Age",
##        y = NULL)
## 
## p_age2 <- ggplot(reading, aes(x = ChildAge_c)) +
##   geom_histogram(#bins = 30,
##                  color = "skyblue", fill = "lightblue") +
##   labs(#title = "Histogram of Child Age",
##        x = "Child Age centered",
##        y = NULL)
## 
## ggarrange(p_age1, p_age2, ncol = 2)


## ----echo=TRUE---------------------------------------------------------
reading$ParentChildAct_c <- reading$ParentChildAct - mean(reading$ParentChildAct)


## ----echo=TRUE---------------------------------------------------------
literacy_multiple_centered = lm(OverallLiteracy ~ ChildAge_c + ParentChildAct_c,
                           data = reading)


## ----------------------------------------------------------------------
broom::tidy(literacy_multiple_centered) |> kableExtra::kbl()


## ----echo=TRUE---------------------------------------------------------
interaction_model <- lm(OverallLiteracy ~ ChildAge_c * ParentChildAct_c, data = reading)


## ----------------------------------------------------------------------
broom::tidy(interaction_model)


## ----echo=TRUE---------------------------------------------------------
int_plot <- sjPlot::plot_model(interaction_model, type = "int", jitter = TRUE,
           mdrt.values = "meansd", show.data = TRUE)


## ----------------------------------------------------------------------
#| fig-align: "center"
#| out-width: 45%
int_plot +
  xlab("Child Age centered") +
  my_theme

