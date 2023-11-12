library(dplyr)
library(car)
library(ggplot2)
library(patchwork)

dat <- readr::read_csv("23-Fall/psc204-fq23/lab6/age_religion_health.csv") |> 
  dplyr::mutate(married_04 = factor(married_04,
                                    levels = c("Widowed",
                                               "Married",
                                               "Never Married",
                                               "Divorced",
                                               "Separated"
                                               )))


anova_marital <- aov(Bible_04 ~ married_04, data=dat)
summary.aov(anova_marital)


# B -----------------------------------------------------------------------

c1 <- c(-1, 1/4, 1/4, 1/4, 1/4)
c2 <- c(0, -1, -1, 1, 1)
c3 <- c(0, 1, -1, 0, 0)
c4 <- c(0, 0, 0, 1, -1)
cont_matrix <- cbind(c1, c2, c3, c4)

contrasts(dat$married_04) <- cont_matrix
cont_matrix

anova_marital <- aov(Bible_04 ~ married_04, data=dat)

split_list <- list(
  married_04 = list(
    "Widowed vs all" = 1,
    "Divorced and Separated vs Married and Never Married" = 2,
    "Married vs Never married" = 3,
    "Divorced vs Separated" = 4
  )
)

s_aov <- summary.aov(anova_marital, split = split_list)


# C -----------------------------------------------------------------------

contr_data = dat |> 
  mutate("widowed_v_rest" = case_when(
    married_04 == "Widowed" ~ "Widowed", 
    TRUE ~ "Marr, NM, Div, Sep"),
    "MarNM_v_DivSep" = case_when(
      married_04 == "Married" | married_04 == "Never Married" ~ "MarNM",
      married_04 == "Divorced" | married_04 == "Separated" ~ "DivSep", 
      TRUE ~ NA_character_))

# Contrast 1: Widowed vs all

graph_data_c1 <- contr_data  |> 
  group_by(widowed_v_rest) %>%
  summarise(mean = mean(Bible_04, na.rm = TRUE),
            sd = sd(Bible_04, na.rm = TRUE),
            n = length(Bible_04)) %>%
  mutate(se = sd/sqrt(n))

graph_c1 <-  ggplot(data = graph_data_c1,
                  aes(x = widowed_v_rest,
                      y = mean))+
  geom_bar(stat = "identity",
           color = "black", fill = "peachpuff4")+
  geom_errorbar(width = .3, aes(ymin = mean - se,
                                ymax = mean + se))+
  theme_classic()+
  labs(x = "Marital Status",
       y = "Frequency Reading Bible",
       title = "Contrast 1")

# Contrast 2: (Married, Never Married) vs (Divorced, Separated)

graph_data_c2 <- contr_data |> 
  filter(!is.na(MarNM_v_DivSep)) |>  # note that we filtered out the NA in the grouping variable
  group_by(MarNM_v_DivSep) |> 
  summarise(mean = mean(Bible_04, na.rm = TRUE),
            sd = sd(Bible_04, na.rm = TRUE),
            n = length(Bible_04))  |> 
  mutate(se = sd/sqrt(n))

graph_c2  <- ggplot(data = graph_data_c2,
                  aes(x = MarNM_v_DivSep,
                      y = mean))+
  geom_bar(stat = "identity",
           color = "black", fill = "peachpuff4")+
  geom_errorbar(width = .3, aes(ymin = mean - se,
                                ymax = mean + se))+
  theme_classic()+
  labs(x = "Marital Status",
       y = "Frequency Reading Bible",
       title = "Contrast 2")

# Contrast 3: Married vs Never Married

graph_data_c3 <-  contr_data  |> 
  filter(married_04 == "Married" | married_04 == "Never Married") |> 
  group_by(married_04) |> 
  summarise(mean = mean(Bible_04, na.rm = TRUE),
            sd = sd(Bible_04, na.rm = TRUE),
            n = length(Bible_04)) |> 
  mutate(se = sd/sqrt(n))

graph_c3  <-  ggplot(data = graph_data_c3,
                  aes(x = married_04,
                      y = mean))+
  geom_bar(stat = "identity",
           color = "black", fill = "peachpuff4")+
  geom_errorbar(width = .3, aes(ymin = mean - se,
                                ymax = mean + se))+
  theme_classic()+
  labs(x = "Marital Status",
       y = "Frequency Reading Bible",
       title = "Contrast 3")

# Contrast 4: Divorced vs Separated

graph_data_c4 <-  contr_data  |> 
  filter(married_04 == "Divorced" | married_04 == "Separated") |> 
  group_by(married_04) |> 
  summarise(mean = mean(Bible_04, na.rm = TRUE),
            sd = sd(Bible_04, na.rm = TRUE),
            n = length(Bible_04)) |> 
  mutate(se = sd/sqrt(n))

graph_c4  <-  ggplot(data = graph_data_c4,
                     aes(x = married_04,
                         y = mean))+
  geom_bar(stat = "identity",
           color = "black", fill = "peachpuff4")+
  geom_errorbar(width = .3, aes(ymin = mean - se,
                                ymax = mean + se))+
  theme_classic()+
  labs(x = "Marital Status",
       y = "Frequency Reading Bible",
       title = "Contrast 4")

graph_c1 + graph_c2 + graph_c3 + graph_c4


# 3 -----------------------------------------------------------------------

aov_int <- aov(Weight_04 ~ married_04 + sex + married_04:sex, data= dat)
summary(aov_int)

graph_data <- 
  dat |> 
  group_by(married_04, sex) |> 
  filter(!is.na(married_04)) |> 
  summarise(mean = mean(Weight_04, na.rm=TRUE),
            sd = sd(Weight_04, na.rm=TRUE),
            N = length(Weight_04),
            se = sd / sqrt(N))

p1 <- ggplot(data = graph_data, 
       aes(sex, mean, fill = married_04)) + 
  geom_col(position = "dodge") + 
  geom_errorbar(width = .5, 
                aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(.9)) +
  theme_classic() + 
  labs(y = "Weight", 
       x = "Sex", 
       fill = "Marital Status",
       title = "Weight by Sex and Marital Status") 

p2 <- ggplot(data = graph_data, 
       aes(x = married_04, y = mean, fill = sex)) + 
  geom_bar(stat="identity", color = "black", position = position_dodge()) + 
  geom_errorbar(width = .5, 
                aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(.9)) + 
  theme_classic() + 
  labs(y = "Weight", 
       fill = "Sex",
       x = "Marital Status", 
       title = "Weight by Marital Status and Sex") 

# 4 -----------------------------------------------------------------------

status <- levels(dat$married_04)

purrr::map(status, ~summary(aov(Weight_04 ~ sex, data = contr_data, 
                               subset = married_04 == .x)))
summary(aov(SundaySchool_04 ~ sex, data = lab_data, 
            subset = HeartHealth_04 == "Poor"))

pairwise.t.test(dat$Bible_04, g = dat$married_04, p.adjust.method = "bonferroni")
