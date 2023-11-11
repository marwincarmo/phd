library(dplyr)

dat <- readr::read_csv("23-Fall/psc204-fq23/lab6/age_religion_health.csv")


head(dat)
anova_marital <- aov(Bible_04 ~ married_04, data=dat)
summary(anova_marital)
count(dat, married_04)
