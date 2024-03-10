
# 01 Load data ------------------------------------------------------------

wine <- read.csv("lab/data/wine.csv")

# 02 Quality ~ chlorides --------------------------------------------------

log_fit <- glm(quality_binary ~ chlorides,
               data = wine,
               family = "binomial")

summary(log_fit)

