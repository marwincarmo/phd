
# 0 Load in data ----------------------------------------------------------

icecream <- read.csv("lab/data/IceCreamSales.csv")


# Covariance --------------------------------------------------------------

cov(x =, y=)
cov(x = icecream)


# Correlation -------------------------------------------------------------

cor(x = icecream, use = "complete.obs")


# Significance testing ----------------------------------------------------

# H0: rho = 0
# H1: rho != 0 

# Plot data

plot( y= icecream$Sales,x= icecream$Temperature)
abline
## T tstatistic

cor_estimate <- cor(icecream$Sales, icecream$Temperature)

sample_size <- nrow(icecream)

cor_se <- sqrt((1 - cor_estimate^2) / (sample_size - 2))

t_stat <- cor_estimate / cor_se

df_cor <- sample_size -2

# calculate the p value

pt(t_stat, df = df_cor, lower.tail = FALSE) * 2


cor.test(x = icecream$Temperature, y = icecream$Sales, 
         method = "pearson", conf.level = .99)

# We examined th relation between temperature  and ice cream sales in a
# sample of 12 days using Pearson's correlation coefficient test. 
# The re was a positive, statistically significant and large correlation between
#  tmperature and ice cream sales, r = 0.96, t(10) = 10.50, p < 0.001.
#  

# Simple linear regression ------------------------------------------------

# Sales =   b0  + b1*Temperature

-694.37 + 16.71 * 75

summary(simple_regression)

simple_regression <- lm(Sales ~ Temperature, data = icecream)
