############################################################################################################
# Week 3: Repeated Measures ANOVA - Three Equally Spaced Occasions
############################################################################################################*This script provides 3 ways to conduct ANOVA analysis on repeated measures,
# This script provides 3 ways to conduct ANOVA analysis on repeated measures,
# [1] Using the ezANOVA and aov functions (long format data) - traditional way
# [2] Regressing Trend Scores On A Constant (wide format data) - MANOVA
# [3] Using the gls funciton in the nlme package (long format data) - mixed model


# Inputing data
wiscraw <- read.csv('../hde205/data//wisc3raw.csv')

# Creating a reduced data set with only 3 equally spaced occasions
verb <- wiscraw[,c(1,3:5)] 

# convert wide data to long format
verb_long <- reshape(verb,idvar="id",v.names="verb",varying=c(2:4),times=c(2,4,6), 
                     direction="long")
verb_long <- verb_long[order(verb_long$id),] # sort data by id

############################################################################################################
# [1] Repeated Measures ANOVA - Method 1
############################################################################################################
# convert variables to factors (i.e., categorical variables)
verb_long <- within(verb_long, {
    time <- factor(time)
    id <- factor(id)
})

# Use the ezANOVA function in the "ez" package to get Mauchly's test of sphericity
library(ez) 
m1 <- ezANOVA(data=verb_long, dv=verb, wid=id, within=time)
m1
# Sphericity is not met, but violation is not severe -> Report adjusted p-value

# The aov function produces prettier outputs
m2 <- aov(verb ~ time + Error(id), data = verb_long)
summary(m2)
# Effect of Time is significant with F=854.3

# setting up polynomial contrasts
contrasts(verb_long$time) <- contr.poly(3) # Important: equal spacing is assumed here!
m3 <- aov(verb ~ time + Error(id), data = verb_long)
summary(m3,split=list(time=list("linear"=1,"quadratic"=2))) # naming the first contrast "linear" and the second "quadratic" 
# Linear contrast is significant with F=1682.64
# Quadratic contrast is significant with F=26.03

# removing contrasts
contrasts(verb_long$time) <- NULL

############################################################################################################
# [2] Repeated Measures ANOVA - Method 2
############################################################################################################
# Create trend scores variables using orthogonal effect codes (for 3 equally spaced occasions)
# Note: if you have unequal spacing, you need to obtain coefficients using the contr.poly function
verb$verblin <- -1*verb$verb2+0*verb$verb4+1*verb$verb6
verb$verbqua <-  1*verb$verb2-2*verb$verb4+1*verb$verb6
verb$constant <- 1

# Regress trend scores on the constant of 1
m4 <- lm(verblin ~ -1 + constant, data=verb) # "-1" remove the intercept, which is redundant
m5 <- lm(verbqua ~ -1 + constant, data=verb)

summary(m4)
anova(m4)
# Linear trend is significant, but F value (1219.7) is smaller

summary(m5)
anova(m5)
# Quadratic trend is significant but F value (41.95) is larger

# The F values are different from above because Method 1 assumes sphericity
# When testing the trend scores, we do not make this assumption (i.e., we assumed an "unstructured" pattern)
# This is also called the "MANOVA" approach


############################################################################################################
# [3] Repeated Measures ANOVA - Method 3
############################################################################################################
library(nlme)

# gls = generalized least squares
# Assuming compound symmetry
m6 <- gls(verb ~ time, corr = corCompSymm(form = ~1|id), method="ML", data=verb_long)
anova(m6)
# Same F value as Method 1
summary(m6)
# The coefficients are the fixed effects

# testing contrasts
contrasts(verb_long$time) <- contr.poly(3) # Important: equal spacing is assumed here!
m7 <- gls(verb ~ time, corr = corCompSymm(form = ~1|id), method="ML", data=verb_long)
summary(m7)
# You can square the t-value to get the F-value when df=1
# Linear contrast: t=41.02, which is equivalent to F=1682.64
# Quadratic contrast: t=5.102, which is equivalent to F=26.03
# Hence these results are equivalent to those using Method 1

# Assuming unstructured covariance matrix
m8 <- gls(verb ~ time, corr = corSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time), 
          method="ML",data=verb_long)
anova(m8)
# F value is smaller, because we are estimating more parameters, which leads to lower power
summary(m8)
# Linear contrast: t=34.925, which is equivalent to F=1219.7
# Quadratic contrast: t=6.477, which is equivalent to F=41.95
# Hence these results are equivalent to those using Method 2

# Comparing CS and UN
anova(m6,m8)
# m8 has lower AIC and BIC and is a significant improvement over m6
# Note: you can only compare AIC and BIC if the models are estimated using "ML"
# By default, gls uses "REML"


## Assignment 2b: decide uni or multi via sphericity test