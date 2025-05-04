
############################################################################################################
# Week 4: Repeated Measures ANOVA II
############################################################################################################
# This script contains two parts. The first part provides codes for setting up covariance pattern models.
# The second part provides 3 ways to conduct repeated measures ANOVA with multiple groups.

# Inputing data
wiscraw <- read.csv('C:/Users/siwei-admin/Dropbox/HDE 205/WISC data/wisc3raw.csv')

# Transform from wide to long
list1 <- 2:5 # indicators for verb
list2<- 6:9 # indicators for perfo
wiscraw_long <- reshape(wiscraw,idvar="id",v.names=c("verb","perfo"),varying=list(list1,list2),times=c(1,2,4,6), 
                        direction="long")
wiscraw_long <- wiscraw_long[order(wiscraw_long$id),] # sort data by id

############################################################################################################
# Covariance Pattern Models
############################################################################################################
# descriptive statistics for verbal scores
library(psych)
describe(wiscraw[,2:5])
# Both the mean and sd increase over time

# convert variables to factors (i.e., categorical variables)
wiscraw_long <- within(wiscraw_long, {
    time <- factor(time)
    grad <- factor(grad)
    id <- factor(id)
})

library(nlme)

# Compound symmetry
m1 <- gls(verb ~ time, corr = corCompSymm(form = ~1|id), method="ML", data=wiscraw_long)
summary(m1) # get the fixed effects parameters and fit indices
getVarCov(m1) # get the random effects parameters
cov2cor(getVarCov(m1))

# AR1 pattern
m2 <- gls(verb ~ time, corr = corAR1(form = ~1|id), method="ML", data=wiscraw_long)
summary(m2)
getVarCov(m2)
cov2cor(getVarCov(m2))

# CS with heterogeneous variances
m3 <- gls(verb ~ time, corr = corCompSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time),
          method="ML",data=wiscraw_long)
summary(m3)
getVarCov(m3)
cov2cor(getVarCov(m3))

# AR1 with heterogeneous variances
m4 <- gls(verb ~ time, corr = corAR1(form = ~1|id), weights = varIdent(form = ~ 1 | time),
          method="ML",data=wiscraw_long)
summary(m4)
getVarCov(m4)
cov2cor(getVarCov(m4))

# Unstructured
m5 <- gls(verb ~ time, corr = corSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time), 
          method="ML",data=wiscraw_long)
summary(m5)
getVarCov(m5)
cov2cor(getVarCov(m5))

# Compare all 5 models
anova(m1,m2,m3,m4,m5)
# AIC picks UN, BIC picks CSH

anova(m3)
anova(m5)
# Model with CSH pattern has higher power

# Useful resources: https://stats.oarc.ucla.edu/r/seminars/repeated-measures-analysis-with-r/


############################################################################################################
# Repeated Measures ANOVA with Multiple Groups - Method 1
############################################################################################################
table(wiscraw$grad)
# Most mothers did not graduate from high school

# run anova with multiple groups
library(ez) 
mg1 <- ezANOVA(data=wiscraw_long, dv=verb, wid=id, within=time, between=grad)
# ezANOVA produced a warning that the value of "type" needs to be carefully selected
# There are three types of SS:
# Type I is generally not recommended for unbalanced data
# Type II is generally recommended for unbalanced data, especially when the focus is on the main effects
# Type III should be used with unbalanced data when interactions are suspected to be present

# By default, ezANOVA uses Type II SS
mg1
# Sphericity is not satisfied and is severely violated.
# All effects are significant.

# Because the interaction is significant, Type III SS is more appropriate
mg1_type3 <- ezANOVA(data=wiscraw_long, dv=verb, wid=id, within=time, between=grad, type=3)
mg1_type3
# These should be identical to SAS and SPSS outputs

# Unfortunately, the aov function for testing contrasts assumes balanced data, which is not appropriate here
# Hence, we will not test for contrasts using Method 1

############################################################################################################
# Repeated Measures ANOVA with Multiple Groups - Method 2
############################################################################################################
# Obtain contrast coefficients
contr.poly(4,scores=c(1,2,4,6))

# Create trend scores variables using orthogonal effect codes 
wiscraw <- within(wiscraw,{
    verblin <- -0.5858501*verb1 - 0.3254723*verb2 + 0.1952834*verb4 +  0.7160390*verb6
    verbqua <-  0.4959593*verb1 - 0.2806086*verb2 - 0.6786813*verb4 +  0.4633304*verb6
    verbcub <- -0.4010038*verb1 + 0.7518821*verb2 - 0.5012547*verb4 +  0.1503764*verb6
    verbave <- (verb1 + verb2 + verb4 + verb6)/4
})

# The Anova function in the car package is needed to calculate Type III SS
library(car)
# Regress trend scores on the constant of 1 and grad
mg3 <- lm(verblin ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg3,type=3)

mg4 <- lm(verbqua ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg4,type=3)

mg5 <- lm(verbcub ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg5,type=3)

mg6 <- lm(verbave ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg6,type=3)

############################################################################################################
# Repeated Measures ANOVA with Multiple Groups - Method 3
############################################################################################################
# Compound symmetry
mg7 <- gls(verb ~ time*grad, corr = corCompSymm(form = ~1|id), method="ML", data=wiscraw_long)
anova(mg7)
# Same F-values as Method 1 (Type II SS)
summary(mg7)

# Unstructured 
mg8 <- gls(verb ~ time*grad, corr = corSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time), 
           method="ML", data=wiscraw_long)
anova(mg8)
# F values are smaller, because we are estimating more parameters, which leads to lower power
summary(mg8)

# compare the two models
anova(mg7,mg8)
# UN fits better than CS

# testing contrasts
contrasts(wiscraw_long$time) <- contr.poly(4,scores=c(1,2,4,6)) 
mg9 <- gls(verb ~ time*grad, corr = corSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time), 
           method="ML", data=wiscraw_long)
summary(mg9)
# These results correspond to the MANOVA approach


# Plot the means with error bars
library(emmeans)
library(ggplot2)

# based on mg7
mg7_means <- data.frame(emmeans(mg7, ~ time*grad))

ggplot(mg7_means, aes(x = time, y = emmean, color = grad)) +
    geom_point(position = position_dodge(0.3), size = 3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.2,
                  position = position_dodge(0.3),
                  linewidth = 1) +
    geom_line(aes(group = grad), position = position_dodge(0.3), linewidth = 1) +
    labs(
        title = "Mean ± 95% CI Over Time Based on CS",
        x = "Time",
        y = "Verbal Score Means"
    ) +
    scale_color_manual(
        values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
        labels = c("0" = "NonGrad", "1" = "Grad")) +
    theme(plot.title = element_text(hjust = 0.5))
# note that the error bars have the same width across time


# based on mg9
mg9_means <- data.frame(emmeans(mg9, ~ time*grad))

ggplot(mg9_means, aes(x = time, y = emmean, color = grad)) +
    geom_point(position = position_dodge(0.3), size = 3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.2,
                  position = position_dodge(0.3),
                  linewidth = 1) +
    geom_line(aes(group = grad), position = position_dodge(0.3), linewidth = 1) +
    labs(
        title = "Mean ± 95% CI Over Time Based on UN",
        x = "Time",
        y = "Verbal Score Means"
    ) +
    scale_color_manual(
        values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
        labels = c("0" = "NonGrad", "1" = "Grad")) +
    theme(plot.title = element_text(hjust = 0.5))
# note that the error bars have different width across time

############################################################################################################
# Setting Up Other Types of Contrasts
############################################################################################################
# Manually creating linear*grad contrast
emm <- emmeans(mg9, ~ time*grad)
contrast(emm,list(lin_g=c(-0.5858501, -0.3254723, 0.1952834, 0.7160390,0.5858501, 0.3254723, -0.1952834, -0.7160390)))
# same as in mg9


# Comparing the two groups in the difference between verb1 and verb2
contrast(emm,list(diff12g=c(-1,1,0,0,1,-1,0,0)))
# Not significant. Hence, both groups had similar increase in verbal scores between time 1 and time 2.
# The adjust argument can be used to adjust for multiple comparisons.

=======
############################################################################################################
# Week 4: Repeated Measures ANOVA II
############################################################################################################
# This script contains two parts. The first part provides codes for setting up covariance pattern models.
# The second part provides 3 ways to conduct repeated measures ANOVA with multiple groups.

# Inputing data
wiscraw <- read.csv('C:/Users/siwei-admin/Dropbox/HDE 205/WISC data/wisc3raw.csv')

# Transform from wide to long
list1 <- 2:5 # indicators for verb
list2<- 6:9 # indicators for perfo
wiscraw_long <- reshape(wiscraw,idvar="id",v.names=c("verb","perfo"),varying=list(list1,list2),times=c(1,2,4,6), 
                        direction="long")
wiscraw_long <- wiscraw_long[order(wiscraw_long$id),] # sort data by id

############################################################################################################
# Covariance Pattern Models
############################################################################################################
# descriptive statistics for verbal scores
library(psych)
describe(wiscraw[,2:5])
# Both the mean and sd increase over time

# convert variables to factors (i.e., categorical variables)
wiscraw_long <- within(wiscraw_long, {
    time <- factor(time)
    grad <- factor(grad)
    id <- factor(id)
})

library(nlme)

# Compound symmetry
m1 <- gls(verb ~ time, corr = corCompSymm(form = ~1|id), method="ML", data=wiscraw_long)
summary(m1) # get the fixed effects parameters and fit indices
getVarCov(m1) # get the random effects parameters
cov2cor(getVarCov(m1))

# AR1 pattern
m2 <- gls(verb ~ time, corr = corAR1(form = ~1|id), method="ML", data=wiscraw_long)
summary(m2)
getVarCov(m2)
cov2cor(getVarCov(m2))

# CS with heterogeneous variances
m3 <- gls(verb ~ time, corr = corCompSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time),
          method="ML",data=wiscraw_long)
summary(m3)
getVarCov(m3)
cov2cor(getVarCov(m3))

# AR1 with heterogeneous variances
m4 <- gls(verb ~ time, corr = corAR1(form = ~1|id), weights = varIdent(form = ~ 1 | time),
          method="ML",data=wiscraw_long)
summary(m4)
getVarCov(m4)
cov2cor(getVarCov(m4))

# Unstructured
m5 <- gls(verb ~ time, corr = corSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time), 
          method="ML",data=wiscraw_long)
summary(m5)
getVarCov(m5)
cov2cor(getVarCov(m5))

# Compare all 5 models
anova(m1,m2,m3,m4,m5)
# AIC picks UN, BIC picks CSH

anova(m3)
anova(m5)
# Model with CSH pattern has higher power

# Useful resources: https://stats.oarc.ucla.edu/r/seminars/repeated-measures-analysis-with-r/


############################################################################################################
# Repeated Measures ANOVA with Multiple Groups - Method 1
############################################################################################################
table(wiscraw$grad)
# Most mothers did not graduate from high school

# run anova with multiple groups
library(ez) 
mg1 <- ezANOVA(data=wiscraw_long, dv=verb, wid=id, within=time, between=grad)
# ezANOVA produced a warning that the value of "type" needs to be carefully selected
# There are three types of SS:
# Type I is generally not recommended for unbalanced data
# Type II is generally recommended for unbalanced data, especially when the focus is on the main effects
# Type III should be used with unbalanced data when interactions are suspected to be present

# By default, ezANOVA uses Type II SS
mg1
# Sphericity is not satisfied and is severely violated.
# All effects are significant.

# Because the interaction is significant, Type III SS is more appropriate
mg1_type3 <- ezANOVA(data=wiscraw_long, dv=verb, wid=id, within=time, between=grad, type=3)
mg1_type3
# These should be identical to SAS and SPSS outputs

# Unfortunately, the aov function for testing contrasts assumes balanced data, which is not appropriate here
# Hence, we will not test for contrasts using Method 1

############################################################################################################
# Repeated Measures ANOVA with Multiple Groups - Method 2
############################################################################################################
# Obtain contrast coefficients
contr.poly(4,scores=c(1,2,4,6))

# Create trend scores variables using orthogonal effect codes 
wiscraw <- within(wiscraw,{
    verblin <- -0.5858501*verb1 - 0.3254723*verb2 + 0.1952834*verb4 +  0.7160390*verb6
    verbqua <-  0.4959593*verb1 - 0.2806086*verb2 - 0.6786813*verb4 +  0.4633304*verb6
    verbcub <- -0.4010038*verb1 + 0.7518821*verb2 - 0.5012547*verb4 +  0.1503764*verb6
    verbave <- (verb1 + verb2 + verb4 + verb6)/4
})

# The Anova function in the car package is needed to calculate Type III SS
library(car)
# Regress trend scores on the constant of 1 and grad
mg3 <- lm(verblin ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg3,type=3)

mg4 <- lm(verbqua ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg4,type=3)

mg5 <- lm(verbcub ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg5,type=3)

mg6 <- lm(verbave ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg6,type=3)

############################################################################################################
# Repeated Measures ANOVA with Multiple Groups - Method 3
############################################################################################################
# Compound symmetry
mg7 <- gls(verb ~ time*grad, corr = corCompSymm(form = ~1|id), method="ML", data=wiscraw_long)
anova(mg7)
# Same F-values as Method 1 (Type II SS)
summary(mg7)

# Unstructured 
mg8 <- gls(verb ~ time*grad, corr = corSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time), 
           method="ML", data=wiscraw_long)
anova(mg8)
# F values are smaller, because we are estimating more parameters, which leads to lower power
summary(mg8)

# compare the two models
anova(mg7,mg8)
# UN fits better than CS

# testing contrasts
contrasts(wiscraw_long$time) <- contr.poly(4,scores=c(1,2,4,6)) 
mg9 <- gls(verb ~ time*grad, corr = corSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time), 
           method="ML", data=wiscraw_long)
summary(mg9)
# These results correspond to the MANOVA approach


# Plot the means with error bars
library(emmeans)
library(ggplot2)

# based on mg7
mg7_means <- data.frame(emmeans(mg7, ~ time*grad))

ggplot(mg7_means, aes(x = time, y = emmean, color = grad)) +
    geom_point(position = position_dodge(0.3), size = 3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.2,
                  position = position_dodge(0.3),
                  linewidth = 1) +
    geom_line(aes(group = grad), position = position_dodge(0.3), linewidth = 1) +
    labs(
        title = "Mean ± 95% CI Over Time Based on CS",
        x = "Time",
        y = "Verbal Score Means"
    ) +
    scale_color_manual(
        values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
        labels = c("0" = "NonGrad", "1" = "Grad")) +
    theme(plot.title = element_text(hjust = 0.5))
# note that the error bars have the same width across time


# based on mg9
mg9_means <- data.frame(emmeans(mg9, ~ time*grad))

ggplot(mg9_means, aes(x = time, y = emmean, color = grad)) +
    geom_point(position = position_dodge(0.3), size = 3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.2,
                  position = position_dodge(0.3),
                  linewidth = 1) +
    geom_line(aes(group = grad), position = position_dodge(0.3), linewidth = 1) +
    labs(
        title = "Mean ± 95% CI Over Time Based on UN",
        x = "Time",
        y = "Verbal Score Means"
    ) +
    scale_color_manual(
        values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
        labels = c("0" = "NonGrad", "1" = "Grad")) +
    theme(plot.title = element_text(hjust = 0.5))
# note that the error bars have different width across time

############################################################################################################
# Setting Up Other Types of Contrasts
############################################################################################################
# Manually creating linear*grad contrast
emm <- emmeans(mg9, ~ time*grad)
contrast(emm,list(lin_g=c(-0.5858501, -0.3254723, 0.1952834, 0.7160390,0.5858501, 0.3254723, -0.1952834, -0.7160390)))
# same as in mg9


# Comparing the two groups in the difference between verb1 and verb2
contrast(emm,list(diff12g=c(-1,1,0,0,1,-1,0,0)))
# Not significant. Hence, both groups had similar increase in verbal scores between time 1 and time 2.
# The adjust argument can be used to adjust for multiple comparisons.

>>>>>>> 80ecabd (fix conflicts)
=======
############################################################################################################
# Week 4: Repeated Measures ANOVA II
############################################################################################################
# This script contains two parts. The first part provides codes for setting up covariance pattern models.
# The second part provides 3 ways to conduct repeated measures ANOVA with multiple groups.

# Inputing data
wiscraw <- read.csv('../hde205/data/wisc3raw.csv')

# Transform from wide to long
list1 <- 2:5 # indicators for verb
list2<- 6:9 # indicators for perfo
wiscraw_long <- reshape(wiscraw,idvar="id",v.names=c("verb","perfo"),varying=list(list1,list2),times=c(1,2,4,6), 
                        direction="long")
wiscraw_long <- wiscraw_long[order(wiscraw_long$id),] # sort data by id

############################################################################################################
# Covariance Pattern Models
############################################################################################################
# descriptive statistics for verbal scores
library(psych)
describe(wiscraw[,2:5])
# Both the mean and sd increases over time

# convert variables to factors (i.e., categorical variables)
wiscraw_long <- within(wiscraw_long, {
  time <- factor(time)
  grad <- factor(grad)
  id <- factor(id)
})

library(nlme)

# Compound symmetry
m1 <- gls(verb ~ time, corr = corCompSymm(form = ~1|id), method="ML", data=wiscraw_long)
summary(m1) # get the fixed effects parameters and fit indices
# estimated covariance matrix. all diagonal and off-diagonal elements are the same
getVarCov(m1) # get the random effects parameters
cov2cor(getVarCov(m1))

# AR1 pattern
# the estimates should be the same as the CS model, but the standard errors should be different
m2 <- gls(verb ~ time, corr = corAR1(form = ~1|id), method="ML", data=wiscraw_long)
summary(m2)
getVarCov(m2)
cov2cor(getVarCov(m2))

# CS with heterogeneous variances
# allows the variances to be different
m3 <- gls(verb ~ time, corr = corCompSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time),
          method="ML",data=wiscraw_long)
summary(m3)
getVarCov(m3)
# the correlation is still a CS model, we are just allowing the variances to change
cov2cor(getVarCov(m3))

# AR1 with heterogeneous variances
m4 <- gls(verb ~ time, corr = corAR1(form = ~1|id), weights = varIdent(form = ~ 1 | time),
          method="ML",data=wiscraw_long)
summary(m4)
getVarCov(m4)
cov2cor(getVarCov(m4))

# Unstructured
m5 <- gls(verb ~ time, corr = corSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time), 
          method="ML",data=wiscraw_long)
summary(m5)
getVarCov(m5)
cov2cor(getVarCov(m5))

# Compare all 5 models
# LRT can only be used to test nested models. That is why there are only 2 tests
anova(m1,m2,m3,m4,m5)
# AIC picks UN, BIC picks CSH

anova(m3)
anova(m5)
# Model with CSH pattern has higher power

# Useful resources: https://stats.oarc.ucla.edu/r/seminars/repeated-measures-analysis-with-r/


############################################################################################################
# Repeated Measures ANOVA with Multiple Groups - Method 1
############################################################################################################
table(wiscraw$grad)
# Most mothers did not graduate from high school

# run anova with multiple groups
library(ez) 
mg1 <- ezANOVA(data=wiscraw_long, dv=verb, wid=id, within=time, between=grad)
# ezANOVA produced a warning that the value of "type" needs to be carefully selected
# There are three types of SS:
# Type I is generally not recommended for unbalanced data
# Type II is generally recommended for unbalanced data, especially when the focus is on the main effect
# Type III should be used with unbalanced data when interactions are suspected to be present

# By default, ezANOVA uses Type II SS
mg1
# Sphericity is not satisfied and severely violated.
# All effects are significant.

# Because the interaction is significant, Type III SS is more appropriate
mg1_type3 <- ezANOVA(data=wiscraw_long, dv=verb, wid=id, within=time, between=grad, type=3)
mg1_type3
# These should be identical to SAS and SPSS outputs

# Unfortunately, the aov function for testing contrasts assumes balanced data, which is not appropriate here
# Hence, we will not test for contrasts using Method 1

############################################################################################################
# Repeated Measures ANOVA with Multiple Groups - Method 2
############################################################################################################
# Obtain contrast coefficients
contrasts(wiscraw_long$time)

# Create trend scores variables using orthogonal effect codes 
wiscraw <- within(wiscraw,{
  verblin <- -0.5858501*verb1 - 0.3254723*verb2 + 0.1952834*verb4 +  0.7160390*verb6
  verbqua <-  0.4959593*verb1 - 0.2806086*verb2 - 0.6786813*verb4 +  0.4633304*verb6
  verbcub <- -0.4010038*verb1 + 0.7518821*verb2 - 0.5012547*verb4 +  0.1503764*verb6
  verbave <- (verb1 + verb2 + verb4 + verb6)/4
})

# The Anova function in the car package is needed to calculate Type III SS
library(car)
# Regress trend scores on the constant of 1 and grad
mg3 <- lm(verblin ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg3,type=3)
# depending on the mothers education, the linear trajectory can change?

mg4 <- lm(verbqua ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg4,type=3)
# constant not sig, means that there is no overall quadratic effect. but the quadratic by group interaction
# is significant. on average, the quadratic component is non-sig, but the groups
# are different in the quad effect.

mg5 <- lm(verbcub ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg5,type=3)
# sig cubic trend overall but no group by cube interaction
# 
mg6 <- lm(verbave ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg6,type=3)

############################################################################################################
# Repeated Measures ANOVA with Multiple Groups - Method 3
############################################################################################################
# removing the contrasts set up earlier
contrasts(wiscraw_long$time) <- NULL

# Compound symmetry
mg7 <- gls(verb ~ time*grad, corr = corCompSymm(form = ~1|id), method="ML", data=wiscraw_long)
anova(mg7)
# Same F-values as Method 1 (Type II SS)
summary(mg7)

# Unstructured 
mg8 <- gls(verb ~ time*grad, corr = corSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time), 
          method="ML", data=wiscraw_long)
anova(mg8)
# F values are smaller, because we are estimating more parameters, which leads to lower power
summary(mg8)

anova(mg7,mg8)
# UN fits better than CS

# testing contrasts
contrasts(wiscraw_long$time) <- contr.poly(4,scores=c(1,2,4,6)) 
mg9 <- gls(verb ~ time*grad, corr = corSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time), 
           method="ML", data=wiscraw_long)
summary(mg9)
# These results correspond to the MANOVA approach


# Plot the means with error bars
library(emmeans)
library(ggplot2)

# based on mg7
mg7_means <- data.frame(emmeans(mg7, ~ time*grad))

ggplot(mg7_means, aes(x = time, y = emmean, color = grad)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2,
                position = position_dodge(0.3),
                linewidth = 1) +
  geom_line(aes(group = grad), position = position_dodge(0.3), linewidth = 1) +
  labs(
    title = "Mean ± 95% CI Over Time Based on CS",
    x = "Time",
    y = "Verbal Score Means"
  ) +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("0" = "NonGrad", "1" = "Grad")) +
  theme(plot.title = element_text(hjust = 0.5))
# error bars for each group are equal across time because we're assuming CS

# based on mg9
mg9_means <- data.frame(emmeans(mg9, ~ time*grad))

ggplot(mg9_means, aes(x = time, y = emmean, color = grad)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2,
                position = position_dodge(0.3),
                linewidth = 1) +
  geom_line(aes(group = grad), position = position_dodge(0.3), linewidth = 1) +
  labs(
    title = "Mean ± 95% CI Over Time Based on UN",
    x = "Time",
    y = "Verbal Score Means"
  ) +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("0" = "NonGrad", "1" = "Grad")) +
  theme(plot.title = element_text(hjust = 0.5))
# the points are in the same position as before because the mean estimates 
# are the same, but the error bars now are allowed to be different.

############################################################################################################
# Setting Up Other Types of Contrasts
############################################################################################################
# Manually creating linear*grad contrast
emm <- emmeans(mg9, ~ time*grad)
contrast(emm,list(lin_g=c(-0.5858501, -0.3254723, 0.1952834, 0.7160390,0.5858501, 0.3254723, -0.1952834, -0.7160390)))
# same as in mg9


# Comparing the two groups in the difference between verb1 and verb2
contrast(emm,list(diff12g=c(-1,1,0,0,1,-1,0,0)))
# Not significant. Hence, both groups had similar increase in verbal scores between time 1 and time 2.

############################################################################################################
# Week 4: Repeated Measures ANOVA II
############################################################################################################
# This script contains two parts. The first part provides codes for setting up covariance pattern models.
# The second part provides 3 ways to conduct repeated measures ANOVA with multiple groups.

# Inputing data
wiscraw <- read.csv('C:/Users/siwei-admin/Dropbox/HDE 205/WISC data/wisc3raw.csv')

# Transform from wide to long
list1 <- 2:5 # indicators for verb
list2<- 6:9 # indicators for perfo
wiscraw_long <- reshape(wiscraw,idvar="id",v.names=c("verb","perfo"),varying=list(list1,list2),times=c(1,2,4,6), 
                        direction="long")
wiscraw_long <- wiscraw_long[order(wiscraw_long$id),] # sort data by id

############################################################################################################
# Covariance Pattern Models
############################################################################################################
# descriptive statistics for verbal scores
library(psych)
describe(wiscraw[,2:5])
# Both the mean and sd increase over time

# convert variables to factors (i.e., categorical variables)
wiscraw_long <- within(wiscraw_long, {
    time <- factor(time)
    grad <- factor(grad)
    id <- factor(id)
})

library(nlme)

# Compound symmetry
m1 <- gls(verb ~ time, corr = corCompSymm(form = ~1|id), method="ML", data=wiscraw_long)
summary(m1) # get the fixed effects parameters and fit indices
getVarCov(m1) # get the random effects parameters
cov2cor(getVarCov(m1))

# AR1 pattern
m2 <- gls(verb ~ time, corr = corAR1(form = ~1|id), method="ML", data=wiscraw_long)
summary(m2)
getVarCov(m2)
cov2cor(getVarCov(m2))

# CS with heterogeneous variances
m3 <- gls(verb ~ time, corr = corCompSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time),
          method="ML",data=wiscraw_long)
summary(m3)
getVarCov(m3)
cov2cor(getVarCov(m3))

# AR1 with heterogeneous variances
m4 <- gls(verb ~ time, corr = corAR1(form = ~1|id), weights = varIdent(form = ~ 1 | time),
          method="ML",data=wiscraw_long)
summary(m4)
getVarCov(m4)
cov2cor(getVarCov(m4))

# Unstructured
m5 <- gls(verb ~ time, corr = corSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time), 
          method="ML",data=wiscraw_long)
summary(m5)
getVarCov(m5)
cov2cor(getVarCov(m5))

# Compare all 5 models
anova(m1,m2,m3,m4,m5)
# AIC picks UN, BIC picks CSH

anova(m3)
anova(m5)
# Model with CSH pattern has higher power

# Useful resources: https://stats.oarc.ucla.edu/r/seminars/repeated-measures-analysis-with-r/


############################################################################################################
# Repeated Measures ANOVA with Multiple Groups - Method 1
############################################################################################################
table(wiscraw$grad)
# Most mothers did not graduate from high school

# run anova with multiple groups
library(ez) 
mg1 <- ezANOVA(data=wiscraw_long, dv=verb, wid=id, within=time, between=grad)
# ezANOVA produced a warning that the value of "type" needs to be carefully selected
# There are three types of SS:
# Type I is generally not recommended for unbalanced data
# Type II is generally recommended for unbalanced data, especially when the focus is on the main effects
# Type III should be used with unbalanced data when interactions are suspected to be present

# By default, ezANOVA uses Type II SS
mg1
# Sphericity is not satisfied and is severely violated.
# All effects are significant.

# Because the interaction is significant, Type III SS is more appropriate
mg1_type3 <- ezANOVA(data=wiscraw_long, dv=verb, wid=id, within=time, between=grad, type=3)
mg1_type3
# These should be identical to SAS and SPSS outputs

# Unfortunately, the aov function for testing contrasts assumes balanced data, which is not appropriate here
# Hence, we will not test for contrasts using Method 1

############################################################################################################
# Repeated Measures ANOVA with Multiple Groups - Method 2
############################################################################################################
# Obtain contrast coefficients
contr.poly(4,scores=c(1,2,4,6))

# Create trend scores variables using orthogonal effect codes 
wiscraw <- within(wiscraw,{
    verblin <- -0.5858501*verb1 - 0.3254723*verb2 + 0.1952834*verb4 +  0.7160390*verb6
    verbqua <-  0.4959593*verb1 - 0.2806086*verb2 - 0.6786813*verb4 +  0.4633304*verb6
    verbcub <- -0.4010038*verb1 + 0.7518821*verb2 - 0.5012547*verb4 +  0.1503764*verb6
    verbave <- (verb1 + verb2 + verb4 + verb6)/4
})

# The Anova function in the car package is needed to calculate Type III SS
library(car)
# Regress trend scores on the constant of 1 and grad
mg3 <- lm(verblin ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg3,type=3)

mg4 <- lm(verbqua ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg4,type=3)

mg5 <- lm(verbcub ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg5,type=3)

mg6 <- lm(verbave ~ -1 + constant + as.numeric(grad), data=wiscraw) # "-1" remove the intercept, which is redundant
Anova(mg6,type=3)

############################################################################################################
# Repeated Measures ANOVA with Multiple Groups - Method 3
############################################################################################################
# Compound symmetry
mg7 <- gls(verb ~ time*grad, corr = corCompSymm(form = ~1|id), method="ML", data=wiscraw_long)
anova(mg7)
# Same F-values as Method 1 (Type II SS)
summary(mg7)

# Unstructured 
mg8 <- gls(verb ~ time*grad, corr = corSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time), 
           method="ML", data=wiscraw_long)
anova(mg8)
# F values are smaller, because we are estimating more parameters, which leads to lower power
summary(mg8)

# compare the two models
anova(mg7,mg8)
# UN fits better than CS

# testing contrasts
contrasts(wiscraw_long$time) <- contr.poly(4,scores=c(1,2,4,6)) 
mg9 <- gls(verb ~ time*grad, corr = corSymm(form = ~1|id), weights = varIdent(form = ~ 1 | time), 
           method="ML", data=wiscraw_long)
summary(mg9)
# These results correspond to the MANOVA approach


# Plot the means with error bars
library(emmeans)
library(ggplot2)

# based on mg7
mg7_means <- data.frame(emmeans(mg7, ~ time*grad))

ggplot(mg7_means, aes(x = time, y = emmean, color = grad)) +
    geom_point(position = position_dodge(0.3), size = 3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.2,
                  position = position_dodge(0.3),
                  linewidth = 1) +
    geom_line(aes(group = grad), position = position_dodge(0.3), linewidth = 1) +
    labs(
        title = "Mean ± 95% CI Over Time Based on CS",
        x = "Time",
        y = "Verbal Score Means"
    ) +
    scale_color_manual(
        values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
        labels = c("0" = "NonGrad", "1" = "Grad")) +
    theme(plot.title = element_text(hjust = 0.5))
# note that the error bars have the same width across time


# based on mg9
mg9_means <- data.frame(emmeans(mg9, ~ time*grad))

ggplot(mg9_means, aes(x = time, y = emmean, color = grad)) +
    geom_point(position = position_dodge(0.3), size = 3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.2,
                  position = position_dodge(0.3),
                  linewidth = 1) +
    geom_line(aes(group = grad), position = position_dodge(0.3), linewidth = 1) +
    labs(
        title = "Mean ± 95% CI Over Time Based on UN",
        x = "Time",
        y = "Verbal Score Means"
    ) +
    scale_color_manual(
        values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
        labels = c("0" = "NonGrad", "1" = "Grad")) +
    theme(plot.title = element_text(hjust = 0.5))
# note that the error bars have different width across time

############################################################################################################
# Setting Up Other Types of Contrasts
############################################################################################################
# Manually creating linear*grad contrast
emm <- emmeans(mg9, ~ time*grad)
contrast(emm,list(lin_g=c(-0.5858501, -0.3254723, 0.1952834, 0.7160390,0.5858501, 0.3254723, -0.1952834, -0.7160390)))
# same as in mg9


# Comparing the two groups in the difference between verb1 and verb2
contrast(emm,list(diff12g=c(-1,1,0,0,1,-1,0,0)))
# Not significant. Hence, both groups had similar increase in verbal scores between time 1 and time 2.
# The adjust argument can be used to adjust for multiple comparisons.
