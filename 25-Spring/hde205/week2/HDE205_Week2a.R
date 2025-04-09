############################################################################################################
# Week 2: Measures of Change 
############################################################################################################
# This script provides a framework for calculating three types of 'change' scores,
# [1] Residualized Gain Scores
# [2] Trend Scores
# [3] Individual Curves;

# Load packages
library(QuantPsyc)
library(psych)

# Inputing data
wiscraw <- read.csv('data//wisc3raw.csv',na.strings="NA")

# Creating a reduced data set with only 3 equally spaced occasions 
verb <- wiscraw[,c(1,3:5)] 

############################################################################################################
# [1] Creating residualized gain scores for TWO occasions
############################################################################################################
#### Regressing verb4 on verb2
m1 <- lm(verb4 ~ verb2, data=verb) # fitting the model and storing the results in object 'm1'
summary(m1) # summary of results

# getting standardized coefficient
lm.beta(m1) 
# The standardized coefficient, B1 = .76, can be interpreted as a 'stability index'.

# getting the residuals
resid(m1)
# These are the residualized gain scores.


#### Regressing verb6 on verb4
m2 <- lm(verb6 ~ verb4, data=verb) 
summary(m2) 

# getting standardized coefficient
lm.beta(m2) 
# The standardized coefficient, B1 = .80, can be interpreted as a 'stability index'.

# getting the residuals
resid(m2) 
# These are the residualized gain scores.

# Descriptives of the residualized gain scores
describe(cbind(resid(m1),resid(m2)))

cor.test(resid(m1),resid(m2))


# For longer time interval, we would expect B to be smaller;
### Regressing verb6 on verb2;
m3 <- lm(verb6 ~ verb2, data=verb) 

# getting standardized coefficient
lm.beta(m3)

# the standardized coefficients are just the correlations between occasions
corr.test(verb[,2:4])

############################################################################################################
# [2] Creating trend scores for THREE occasions
############################################################################################################
# Making new data set with trend scores calculated using orthogonal effect codes (for 3 equally spaced occasions)
# See the Table Handout for the effect codes;
verb$verblin <- -1*verb$verb2+0*verb$verb4+1*verb$verb6
verb$verbqua <-  1*verb$verb2-2*verb$verb4+1*verb$verb6

# Descriptives
describe(verb[,5:6])

# Compare with means at the three occasions
describe(verb[,2:4])

# Examining correlations
corr.test(verb[,5:6])

# Another way to get the means of the trend scores is to regress the variable on the constant 1
summary(lm(verblin ~ 1, data=verb))
summary(lm(verbqua ~ 1, data=verb))

############################################################################################################
# [3] Calculating individuals curves for each person
############################################################################################################
# convert wide data to long format
verb_long <- reshape(verb,idvar="id",v.names="verb",varying=c(2:4),times=c(2,4,6), 
                        direction="long")
verb_long <- verb_long[order(verb_long$id),] # sort data by id

#### plot data
library(ggplot2) 

# define base for the graph and store in object 'p'
p <- ggplot(data = verb_long, aes(x = time, y = verb, group = id))

# spaghetti plot
p + geom_point() + geom_line()

#### Running a regression for each individual separately - and saving parameters;
#### Within-person regressions;
#### Equation: verb(t) = B0 + B1*time(t) + e(t);

# create data frame to store individual intercepts and slopes
istats <- data.frame(id=1:204,iintercept=rep(NA,204),islope=rep(NA,204))

# run individual regressions
for (i in 1:204){
    datai <- verb_long[which(verb_long$id == i),] # subset data for each individual

    istats$iintercept[i] <- coefficients(lm(verb~time,data=datai))[1]
    istats$islope[i] <- coefficients(lm(verb~time,data=datai))[2]
    rm(datai)
}

# descriptives and correlation
describe(istats[,2:3])
corr.test(istats[,2:3])

# merge istats with verb_long
verb_long <- merge(verb_long,istats,by="id")

# calculate predicted values using the individual regression parameters obtained above
# Y(t) = B0 + B1*time(t) + e(t);
verb_long$verbipred <- verb_long$iintercept+verb_long$islope*verb_long$time + 0

# plot predicted individual change
p <- ggplot(data = verb_long, aes(x = time, y = verbipred, group = id))
p + geom_point(color="blue") + geom_line(color="blue")
