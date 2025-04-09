############################################################################################################
# Week 2: Measures of Change (Unequal Intervals)
############################################################################################################

# Load packages
library(QuantPsyc)
library(psych)

# Inputing data
wiscraw <- read.csv('C:/Users/siwei-admin/Dropbox/HDE 205/WISC data/wisc3raw.csv',na.strings="NA")

# Creating a reduced data set with only 3 unequally spaced occasions 
verb <- wiscraw[,c(1:4)] 

############################################################################################################
# [2] Creating trend scores for FOUR occasions (unequally spaced)
############################################################################################################
# Making new data set with trend scores calculated using orthogonal effect codes 
contr.poly(3,scores=c(1,2,4)) 

verb$verblin <- -.6172134*verb$verb1-0.1543033*verb$verb2+0.7715167*verb$verb4
verb$verbqua <-  0.5345225*verb$verb1-0.8017837*verb$verb2+0.2672612*verb$verb4

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
verb_long <- reshape(verb,idvar="id",v.names="verb",varying=c(2:4),times=c(1,2,4), 
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
