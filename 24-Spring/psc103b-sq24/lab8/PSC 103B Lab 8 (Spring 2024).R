######################################
##              Lab 8:              ##
##    Covariance and Correlation    ## 
#                                   ## 
##                                  ##
##          Spring 2024             ##
######################################

## Load Packages 

library(tidyverse)

## Read in Data

# For today's class, we're going to use the same NPAS
# dataset from Lab 5
setwd("~/Library/CloudStorage/Box-Box/PSC 103B/Labs/Lab 8")
nerdy = read.csv("NPAS_Lab5-2.csv")
# Recall the variables in this dataset:

# country/continent: person's country and continent
# nerdy_scale: a person's average score on the NPAS items
# TIPI: Ten Item Personality Inventory (1-7 scale) asking about:
# TIPI1: extraverted/enthusiastic
# TIPI2: critical, quarrelsome
# TIPI3: dependable, self-disciplined
# TIPI4: Anxious, easily upset
# TIPI5: open to new experiences, complex
# TIPI6: reserved, quiet
# TIPI7: sympathetic, warm
# TIPI8: disorganized, careless
# TIPI9: calm, emotionally stable
# TIPI10: convetional, uncreative
# nerdy_selfreport: self-reported nerdiness (1-7 sclae)

# Today, we'll be examining the relations between
# the ten personality items and the nerdiness score on the NPAS

## Covariance and Correlation

# Up until now, when we have worked with more than one variable
# it has typically been that one of the variables is
# categorical, and the other is continuous
# Today, we're going to focus on exploring the relations 
# between two (or more!) continuous (or ordinal) variables

# There are different ways to explore the relation between
# continuous variables - the first one will be
# their covariance and correlation
# This is a good summary statistic to quantify the relation
# without making any statements about the direction
# you expect this relation to be in (e.g, X predicting Y
# versus Y predicting X)

# Recall that in order to get the variance of just
# a single variable, you can use the var() function

var(nerdy$nerdy_scale)

# The output of this function (the variance) quantifies
# the spread of the variable around its mean
# On average, how far is a a value from the mean?

# Now that we're interested in the relation between 
# two variables, we shift from the variance to the
# covariance, which looks at how the spread in one variable
# corresponds to the spread of the other variable
# Or in other words, whether or not the two variables travel
# together or in opposite directions

# Say we're interested in the covariance between how extravered
# someone is and how reserved they are
# We can get their covariance by using the cov() function

cov(nerdy$TIPI1, nerdy$TIPI6)

# What does this tell us about the relation between these
# two variables?

# We can also get all pairwise covariances of multiple
# variables by simply supplying the cov() function
# with a dataframe of those variables
# instead of just 2 variables at a time
# Note that you could also use the var() function here
# and get the same answer

cov(nerdy)

# Wait, we got an error! Why is that?
# It's because some of our variables (mainly, country
# and continent) are not numeric, so we cannot calculate
# a covariance with them

# We need to first subset to the columns we want
# using the select() function; let's for now focus on 
# a smaller subset of variables: nerdy_scale, and TIPI1 - TIPI3


cov(nerdy %>% select(nerdy_scale, TIPI1, TIPI2, TIPI3))
##or in base R if I havent convinced you of tidyverse yet
cov(nerdy[, c(2, 4:6)])

# The diagonal elements of the variance-covariance matrix
# are the variances of the variables
# Why? Because the way that a variable relates to itself
# is just its spread; you can use the covariance
# equation to see how it becomes the variance
# formula when the variable is the same

# The off-diagonal elements are the covariances between
# each pair of variables (ignoring the other variables)
# Notice that this matrix is symmetric
# The covariance between, say, nerdiness and extraversion is the same
# as the covariance of extraversion and nerdiness

# However, the values of covariances are hard to interpret
# It's hard to tell whether a value of -0.293 is meant
# to represent a strong relation, or a weak relation
# This is why we tend to report correlation
# between variables - correlation is just a standardized
# covariance, and since it can only fall between -1 and 1
# it makes it easier to interpret the value as a measure
# of the strength of the relation

# To get the correlation matrix, we just use the cor() 
# function 

cor(nerdy %>% select(nerdy_scale, TIPI1, TIPI2, TIPI3))

# Now let's look at these values - what are they
# telling us about the relations between our variables
# Which relations appear to be strong? Which ones
# appear to be weak?

### Significance Testing for a Correlation ####

# So even though we were able to calculate our
# correlations, we aren't really able to tell whether
# any of these values are representing significant
# relations - significant meaning that the correlation
# coefficient is different from 0
# Since a correlation coefficient of 0 means that
# there is no significant linear relation between the variables

# Let's test if there is a significant correlation
# between NPAS score (nerdy_scale) and how quiet someone
# is (TIPI6)

# First, let's plot our data - we want to make sure
# that using the correlation to quantify the relationship
# between the variables is suitable 
# We wouldn't want to use correlation to measure what
# looks like a non-linear relation, because correlation
# and covariance can only capture linear relations
# We also don't want to necessarily use correlation if
# there are strong outliers in our data, as those
# outliers can change the value of our correlation
# coefficient

ggplot(data = nerdy, aes(x = TIPI6, y = nerdy_scale))+
  geom_point()+
  theme_classic()+
  labs(x = "Reservedness", y = "Nerdiness")

# Note that you could also plot the data using:

plot(nerdy$TIPI6, nerdy$nerdy_scale)

# Ok, everything looks good here, so let's test
# whether the correlation between reservedness and nerdiness is
# significantly different from 0

# Our null and alternative hypotheses are

# H0: rho = 0
# H1: rho != 0

# Notice that this is a two-sided test - either there
# is a relation or there isn't a relation, but we're
# not specifying which direction we expect that relation
# to be in

# In order to calculate our test statistic, we just need
# two things: the sample estimate of the correlation
# and the sample size
# Let's get those things

cor_estimate = cor(nerdy$TIPI6, nerdy$nerdy_scale)

##correlation can be thought of as a standardized covariance
## we standardized by dividing by the standard deviations of the variables

cov<-cov(nerdy$TIPI6, nerdy$nerdy_scale)
sdx <- sd(nerdy$TIPI6)
sdy <- sd(nerdy$nerdy_scale)

cov/(sdx*sdy) ##matches our cor() output


# The denominator of our test statistic is an estimate
# of the standard error of the correlation
# Let's calculate that separately since it's a bit long

sample_size = nrow(nerdy)

cor_se = sqrt((1 - cor_estimate^2) / (sample_size - 2))

# Now our test statistic

t_stat = cor_estimate / cor_se

# So we now have our test statistic
# This statistic is distributed as a t-distribution
# with df = N - 2
# if we wanted to see whether we reject or fail to
# reject our null hypothesis, we could calculate the critical
# value of our distribution and see if our value is larger
# or calculate the p-value
# I'll calculate the p-value

2 * pt(t_stat, df = sample_size - 2, lower.tail = FALSE)

# Notice that I'm multiplying my p-value by 2
# because we're doing a two-sided test

# What do we conclude?

# And here is how we could do it in one line of R code

cor.test(nerdy$TIPI6, nerdy$nerdy_scale, method = "pearson")

# Since there are a few different ways to calculate
# correlation coefficients, we had to specify method = "pearson"
# because that is the method we've learned in class

# Here is an example of how we could writeup our results
# We examined the relation between reservedness and 
# nerdiness in a sample of 1000 people using a Pearson's
# correlation coefficient test. There was a significant
# positive correlation between reservedness 
# and nerdiness, r(998) = 0.23, p < .001. 

# Note that we report the correlation coefficient, not
# the test statistic; and that we use r before the df!