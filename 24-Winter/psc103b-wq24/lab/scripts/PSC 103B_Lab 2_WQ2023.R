######################################
##              Lab 2:              ##
##        Testing Correlations      ## 
#                 &                 ## 
##      Simple Linear Regression    ##
##                                  ##
##          Spring 2022             ##
######################################

# setwd("Desktop/School/Graduate School/Administrative/Employment/Winter 2023 (PSC 103B)/Labs/Lab 2/")

## Read in Data

# For today's class, we're going to use a fake dataset that I created
# looking at the relation between temperature (in Fahrenheit), 
# ice cream sales, and pool accidents

icecream = read.csv("lab/data/IceCreamSales.csv")


## Covariance and Correlation Review

# Last week, we talked about how to calculate the covariance and 
# correlation between two variables - recall that you can
# do this using the cov() and cor() functions in R

# However, we're often working with more than 2 variables, and 
# are interested in getting a quick summary of their pairwise
# relationships at once, rather than calculating each 
# covariance or correlation individually

# Luckily, we can do this using the exact same functions we used
# last week! 
# The only difference is that instead of inputting our data as
# cov(x = , y = ) or cor(x = , y = ), we have just one data
# argument, which is our entire data frame

# Let's do this for the covariance first

cov(icecream, use = "complete.obs")

# The output of this is called a variance-covariance matrix
# and it describes (as the name suggests) the variances and
# covariances of the variables in your dataset

# The diagonal elements of the variance-covariance matrix
# are the variances of the variables
# Why? Because the way that a variable relates to itself
# is just its spread; you can use the covariance
# equation to see how it becomes the variance
# formula when the variable is the same

# The off-diagonal elements are the covariances between
# each pair of variables
# Notice that this matrix is symmetric
# The covariance between, say, temperature and ice cream sales is the same
# as the covariance of ice cream sales and temperature

# How can we interpret these values? For example, the covariance
# between temperature and sales is 871.37 - what does this tell
# us about their relation?

# Right, we can only tell that the relation between the two
# is positive - that is, the higher the temperature, the more
# ice cream is sold. But is this relation really strong? 
# That's not something we can get from the covariance, and
# is why we turn to correlation instead!

# To get the correlation matrix, we just use the cor() 
# function 

cor(icecream, use = "complete.obs")

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
# between ice cream sales and temperature

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

plot(icecream$Temperature, icecream$Sales)

# Does this relation look linear?

# Yeah it does! So we are justified in using
# the correlation and testing its significance
# Let's proceed with the test

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

cor_estimate = cor(icecream$Temperature, icecream$Sales)

sample_size = nrow(icecream)

# The denominator of our test statistic is an estimate
# of the standard error of the correlation
# Let's calculate that separately since it's a bit long

cor_se = sqrt((1 - cor_estimate^2) / (sample_size - 2))

# Now our test statistic

t_stat = cor_estimate / cor_se

# So we now have our test statistic
# This statistic is distributed as a t-distribution
# with df = N - 2

# What is the df for our test? 10 (12 - 2)

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

cor.test(icecream$Temperature, icecream$Sales, method = "pearson")

# Since there are a few different ways to calculate
# correlation coefficients, we had to specify method = "pearson"
# because that is the method we've learned in class

# Here is an example of how we could write up our results:
# We examined the relation between temperature and 
# ice cream sales in a sample of 12 days using a Pearson's
# correlation coefficient test. There was a significant
# positive correlation between temperature 
# and ice cream sales, r(10) = 0.96, p < .001. 

# Note that we report the correlation coefficient, not
# the test statistic; and that we use r before the df!

#### Simple Linear Regression ####

# In our discussion of covariance and correlation,
# we've only talked about whether the two variables
# are associated with each other - asking if there's an association
# between temperature and sales, which is the same as asking if
# there's an association between sales and temperature
# None of this looks at whether one variable has an effect 
# on the other variable

# That's where linear regression comes in - now we're
# looking at the relation between a dependent
# variable and at least one predictor variable, so we've
# moved to the context of the predictor variable having
# an effect on the dependent variable

# Today's lab is going to focus on one predictor variable,
# which is called simple linear regression. Next week
# will be multiple regression, when we have at least 2
# predictor variables

# For example, let us take our example of temperature
# and ice cream sales and see how temperature *affects*
# ice cream sales

# Let's plot our data again

plot(icecream$Temperature, icecream$Sales)

# What is the equation for the regression line?
# yhat_i = b_0 + b_1 * x_i
# This gives us the predicted value of our outcome
# for a given value of x
# b_0 represents the intercept, which is the value of 
# the outcome when the predictor is 0
# b_1 represents the slope. It represents the expected
# amount of change in our outcome when our predictor
# increases by 1 unit

# We can calculate estimates of these values using
# their formulas

# b_0 = mean_y - b_1 * mean_x
# b_1 = cov(x, y) / var(x)

b1_estimate = cov(icecream$Temperature, icecream$Sales)/var(icecream$Temperature)
b0_estimate = mean(icecream$Sales) - b1_estimate * mean(icecream$Temperature)

b1_estimate
b0_estimate

# Let's write out our rergession line

# Sales_i = -694.37 + 16.72 * Temperature_i

# Now we can also add this regression line to our plot

plot(icecream$Temperature, icecream$Sales)
abline(a = b0_estimate, b = b1_estimate, col = "red") # abline() is a function
# to add a straight line to a plot

# Of course, rather than calculating the values by
# hand, we can simply do it in R

# We use the lm() function (lm stands for "linear model")
# And the function arguments are as follows:
# lm(dependent variable ~ independent variable, data = data_name)
# This might look familiar - it's pretty similar to how we
# conducted our t-tests in R!

simple_regression = lm(Sales ~ Temperature, data = icecream)

# Let's take a look at the output using the summary() function
summary(simple_regression)

# You'll notice that we can get estimates for b_0 ("(Intercept)")
# and for b_1 (Temperature)
# Do those match what we calculated before?
# How do we interpret these values?

# Our intercept is the expected value of the dependent variable
# when the independent variable is 0
# This means that when the temperature is 0 degrees Fahrenheit
# we expect to make -694 dollars in ice cream sales
# (Note that this is a sort of ridiculous value - it never gets 
# to 0 degrees Fahrenheit in Davis, so do we care about
# expected ice cream sales at that temperature? This is why
# centering your predictor is sometimes handy to improve
# interpretation of the intercept!)

# Our slope is the expected change in the dependent variable
# for a 1-unit change in the independent variable
# So as the temperature increases by 1 degree Fahrenheit, we
# expect ice cream sales to increase by $16.72

### Hypothesis Testing for the Slope and R-Squared ####

# You'll notice that in addition to the estimates of the
# intercept and slope, we also have some other information
# In particular, we have a standard error, test statistic,
# and p-value
# These are for testing the null hypothesis that
# the parameter is equal to 0 versus the alternative
# that it's not equal to 0
# Although we get this test for the intercept as well
# that's typically not very interesting to us
# Instead, we're more interested in the test for the slope
# Because that tells us whether the predictor is useful
# in predicting the outcome; otherwise, the slope is no
# different from 0 and there is no linear relation, no useful
# relation between X and Y that we can use to predict Y

# The hypotheses for this test are:
# H0: b_1 = 0
# H1: b_1 != 0

# The t-statistic in this test is distributed as
# a t-dsitribution with N - 2 df. What is the df
# for our example?

# Based on our summary() output, do we reject or fail
# to reject the null hypothesis? What does that tell us?

# We could also create a confidence interval around our estimate
# of the slope using the information provided

# Recall that the formula for a CI is
# Estimate +/- t_crit * SE
# where t_crit is the critical value that cuts off the upper
# 5% of a t-distribution with N - 2 df

t_crit = qt(.025, sample_size - 2, lower.tail = FALSE)

lower_limit = b1_estimate - t_crit * 1.592
upper_limit = b1_estimate + t_crit * 1.592

lower_limit; upper_limit

### R-Squared ###

# One other piece of useful information that you get from
# the summary() output is R-squared, also called
# the coefficient of determination
# R-squared is the proportion of total variability in our
# outcome that is explained by our predictor(s)
# This is calculated as sum of squares regression 
# divided by total sum of squares: SSreg / SStotal
# OR: 1 - (SSresid / SStotal)

# You could manually calculate SSresid and SStotal, but
# I'm not going to bother with that
# You can see the R^2 value in the summary() output, and
# that value is 0.9168. This means that almost 92% of the
# variation in ice cream sales is explained by temperature

# You'll notice that there is also an adjusted R^2
# This is more important in the context of multiple regression
# because as you add more predictors to the model, your
# R^2 will typically increase even if the added predictors
# are just explaining noise; therefore, adjusted R^2 corrects
# for the number of predictors in the model

# Why do we care about R^2? Because  it may be the case
# that we have a significant predictor, but it doesn't
# actually explain a whole lot of the total variance
# So that's why in our write-up of the results, we'd want
# to report both the regression coefficients and R^2

# Here's what that write-up could look like:

# We ran a simple linear regression to determine whether
# temperature predicts ice cream sales
# Temperature did significantly predict
# ice cream sales (b = 16.72, p < .001). Temperature
# explained 91.68% of the total variability in ice cream sales

#### Your Turn ####

# Run a simple linear regression predicting the number of pool
# accidents from the temperature
# Interpret the intercept and slope. Is the slope significant?