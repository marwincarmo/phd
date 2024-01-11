##############################################
######            PSC 103B              ######
######              Lab 3               ######
######.     Confidence Intervals &      ######
######        Multiple Regression       ######
######                                  ######
######    Instructor: Simran Johal      ######
##############################################

#### Read in Data #####

reading = read.csv("Lab3Data.csv",
                   header = TRUE)

# This data is data that I generated based on information 
# provided in: Weigel et al. (2006). Contributions of the 
# home literacy environment to preschool-aged children's 
# emerging literacy and language skills.
# DOI: https://doi.org/10.1080/03004430500063747

# The dataset contains the following variables:

# ParentChildAct: A composite measure of how often parents
# spend time with reading-related activities to their
# children, such as reading aloud to them. This was measured
# in minutes per day

# ChildAge: The child's age (in months)

# 5 different measures of literacy and language skills:
# Print knowledge, reading interest, emergent writing,
# expressive language, and receptive language

# OverallLiteracy: A composite measure I created by
# averaging the 5 measures of literacy/language
# We're going to think of this as the number of points
# on some test (to help our interpretations)

#### Confidence Intervals #####

# We spent some time this week going over how
# to calculate confidence intervals for correlations
# and slopes of linear regression models

# I just wanted to very quickly cover how to find the
# confidence interval for a correlation in R
# And when I talk about multiple regression, I can show
# you how to find the confidence intervals for the slope(s)

# Basically, finding the confidence interval for a correlation
# is super simple
# Remember the cor.test() function we used to test whether
# our correlation was significantly different from 0?
# That function also reports the confidence interval

# So let's say we were interested in the correlation
# between children's age and their literacy skills

cor.test(reading$ChildAge, reading$OverallLiteracy)

# The sample correlation is 0.51, but our confidence
# interval shows that this could've ranged from
# 0.38 to 0.62

#### Multiple Regression #####

# Last week, we learned about simple linear regression
# which is when we are trying to predict or explain
# some outcome variable using a single predictor
# or independent variable

# If you recall the example from last week, we were
# trying to explain how the temperature outside
# predicts or explains the revenue gained from ice
# cream sales, and in that example, temperature
# did a pretty good job at predicting ice cream sales

# But that was a toy example, so let's take one that is
# a bit more realistic in our reading dataset

# Let's say we thought children's literacy skills were only
# predicted by the child's age (parents have no influence)
# We could run that regression model:

simple_reg = lm(OverallLiteracy ~ ChildAge, data = reading)

# and look at the output

summary(simple_reg)

# Review: How do we interpret these values?
# The intercept of 10.33 means that child who is 0 months
# old would be expected to have a literacy score of 10.33
# The slope of 0.20 means that for every month older a child is
# their literacy score is expected to increase by 0.20 points

# The other important piece of info is the R^2 of 0.26
# What does that mean?
# A R^2 value of 0.26 means that 26% of the variation
# in literacy scores is explained by the child's age
# But that means that there is still 74% that is 
# NOT explained by the child's age,
# or in other words, one predictor
# was not sufficient for explaining a decent amount
# of the variability in our outcome 
# and we need to find other variables that could help predict 
# our outcome

# This is where multiple regression comes in: we want to predict
# or explain a single outcome variable using 2 or more
# predictors

# So if this is our simple linear regression model is
# Y = b0 + b1*X
# Multiple regression just adds on more predictors so our
# model is now:
# Y = b0 + b1*X1 + b2*X2 + b3*X3 + ....

# In our case, let's add on just 1 more predictor
# of the amount of time parents spend doing reading-related
# activities with their child

# so before our model was: literacy = b0 + b1*age
# and now our model is: literacy = b0 + b1*age + b2*parent
# We just have to add on the predictor to our model
# and luckily, it's also pretty easy to add it in R

multiple_reg = lm(OverallLiteracy ~ ChildAge + ParentChildAct, 
                  data = reading)

summary(multiple_reg)

# Now we have more terms: we have the intercept and the 
# effect of age, like before
# and now we also have the slope associated with the 
# time parents spend doing reading-related activities

# How can we interpret these terms?
# Like before, the intercept is the expected value
# of our outcome when *all* predictors are 0
# So when a child is 0 months old, and a parent spends
# 0 minutes per day doing reading-related activities with them
# the expected literacy score is 3.37

# What has changed is the interpretation of the slope
# The slope is now the expected change in Y for a 1-unit
# increase in X, *holding the other variable constant*

# So the slope for age means that if we keep the 
# amount of time on parent-child activities constant, then
# increasing child's age by 1 month will
# increase the literacy score by 0.20

# Similarly, the slope for parent-child activities
# means that if we hold the child's age constant, 
# then increasing the time on reading activities by 1 minute
# is expected to increase the child's score by 0.21 points

# Are these slopes significant?
# Yes, both slopes are significant, meaning that 
# they each have a unique effect on the outcome variable

# And we can get the confidence intervals for each
# of the terms in our model:

confint(multiple_reg)

# As a side note: this is why you might read research
# articles that talk about "controlling for" or 
# "adjusting for" other variables like gender or SES
# Basically, all they're doing is running a multiple regression
# model with gender or SES as another predictor
# This is because multiple regression lets us examine the
# unique effect of each variable in our model, given
# all the other variables; therefore, if the slope
# is still significant, it means that there is something
# unique about our predictor that is predicting the outcome

# What is our R2 now? Before, in simple regression, it was 26%
# Now, it has increased to 37%
# But remember what I mentioned last week: every time you
# add predictors to a model, the R2 will generally increase
# This is why, in multiple regression, you report
# the adjusted R2, which penalizes you for having 
# too many predictors in your model

# Our adjusted R2 isn't too different - 36%
# This means that 36% of the variation in literacy
# scores is explained by our predictors (notice
# that we can't separate out the percentage due to 
# each predictor, but just an overall value)

##### Centering Predictors #####

# One thing you might have noticed is that the intercept
# doesn't always make sense
# In our simple regression: Are we ever expected to have 
# a child who is 0 months old?
# No! That child isn't even born yet!

# So the intercept value isn't very useful in this case
# This is why you will often see researchers 
# saying that they centered their predictors before
# doing the regression

# Centering shifts all the values of your variable
# so that the overall distribution stays the same
# but the mean of that distribution is now 0
# Commonly, we center our values using the average
# so that a value of 0 in the centered distribution
# corresponds to the average value of our original dist

# Let's see this. Here is a distribution of the 
# child's age

hist(reading$ChildAge)

# You can see the mean is around 55

# Now we can center this by subtracting the mean from
# all values:

reading$ChildAge - mean(reading$ChildAge)

# Well that's just a lot of values; let's save it and plot it

reading$ChildAge_c = reading$ChildAge - mean(reading$ChildAge)

par(mfrow = c(1, 2))
hist(reading$ChildAge)
hist(reading$ChildAge_c)

# Notice that the shape of the distribution has 
# stayed the same, but the center is no longer
# at around 55 months, but is now at 0

dev.off() # to reset the plotting window

# Let's also center our parent-child activities variable:

reading$ParentChildAct_c = reading$ParentChildAct - mean(reading$ParentChildAct)

# Let's re-run our regression to see what changes

multiple_reg_centered = lm(OverallLiteracy ~ ChildAge_c + ParentChildAct_c,
                           data = reading)

summary(multiple_reg_centered)

# Let's compare this with our previous model
# that we have written on the board

# Notice that our slopes have not changed: the effect 
# of a 1-unit change in a variable, holding the other 
# constant, stays the same

# What has changed is the intercept
# The intercept still represents the expected academic
# performance when all our predictors are 0
# But now our predictors are centered, so instead of 
# Y = b0 + b1*X1 + b2*X2, we have
# Y = b0 + b1*(X1 - mean(X1)) + b2*(X2 - mean(X2))
# So the predictors are only 0 when the variables
# are equal to the average value

# This makes the intercept more useful: it is the expected
# literacy when we have a child who is the average age
# and whose parents spend the average amount of time
# doing reading related activities with them

##### Interactions #####

# One last concept that we will be covering is interactions
# This is a little bit early, because you don't get 
# to interactions until next Monday
# But I figured you guys would rather not have lab next Friday
# so if I cover interactions today, I can cancel next week's lab

# So interactions are how we can determine whether there
# is a moderation effect
# Moderation between 2 predictor variables is like 
# saying "depends on" - the effect of one predictor
# variable "depends on" what value the other predictor takes

# This is often seen with categorical variables like
# bio sex (e.g., the effect of age depends on whether 
# you're biologically male or female)
# But can be done with numeric variables too

# For our example, perhaps the effect of parent-child activities
# depends on the child's age
# Maybe it matters that parents spend time doing these
# activities with their children when their children
# are a little bit older, versus when they're younger
# and parent-child activities has no effect
# Since the effect depends on how old the child is,
# that is a moderation, or there is an interaction
# between child's age and parent-child activities 

# To test an interaction in R, you just need to 
# multiply your 2 predictors together instead of adding
# So it would be like lm(outcome ~ pred1 * pred2)
# where pred1 * pred2 is shorthand for pred1 + pred2 + pred1:pred2

# In our case:

interaction_model = lm(OverallLiteracy ~ ParentChildAct_c*ChildAge_c,
                       data = reading)
summary(interaction_model)

# What does our output tell us:

# Is our interaction term significant? 
# No. So that means the effect of parent-child activities
# does not depend on how old the child is - it's equally
# important at all ages

# I know the effect wasn't significant, but I'll show
# you what to do if you *do* have a significant interaction
# In that case, you want to better understand what this 
# effect is, and it's usually better to graph the 
# regression line between the  outcome and 1 predictor at
# different values of the other predictor
# If your "other predictor" is numeric, this is usually
# done at 3 values: 1 SD below the mean, 1 SD above the mean,
# and the mean

# Let's visualize the effect of parent-child activities
# on literacy at different values of children's age

# We could do this by hand, but I'll leave that to class
# Instead, let's let R do it for us!
# An easy way to do this is with a function from 
# a different package

# install.packages(sjPlot)

library(sjPlot)

# The plot_model() function will plot the regression lines for
# you, including the interaction term
# Basically, all you have to do is specify type = "int"
# And R will plot with the first term in your
# interaction model on the x-axis, and the second term as 
# a grouping factor
# You can choose to plot at different values of your grouping
# variable, so we will specify the mean and +/- 1 SD
# using mdrt.values = "meansd"

# plot_model(model_object, type = "int", mdrt.values = "meansd")
plot_model(interaction_model, type = "int", 
           mdrt.values = "meansd", show.data = TRUE)

# You can see here that the slopes of all 3 lines
# are roughly the same, which is further proof
# that there is no interaction between our predictors

# If there was an interaction, you might see something
# where as the child's age increases, you get different
# slopes for the effect of parent-child activities (e.g,
# maybe the slope also increases)
