####################################################
##                                                ##
##                   103B Lab 9                   ##
##              Multiple Regression               ##
##                                                ##
####################################################

# Disable scientific notation
options(scipen=999)

# Load tidyverse package & set working directory
library(tidyverse)

# setwd("")

# Today, we will use a dataset called cereal. This is
# a dataset of many different types of cereals, with several
# variables related to the nutritional value of each of them. We also
# have a "rating" value for each cereal. Let's read that in
# and take a quick look::
cereal <- read_csv('cereal_103B.csv')
glimpse(cereal)

# Let's look specifically at this rating variable with a histogram:
hist(cereal$rating)


# This cereal dataset is real data that I got online
# (except for the Sodium and Calories level variables, that
# I added later for a different class, based on the continuous
# sodium and calories variables). But it didn't come with a very
# good explanation of where this information came from.
# I've looked around specifically to find out what the "ratings" are,
# and the best I could find was someone saying they came from
# a Consumer Reports report. If you are unfamiliar with CR,
# they rate all types of products on various important characteristics
# and then give it a final score. So it sounds like that final
# score is what these ratings are.

# It's safe to assume that characteristics of each cereal may have
# influenced the rating it received, right? For example,
# maybe the rated more sugary cereals higher, or maybe
# since cereal is typically a morning meal,
# its fiber content might be more important to perceptions
# of it. So let's see if we can predict which of these factors
# contribute to higher ratings!

# We could use a single predictor in a simple regression,
# but it's reasonable to assume that more than one thing
# impacts whether a cereal is rated high or not.

# Multiple regression allows us to use more than one variable (predictor)
# to predict a specific outcome (cereal rating).

# Modifying the regression equation from simple to multiple regression
# is pretty simple:

# Y_i = B_0 +  b_1 * X_1i + b_2 * X_2i + e
# For each predictor we add, we get another slope

# In R it's also equally as simple.
# Let's see if calories and sugars are good
# predictors of cereal ratings:
fit1 <- lm(rating ~ calories + sugars, data = cereal)
summary(fit1)

# How can we interpret the output?
# Remember that the intercept represents the cereal rating
# when *each* of the predictors are 0.
# The predicted ratings for the cereal is 84.22 when
# the numbers of calories is 0 and sugars are 0.

# The slopes for each of the predictors represent the increase
# in the outcome (ratings) for a one unit increase in predictor 1
# (calories) holding predictor 2 (sugars) constant.

# The slope for calories is -.27.
# This means that holding sugars constant, increasing the calories
# by 1 point *decreases* the predicted rating by .27 points!

# The slope for sugars is -1.77. This means that holding
# calories constant, increasing the sugars by 1 point
# *decreases* the cereal rating by 1.77 points.

# Are these slopes significant? What does that mean?

# Let's look at the R squared. We use the Adjusted R-squared here.
# the R squared is .6715. Who can tell me what that means?

# This means that sugar and calories accounts for 67.15% of
# the variation in cereal ratings. That's very high!


# Centering ------
# #################@

# Sometimes (very often in psychology) we don't care about making
# inferences when the values of the predictors are zero, especially
# when many of our predictors don't have a meaningful zero value.
# In the example here, where would we even find a cereal with zero
# calories? That's not useful. In this case, we may want to use centering,
# which we briefly covered in class. We'll learn how to do that in R now.

# As we saw in the lectures,
# centering shifts all the values of a variable up or down so that the
# distribution of values remains the same, but the mean of that
# distribution changes. Commonly, we center the distribution on 0,
# such that the average value in the data becomes 0. Let's try this:

# Let's see what the calories variable looks
# like with a histogram:
hist(cereal$calories)

# To center it on the mean, we subtract its mean from the variable:
cereal$calories - mean(cereal$calories)

# That's not very useful, let's plot it as a histogram:
hist(cereal$calories - mean(cereal$calories))

# If you compare the two histograms, you can see they are
# the same, except that the mean of the first one is somewhere
# around 100, while the mean of the second one is zero.

# We'll want to keep this new variable,
# so lets add it to our dataset as calories_c.
cereal <- cereal %>%
  mutate(calories_c = calories - mean(calories),
         sugars_c = sugars - mean(sugars))


# Now let's fit the same model again (ratings predicted by
# calories and sugars), but with our centered predictors
# (calories_c and sugars_c):
fit2 <- lm(rating ~ calories_c + sugars_c, data = cereal)
summary(fit2)

# And compare it to our model with the original predictors.
summary(fit1)

# What differences do we see?
#
# The only meaningful change is in the intercept. The interpretation
# of the intercept is the same: it's the expected rating
# when the predictors are both zero. However, the zero for each
# predictor now means something different: it's the average observation
# in each variable. So the intercept now is more useful: it represents
# the expected rating for a cereal with average sugars and average calories!

# We can add more predictors if we want, but keep in mind that
# for each predictor we add, our interpretation becomes a little
# bit more complicated. For example, let's try predicting ratings
# from other nutritional variables in our data. Here are
# the variables we have again:

# Give me 3 nutritional variables that are in the dataset.
names(cereal)
# We will center them:
cereal <- cereal %>%
  mutate(vitamins_c = vitamins - mean(vitamins, na.rm = T),
         fat_c = fat - mean(fat, na.rm = T),
         fiber_c = fiber-mean(fiber, na.rm = T))

# And run the model:
fit3 <- lm(rating ~ vitamins_c + fat_c + fiber_c, data = cereal)
summary(fit3)

# How can we interpret the effects here?
# Intercept: Expected rating for a cereal with average vitamin content, average, fat content,
#            and average fiber content
# Vitamins_c: Expected decrease in rating for a 1 unit increase in vitamin content, holding 
#             fat and fiber content constant
# Fat_c: Expected decrease in rating for a 1 unit increase in vitamin content, holding 
#        vitamin and fiber content constant
# Fiber_c: Expected increase in rating for a 1 unit increase in vitamin content, holding 
#        vitamin and fiber content constant

# How would this be different if we hadn't centered the variables?