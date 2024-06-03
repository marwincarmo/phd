######################################### ##
##              Lab 7:                    ##
##        Checking Assumptions            ##
##                and                     ##
##     Dealing with Violations            ##
######################################### ##

# First things first
# Load these, you should already have them from
# previous labs
library(tidyverse)
library(car) 

# This one might be new, you might have to install it:
# install.packages('lmPerm')
library(lmPerm)

setwd('')

# You already know how to run t-tests (dependent + independent),
# One-way ANOVAs, and Factorial ANOVAs in R. 
# Today we're going to be discussing how to 
# check the assumptions for these tests as well as what to do 
# if these assumptions are violated. 

# Independent t-tests, one-way ANOVA, and factorial ANOVA all 
# have 3 assumptions 

# * Independence (of the observations)
# * Normality (of residuals) 
# * Equal variances (aka homogeneity of variance)
# IMPORTANT: These assumptions are about *the population* not the sample 
#              (like our H0 and H1 hypotheses)


# Today we'll walk through assumption testing for each of the tests 

# We'll be using both a Big5 dataset and the 
# clinical trial dataset from the homework
# The names of these files are the following:
#   "Big5_Countries.csv"
#   "FactorialAnova_Mood.csv" 
# Make sure you downloaded those into your working directory

# Let's load in the Big5 dataset first
big5 <- read_csv('Big5_Countries.csv')
head(big5)

# You'll see the following variables:

# E = Extraversion
# A = Agreeableness
# C = Conscientiousness
# N = Neuroticism, or negative emotionality
# O = Openness to experience
# Continent_Name = the continent where the participant was
# Country_Name = the country where the participant was

# This is real dataset adapted from the website 
# https://openpsychometrics.org/_rawdata/
# The original dataset had 19,719 observations, from people who 
# volunteered to take this questionnaire: 
# https://openpsychometrics.org/tests/IPIP-BFFM/
# I highly recommend that you take it, just because it's fun!
# This is a science-backed personality quiz, and it tells
# you your score for each of the Big 5 factors at the end!

# Let's look at observations in the dataset per Continent.
table(big5$Continent_Name)

# You'll notice that there are 100 observations per continent.
# That's because there were an unequal number of respondents
# in the full dataset, with most being from North America
# or Europe and a much smaller number from South America
# or Africa. To make things more equal, I randomly selected
# 100 observations from each continent to form our dataset.


# Independent T-test ----
####################### #

# First let's start with an independent t-test
# For this independent t-test we are interested in whether there is a 
# significant difference between Europe and North America on agreeableness 
# In other words, do people who live in Europe have different levels of 
# agreeableness than people who live in North America?

# An independent t-test would be the right test for this question
# (since we're comparing *two* group means)
# *if* the assumptions of independence, normality, and equal variances hold
# So before we interpret any results the t-test may give us, it's important 
# to know how trustworthy these results are. 

### 1. Independence ----
###################### #
# How can we tell if independence is violated?

# This is something that only the study design
# can answer. In this case, these data were collected
# online, and each data point for a given variable is 
# the score on that Big5 personality trait for a 
# different person. So in this case, we can say that
# the assumption of independence holds.

# If independence were violated what can we do?

# We haven't really learned many ways to get around this
# problem in this class, but we did learn one test
# that can help us in the case that independence
# is violated in a very specific way: the paired
# t-test. This only works if the dependency between
# the observations creates natural pairs of observations.
# For example, pre/post designs, or the same person in
# two different conditions, or pairs of people or
# animals. It's important to remember this test is not
# appropriate for other types of violation of 
# independence, though.


### 2. Equal Variances ----
######################### #
NaEurDat <- big5 %>% 
  filter(Continent_Name == c("North America", "Europe")) 

# Let's take a look at the data:
NaEurDat %>%  
ggplot(aes(x = Continent_Name, y = A)) +
  geom_boxplot(outlier.alpha = 0, aes(color = Continent_Name))+
  geom_jitter(width = .2, alpha = .6) +
  theme_bw() +
  theme(legend.position = "None") +
  ylab("Agreeableness") + 
  xlab("")

# Do the variances look very different?
# How can we test if the assumption of 
# equal variances holds? 

# We can use Bartlett's test, like we saw in class.

# What is the H0 of Bartlett's test? 
# H0: variances are equal
# H1: variances are not equal

# bartlett.test(outcome ~ group, data = data.name)

bartlett.test(A~Continent_Name, data = NaEurDat)

# What does our result tell us about the variance of the groups? 



# There is another test of equal variances we 
# used in lab before, and that's Levene's test.
# Even though we didn't see that one in class, we
# can also use that here. 
# There are some differences between the two
# which we won't get into here but they should give 
# approximately the same results here.
leveneTest(A~Continent_Name, data = NaEurDat)


# What can we do if the assumption of equal variances is violated? 
# Luckily, there are versions of the t-test and ANOVA that do not
# assume (or correct for possible violations of) equality of variances
# For t-test, there is Welch's t-test

# We have seen how to run Welch's t-test before: we just change
# the var.equal argument to FALSE

t.test(A~Continent_Name, data = NaEurDat, var.equal = FALSE) 

# It's fine if you want to just run the Welch's test without checking 
# if the assumption is true. But if you want to run the regular 
# t-test or ANOVA you *have* to check if the assumption of homogeneity 
# holds. 

# The Welch's test still assumes independence and normality
# So if you use the Welch's test you still have to check that the
# residuals are normally distributed. 

### 3. Normality (or residuals) ----
################### #

# Let's move onto the third assumption: normality
# It's important to remember that this assumption
# is about the residuals being normally distributed,
# and not about the variables themselves 
# Residuals are the "error" in your models, the stuff
# that your model doesn't account for in your observations.
ttest <- lm(A~Continent_Name, data = NaEurDat)

# What a t-test "model" looks like: 
NaEurDat %>% 
  rowid_to_column() %>%
  group_by(Continent_Name) %>% 
  mutate(GroupMean = mean(A)) %>% 
  ungroup() %>% 
  ggplot(aes(x = A, y = rowid, group = Continent_Name, color = Continent_Name)) + 
  geom_point() +
  geom_vline(xintercept = 3.790, color = "#F8766D") +
  geom_vline(xintercept = 3.898, color = "#00BFC4") +
  theme(legend.position="bottom") +
  labs(x = "Agreeableness Score", y = "")

# These are the residuals (grey lines)
NaEurDat %>% 
  rowid_to_column() %>%
  group_by(Continent_Name) %>% 
  mutate(GroupMean = mean(A)) %>% 
  ungroup() %>% 
  ggplot(aes(x = A, y = rowid, group = Continent_Name, color = Continent_Name)) + 
  geom_point() +
  geom_vline(xintercept = 3.790, color = "#F8766D") +
  geom_vline(xintercept = 3.898, color = "#00BFC4") +
  geom_segment(aes(x = A, y = rowid, xend = GroupMean, yend = rowid), color = "gray60") +
  theme(legend.position="bottom") +
  labs(x = "Agreeableness Score", y = "")

# We can plot the residuals in a histogram!
# Does it look normally distributed? 
ttest_res <- lm(A~Continent_Name, data = NaEurDat)

ggplot(ttest_res) +
  geom_histogram(aes(x = .resid), 
                 bins = 10, color = "black", fill = "gray60")
 
# Sometimes just visualizing the data makes it hard to tell. 
# These tests are pretty robust to assumption violations.
# This means that even if our data don't perfectly 
# follow the assumptions, the tests can still perform OK.
# Only when there is a severe violation of the assumptions
# does it become a problem. 

# A more objective way to test normality is to use 
# the Shapiro test with shapiro.test() function 

# What's the H0 of the shapiro test? 
# H0: the residuals are normally distributed
# H1: the residuals are not normally distributed

# make sure you provide the residuals of the model, not your data!
# We can get residuals from a lm() object using lm.object$residuals
shapiro.test(ttest_res$residuals) 
# Is the assumption of normality violated? 


# Summary:
# Is independence violated? No
# Is equal variances violated? No
# Is normality violated? Yes

# What test could we use?
# We can use a permutation test, which is a nonparametric test
# meaning it doesn't make any assumptions about the type of 
# distribution our residuals should follow

# In a permutation test, group membership is randomly reshuffled 
# across individuals (this assumes H0 is true)
# and our test statistic is calculated for the new groups
# This is repeated many many times to build a sampling distribution
# based on our data, which we can compare our observed test statistic to
# (Like we do when we put our observed test statistic on a t-distribution!)

# To do a permutation test, we can use the lmp() function
# lmp(outcome ~ group, data = data.name)

ttest_res2 <- lmp(A~Continent_Name, data = NaEurDat)
anova(ttest_res2)

# the Pr(Prob) is our p-value and we interpret it 
# the same way we would interpret our p-value from 
# our t-test 

# What if normality and equal variances were violated?
# We can use either a permutation test or
# the Huber-White correction to address both at once

# One-Way ANOVA ----
################### #

# For the one-way ANOVA let's look at the 
# difference in Extraversion between Africa, South America, and Asia

# Assumptions
#  * Independence (of observations)
#  * Equal Variances 
#  * Normality (of residuals)

### 1. Independence ----
# How can we tell if the independence assumption is violated? 
# What would we do if the independence assumption were violated? 

# We'd have to use models that we didn't learn about in this class
# One option is using repeated-measures ANOVA (like the paired
# t-test but for ANOVA). But just like the paired t-test, that 
# only helps with a very specific type of dependence between
# the observations.

### 2. Equal Variances ---- 
# How could we tell if the equal variances assumption was violated?
# We can again Bartlett's test (or Levene's test).

anovaSub <- big5 %>%
  filter(Continent_Name == c("Africa", "South America", "Asia"))

anovaSub %>%  
  ggplot(aes(x = Continent_Name, y = E)) +
  geom_boxplot(outlier.alpha = 0, aes(color = Continent_Name))+
  geom_jitter(width = .2, alpha = .6) +
  theme_bw() +
  theme(legend.position = "None") +
  ylab("Extraversion") + 
  xlab("")

bartlett.test(E~Continent_Name, data = anovaSub)

leveneTest(E~Continent_Name, data = anovaSub)

# We interpret it the same way we interpret these test for when
# we're checking the assumptions of a t-test

# What if the assumption of equal variances is violated? 
# We can use the Welch's ANOVA if this is violated
# Unfortunately, unlike the Welch's t-test, we can't use the same 
# function. 
# If we want to run a Welch's one-way ANOVA we have to use the 
# oneway.test() function. But the information that goes into 
# this function is the same

oneway.test(E ~ Continent_Name, data = anovaSub, var.equal = FALSE)


### 3. Normality (of residuals) ----
# We can plot the residuals here too.
# Do they look (approximately) normally distributed?
anova_res <- lm(E~Continent_Name, data = anovaSub)

ggplot(anova_res) +
  geom_histogram(aes(x = .resid), 
                 bins = 10, color = "black", fill = "gray60")

# Let's use the shapiro test again. 
shapiro.test(anova_res$residuals) 
# Is the normality assumption violated?

# What if the normality assumption were violated? 
# Permutation test!
anova_res2 <- lmp(E~Continent_Name, data = anovaSub)
anova(anova_res2)

# What if normality and equal variance was violated?
# Again, we use either a permutation test or
# the Huber-White correction to address both at once!

# Factorial ANOVA ----
##################### #

# For our factorial ANOVA let's use the clinical trial dataset
# from the homework!
clin.trial <- read_csv("FactorialAnova_Mood.csv")
head(clin.trial)

# For the factorial ANOVA we are interested in both the main 
# effects and the interaction of drug and therapy on 
# mood gain

### 1. Independence ----
# Still a feature of design. If violated we have to use 
# a model that was not taught in this class.

### 2. Equal Variances ----

# Levene's test is the only one that works here
# because it accepts factorial designs. Bartlett's
# test will *not* work for factorial ANOVA.
leveneTest(mood.gain~drug * therapy , data = clin.trial)

# If the assumption of equal variances was violated 
# we couldn't just use a Welch's test. 
# There is no Welch's version for a factorial ANOVA
# So we have to use either a permutation test
# or the Huber-White Robust Correction 

### 3. Normality (of residuals) ----
anovafac_res <- lm(mood.gain ~ drug * therapy, data = clin.trial)
ggplot(anovafac_res) +
  geom_histogram(aes(x = .resid), 
                 bins = 5, color = "black", fill = "gray60") +
  theme_bw()

# We can use the shapiro.test() to test the assumption of normality 
shapiro.test(anovafac_res$residuals)

# If either the normality or equal variance assumptions (or both) 
# are violated we have to use a permutation test or a
# Huber-White Robust Correction 

# Huber-White Robust Correction 
Anova(anovafac_res, white.adjust = TRUE) 
# the Anova function takes the model we just 
# created a few lines above, before plotting it

# Permutation Test 
anovafac_res2 <- lmp(mood.gain~ drug * therapy, data = clin.trial)
anova(anovafac_res2)



