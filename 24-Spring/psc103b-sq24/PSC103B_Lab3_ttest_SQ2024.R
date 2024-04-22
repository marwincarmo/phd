################################################# ##
##                                                ##
##                   103B Lab 3                   ##
##            T-Tests, Power Analysis             ##
##          Instructor: Simran Johal.             ##
##       Adapted from code by: Julia Bottesini    ##
##                 Spring 2024                    ##
##                                                ##
################################################# ##


# Welcome to Lab 3!

# First things first!
# Let's install the packages we're going to need later,
# load our packages, and set our working directory

# If you already installed a package, you don't need to re-install it!
install.packages("pwr", dependencies = TRUE)
install.packages("BayesFactor", dependencies = TRUE)

# now we load everything
library(tidyverse)
library(pwr)
library(BayesFactor)

# Uncomment and run to set your own working directory
# setwd("/Users/simranjohal/PSC103B_SQ2024")

# If you want, you can disable scientific notation so
# that you can see the p values as they are,
# instead of for example, 7.29E-08
# This is optional, but some find it easier
options(scipen=999)


#### The data we're using today:

# Sun_et_al_2017.csv uploaded to Canvas
# "Acting extraverted" experimental data
# Subset of data from Study 2 in Sun, Stevenson, Kabbani, Richardson, & Smillie
# (2017) http://dx.doi.org/10.1037/emo0000273
# You can find a pdf of the article here https://osf.io/preprints/psyarxiv/szk6t/
# Data and materials available at https://osf.io/kyahz/

#### Import Example Data & Set Up Packages ###

# Use Sun_et_al_2017.csv uploaded to Canvas

# Let's load that data
sun2017 <- read_csv("Sun_et_al_2017.csv")

head(sun2017) # Let's take a look at the data...

# id = id number,
# condition = acting condition,
# pa.t1 = positive affect (PA) after first task,
# pa.t2 = PA after second task,
# pa.av = PA averaged across both tasks

#### Descriptives ####
# Let's take a quick look at our dataset!

describe(sun2017)

head(sun2017)

# What function could I use to see how many participants are in this dataset?
# What if I wanted to check for missing data?

# Ideally, a dataset will come with a codebook, telling you
# about each variable, including about any missing data or
# how many participants are in there :)

#### Independent Samples T Tests ####

## Example 1: Do people instructed to act extraverted
# report different positive affect (PA) than people instructed to act introverted?

# One way to answer this question is to look at whether the mean
# positive affect from the "act extraverted" group is higher
# than the mean positive affect for the "act introverted" group.
# So I could just look at the means of the two groups right?
sun2017 %>%
  group_by(cond) %>%
  summarize(Mean_PA = mean(pa.av))

# (when we do this, we're not saving these numbers anywhere,
# but we didn't need to, we just wanted to see them)

# We actually have 3 conditions, so let's get rid of
# the observations in the control condition for now...
# How do we do that?

head(sun2017)

# sun2017 %>% ...


# Are these means different? Yes, but...
# How do we know they are meaningfully different?
# In other words, how do we know this difference isn't just due to chance?

# This is where we can use the statistical hypothesis tests...
# Like a t-test

# Step 1: What's the H0 and H1 hypothesis?
# Directional or non-directional?

# Step 2: Check Assumptions

# Step 3: Calculate t-statistic
# a. (sampleMeanGroup1 - sampleMeanGroup2)/ standard_error
# b. standard error = sqrt( (sampleSdGroup1^2 + sampleSdGroup2^2)/n )

# Step 4: Is the probability of obtaining this t-statistic or one more
#  extreme if the H0 was true?

## Step 1: What's the H0 and H1?
# H0:
# H1:

## Step 2: Skip for now

## Step 3: Conduct the t-test

# We could do this by hand in R, but instead, let's use the built in 
# t.test() function
# Remember from 103A that the t.test() function takes the arguments:
# t.test(DV ~ group, # outcome variable ~ grouping variable
# data = dataframe, # dataframe with the variables
# var.equal= FALSE,  # Are we assuming the groups have equal variance?
# alternative = ) # specify the alternative hypothesis: options below
# "two.sided" (the default)
# "greater"
# "less"

# the grouping variable needs to have only two levels so
# let's create this subset of the real data just to make this simpler:
sun2017_subset <- sun2017 %>% filter(cond != "control")

t.test(pa.av ~ cond, data = sun2017_subset, 
       var.equal = FALSE, alternative = "two.sided")

# You'll notice that in the code above I specified var.equal = FALSE
# This means that instead of conducting Student's t-test (which assumes
# group variances are equal, or homogenous), I conducted Welch's t-test
# which does not make this assumption
# The default in R is actually Welch's t-test, since it will give 
# the same answer as Student's t-test if the variances between groups are equal,
# but won't suffer from inflated type I error if they're not.
# In other words, the Welch Two Sample t-test is robust
# to violation of the homogeneity of variances assumption.
# And that's why some people (e.g., Lakens) recommend just using the Welch
# t-test, instead of "wasting your time" checking the
# homogeneity of variances assumption
# http://daniellakens.blogspot.com/2015/01/always-use-welchs-t-test-instead-of.html

# However, there are statistical tests you could perform to check
# whether you've met the assumption of homogeneity of variances, such as
# Levene's test

# Let's deconstruct the output of that command.
# First R tells us that we've run a two-sample t-test
# Next we have our t value, 6.2296
# Because it's positive, this indicates that
# the mean in our first group (acte) is higher than
# the mean in our second group (acti).
# Group order is set alphabetically when you use the pa.av~cond input format.

# Then we have the p.value, which is very very small (<.001)
# Can you explain what this means?
# It is the probability obtaining this t-statistic or one
# more extreme assuming the H0 is true (i.e., no group differences)
# Since this p-value is less than .05, we would reject H0, and conclude
# there is evidence that people instructed to act extraverted have,
#on average, a higher average positive affect than those instructed 
# to act introverted

# After that, we get the 95% confidence interval (CI)
# for the difference between the two groups.

# Then we have the actual means of each group,
# which always helps to sanity check the direction of the differences.

# However, in class last week, we also learned that there are some
# drawbacks to Null Hypothesis Significance Testing, and the use
# of p-values to make decisions in statistical inference
# These drawbacks have led some people to advocate for a Bayesian approach
# where we can compute the likelihood of our alternative hypothesis
# (or hypotheses) compared to the likelihood of the null hypothesis

# Instead of a p-value, we can compute a Bayes Factor, which is the ratio
# of the likelihood of our data under the alternative hypothesis
# to the likelihood of our data under the null

# To compute a Bayes Factor in R for a two-sample t-test, we can use
# the ttestBF() function from the BayesFactor package
# The arguments to this function are:
# ttestBF(DV ~ group, # a formula for outcome ~ group, like in t.test(0),
# data = dataframe) # the data you're using
# (Note: The R package website for BayesFactor gives good descriptions
# of how to use the functions: https://cran.r-project.org/web/packages/BayesFactor/vignettes/manual.html)

# Unlike with the options in t.test(), the only alternative
# we can specify with this function is  two-sided

ttestBF(formula = pa.av ~ cond, data = sun2017_subset)

# In this output, we have r (which is a value used to control
# the scale of the prior distribution we've placed on the difference)
# and, more importantly, a computed Bayes' Factor, which in our case is
# 144,481.7. This means we have 144,481.7 times more evidence (!!) for
# the alternative (that the means of the 2 groups are different) than
# the null hypothesis
# The denominator part tells us what our null hypothesized effect size was
# which is 0 

## Last step: Interpretation and reporting
## Here are a few things we'd want to include in reporting these results
## Type of T-test, df, means of each relevant group, sd's, t statistic, p value

# If you calculated a Bayes Factor, you would want to report information
# on the prior distribution, the value of the BF, and which hypothesis
# is more supported by your data

# To write up our results from the NHST:

# We ran an independent samples T-test as the groups
# were independent and their variances were equal.
# The mean positive affect for participants who were told to
# act extraverted was 4.54 (SD = .86), whereas the mean
# for participants who were told to act extraverted was 3.27
# (SD = .97). An independent samples t-test indicated
# that this difference was significant (t(54) = 6.23, p = 0.000000073)
#  suggesting that there is a genuine difference in positive affect
# between conditions. Specifically, those who were
# instructed to act extraverted had higher positive affect
# than those instructed to act introverted.

#### Dependent Samples T Tests ####

# Examples of dependent samples:
# 1. Individual change
# e.g., Are people happier after an intervention than before?
# 2. Matched groups
# e.g., Are husbands happier than wives?
# e.g., Is there a difference in adolescent neuroticism reported by self-ratings and by parents?

# Recall that we have two PA scores for each participant:
# after discussion 1 & after discussion 2
# What are the consequences of sustaining introverted behavior over time?
# For this example, let's focus on the acting introverted condition
# and the PA scores at t1 and t2.
# We'll need just the people in the acting introverted condition,
# so let's create a dataset for that

sun2017_acti <- sun2017 %>%
  filter(cond == "acti")

sun2017_acti %>%
  summarize(M_t1 = mean(pa.t1), M_t2 = mean(pa.t2),
            SD_t1 = sd(pa.t1), SD_t2 = sd(pa.t2))

# What's the H0 here?
# H0:

# What's the H1?
# H1:

# Let's use the t.test() function
t.test(sun2017_acti$pa.t1, sun2017_acti$pa.t2, # We give R two variables to compare
       paired = TRUE) # Important! This argument let's R know the groups are
# dependent / paired

# How could we write up these results?

# The average positive affect for participants at time 1
# who were instructed to act introverted was 3.01 (SD = 1.41).
# The average positive affect for the same group at time 2 was
# 2.6 (SD = 1.33). Based on the results from a paired
# t-test, we found that the average difference from time 1 to time 2
# (Mean Difference = .41) was not significantly different
# from 0  (t(27) = 2.36, p = .026).

# Like before, we could compute a Bayes Factor instead of a p-value
# using the ttestBF() function
# and the input is similar to t.test()

# ttestBF(x = datagroup1,
# y = datagroup2,
# paired = TRUE,
# data = dataname)

ttestBF(x = sun2017_acti$pa.t1,
        y = sun2017_acti$pa.t2,
        paired = TRUE,
        data = sun2017_acti)

# Now we have computed a Bayes Factor of 2.10. This means that our alternative
# hypothesis is 2 times more likely than the null. Some suggested cut-offs
# for Bayes Factors are that greater than 3 provides strong evidence
# for the alternative, so our value of 2 would show that the 
# alternative is more likely than the null, but not by much

#### Power Analysis ####

# Statistical hypothesis testing is designed to control
# the Type I error rate. When we fix alpha to .05, we're trying
# to make sure that we will only incorrectly reject the null
# hypothesis when it is actually true 5% of the time.

# But we also care about Type II errors...
# We don't want to fail to reject null hypotheses that are actually false!
# That is, we don't want to miss an effect that is really there.
# So we can also try to minimize Beta (the Type II error rate).

# But we don't really talk about minimizing Beta
# (or the Type II error rate). We most often talk
# about increasing or maximizing power (1-Beta).

# It's helpful to know how much power a study has when running an experiment
# We can do a power analysis to estimate this information
# In R we can use the pwr.t.test() to estimate power for a t test


# pwr.t.test(n = NULL,  # sample size per group
#            d = NULL,  # effect size
#            sig.level = 0.05,  # alpha level (default = 0.05)
#            power = NULL,  # power
#            type = c("two.sample", "one.sample", "paired"),  # what kind of t-test
#            alternative = c("two.sided", "less", "greater"))  # alternative hypothesis
# we'll only consider two-tailed tests in this class

# Leave the one parameter we want the function to calculate as NULL
# For this lab (and the next homework), we will only run power analyses
# for independent samples t-tests.

# How many participants do I need for each group,
# in order to detect a mean difference of d = 0.5
# at an alpha level of .05 and with 80% power?
pwr.t.test(n = NULL,
           d = 0.5,
           sig.level = 0.05,
           power = 0.80,
           type = "two.sample", alternative = "two.sided")

# How much power do I have to detect a mean difference
# of d = 0.5 with a sample size of 30 per group
# at an alpha level of .05?
pwr.t.test(n = 64,
           d = 0.5,
           sig.level = 0.05,
           power = NULL,
           type = "two.sample", alternative = "two.sided")


### Now You Try!! ####

## Independent Samples t-test

# Rather than comparing the two experimental
# conditions, let's compare the group that was told to act
# extraverted to the control group (are there any differences
# in their average positive affect scores?). Create a new subset, then (1)
# conduct the independent samples t-test and (2) calculate a BF
# then compute the BF for the independent samples t-test. Interpret the 
# p-value and BF

## Dependent Samples t-test

# Repeat the dependent samples t-test you did earlier, 
# but now with the group that was told to act extraverted 
# Report and interpret the p-value and BF


## Power

# What effect size would I need to be able to have 80% power to
# reject the null hypothesis, given a sample size of 50 per group,
# at an alpha level of .05?

pwr.t.test(n = ,
           d = ,
           sig.level = ,
           power = ,
           type = "two.sample", alternative = "two.sided")