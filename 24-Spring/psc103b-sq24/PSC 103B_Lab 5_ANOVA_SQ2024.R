####################################################
##                                                ##
##                 103B Lab 5                     ##
##                One-Way ANOVA                   ##
##                                                ##
##                 Simran Johal                   ##
##                 Spring 2024                    ##
##                                                ##
####################################################

# Before anything else...

library(tidyverse)

# Uncomment below and put your working directory inside if needed...
# setwd("")




# Today, we're learning about ANOVA, which stands for
# Analysis of Variance. This can throw some people off - why are
# we analyzing variance if we're testing differences in the means?
# And the reason is, we want to decompose our total variance
# into variance due to the group means being different, and
# what's left over - thus, we need to analyze the variance to see
# how much is explained by the groups versus is left over!

# Today we're going to talk about one-way ANOVA, when you only
# have one grouping variable; next week, we'll learn about factorial
# ANOVA, when we have multiple grouping variables

# The one-way ANOVA is used when we have more than 2 groups and
# we want to know if these groups have different means

# This sounds familiar, right? It's sort of like a t-test.
# So why don't we just use a t-test?
# Well a t-test can only compare 2 groups at once
# And running a t-test multiple times in order to compare all possible
# pairs of groups leads to a higher Type 1 error rate
# Luckily, ANOVA helps us avoid this issue!

# First, let's load in our data for today and look at it.
npas <- read_csv("NPAS_Lab5.csv")
head(npas)

# You'll see the following variables:

# country = the country the participant is from (based on computer address)
# nerdy_scale = participant's average "nerdiness" score, based on the scale (a 1-5 scale)
# continent = the continent a person is from, based on their country
# answers to the Ten Item Personality Inventory (TIPI): Do you see yourself as ___ (1-7 scale)
# TIPI1 = extraverted/enthusiastic
# TIPI2 = critical/quarrelsome
# TIPI3 = dependable/self-disciplined
# TIPI4 = anxious/easily upset
# TIPI5 = open to new experiences/complex
# TIPI6 = reserved/quiet
# TIPI7 = sympathetic/warm
# TIPI8 = disorganized/careless
# TIPI9 = calm/emotionally stable
# TIPI10 = conventional/uncreative
# nerdy_selfreport = do you see yourself as someone who is nerdy (1-7 scale)

# This is a real dataset that is available from the Open Psychometrics
# website: https://openpsychometrics.org/_rawdata/
# and you can even take the Nerdy Personality Attributes Scale yourself!
# https://openpsychometrics.org/tests/NPAS/

# The original dataset had over 25,000 responses from participants
# all over the world - in order to make it even, I chose 200
# responses randomly from each continent
# This is why if you use:

table(npas$continent)

# You'll see that every continent has exactly 200 observations
# This prevents the data from being skewed too heavily toward
# one continent, such as the Americas (which is N. America
# and S. America combined)

# We can also look at the average self-reported nerdiness for each group.
# We learned how to do that in the first lab, using the group_by() and
# summarise() functions

npas %>%
  group_by(continent) %>%
  summarise(
    mean_nerdy = mean(nerdy_selfreport, na.rm = TRUE),
    sd_nerdy = sd(nerdy_selfreport, na.rm = TRUE)
  )

# Look at the means. Are they close or far apart?
# Look at the SDs. What do they tell you?

# We can also plot these statistics, along with the data
# (this uses ggplot2, another package in tidyverse.
# If you're loading packages individually,
# don't forget to add this one!)

# Don't worry if this plot code looks confusing,
# you won't need to know how to do this by yourself
# (if you're curious and do want to learn about this,
# there are several good resources, just ask me about it later!)

npas %>%
  ggplot(aes(x = continent, y = nerdy_selfreport, color = continent)) +
  geom_jitter(alpha = .2, width = .2) +
  stat_summary(
    shape = 20,
    fun = mean, geom = "pointrange",
    fun.min = function(x) mean(x) - 1.96 * (sd(x) / sqrt(200)),
    fun.max = function(x) mean(x) + 1.96 * (sd(x) / sqrt(200))
  ) +
  theme_classic() +
  theme(legend.position = "None") +
  xlab("Continent") +
  ylab("Self-Reported Nerdiness")

# This plot shows us the mean nerdiness of each continent, based
# on the people who took the test; the lines are then the 95%
# confidence intervals around those means
# Each dot represents an individual's data point

# This can be a little bit hard to read, since we have the data for 200
# people in each group all plotted around the mean
# It seems that maybe there are maybe more lower values in Asia
# and Europe compared to other continents, but there are no
# stark differences just yet
# But now we want to answer the question:
# are these differences that we see "real" or due to chance?

#######################
#### One-Way ANOVA ####
#######################

# What are our null and alternative hypotheses?

# H0 : mu_africa = mu_asia = mu_europe = mu_americas = mu_oceania
# or in other words, the means for all groups are equal
# (people from all continents self-report as being equally nerdy)

# We need the alternative hypothesis to "catch" every other case
# but there's a lot of different ways that this null hypothesis
# can be false! We can have a single continent being of a different
# nerdiness than the rest, but the rest are all equally nerdy
# or two continents are equally nerdy to each other, but different
# from the other 3, and those 3 are all equally nerdy,
# or all 5 are different, or so on!

# Therefore, you can often see the alternative being written
# as "H0 is not true" or (more specifically) "At least one
# of the means is different from at least one other mean"

# So how can we go about testing this hypothesis?
# If we go back to our graph, we can look at the different
# types of variability represented 

# The variability of all these points, ignoring which continent they
# belong to, is the total variability of the dataset
# And then the variability of the points *within* each continent
# is the variability of each group
# What we want to know is whether there is larger variance *between*
# the different groups as compared to what we see *within* the different
# groups, which points towards group differences

# Another way to put it is, the ANOVA calculates the amount of variance
# that is due to groups (how much of the difference in nerdiness between
# two people in different continents is due to living in different continents)
# and compares it to the variance that is left (whatever is not due to them
# being from different continents and is just due to them being two people)

# If our null hypothesis is true, and there is no difference between the
# groups, then the variance between the groups wouldn't actually explain
# much of the total variability, and we'd have a lot leftover

# But if the null is false and there is some difference
# between the groups, then you would expect those groups to
# explain a good portion of the total variance (more than
# the residual variance)

# We could do this all by hand, but that's a bit time-consuming
# Instead, we can use two R functions

# The first function you want to use is the lm() function
# to write a formula: outcome ~ group (we'll come back to lm()
# when we learn about regression)
# This formula is the same way we wrote formulas for a t-test,
# only now our group variable has more than two groups!
# The only other argument the lm() function needs is your dataset

lm(
  formula = nerdy_selfreport ~ continent, # outcome ~ group
  data = npas # dataframe
) 

# Where's the p-value?
# Where's the statistic?
# Where's all the stuff that tells you whether you can
# reject or retain the H0?
# Right now, all we're getting is a bunch of coefficients that
# don't make sense without the proper context

# When doing an ANOVA save the output of the lm function
my.anova <- lm(formula = nerdy_selfreport ~ continent, data = npas)

# lm() also does a lot of extra stuff in the background
# by saving it as an object, we'll be able to access those other things
# whenever we want

# To get the information we need to know if we can reject the H0
# We can use the function anova()

anova(my.anova) 

# Let's look at each of these values

# dfg = # groups - 1
# dfr = Total sample size - # groups
# dfTotal = Total sample size - 1 (notice that dfg + dfr = dfTotal)

# MSg describes the amount of variance in nerdiness
# that can be attributed to group differences

# And MSr describes the amount of variance that is left
# or the amount of variance in nerdiness that cannot be
# described by group differences

# Notice that although SSG + SSR = SSTotal, it is NOT the case
# that MSG + MSR = MSTotal (the denominators are different)
# So if you were ever given an incomplete ANOVA table to fill in,
# focus on adding SS and df first to calculate MS

# If the H0 was correct and there was no difference between
# these groups, would we expect the MSg to be large or small
# compared to the MSr? In other words, do we expect our F ratio
# to be larger than 1 or closer to 1

# What about if the H0 was incorrect?

# Pr(>F) is your p-value - compare to .05 to decide to reject or
# fail to reject H0

# What decision do we make here? And what can we conclude from that decision?

# Note, that this is what makes ANOVA different from a t-test
# in a t-test, if you reject H0, you can conclude that your
# two groups are significantly different
# We can't do the same in an ANOVA because it's what is
# called an omnibus test - all a significant p-value tells us
# is that there is some difference, somewhere, between the groups

# But which groups are different?

#### Linear Contrasts ####
#######################################################

# Based on our group means, we may have had some a priori
# guesses about which group means were significantly different
# from the rest (e.g., it seems people who live in Asia have a much
# lower average nerdiness score compared to other continents)
# or we might be interested in whether people in the Americas
# have a different level of nerdiness than people in Europe

# Let's tackle these two comparisons

# Comparison 1: Americas vs. Europe
# This is a pairwise linear contrast, because we're only looking
# at two of our groups

# Our null hypothesis here is H0: psi = 0
# This implies H0: mu_americas = mu_europe
# and HA: mu_americas != mu_europe
# What contrast coefficients/weights could we use for this contrast?

# Recall: the means of each group can be calculated using:

npas %>%
  group_by(continent) %>%
  summarise(
    mean_nerdy = mean(nerdy_selfreport, na.rm = TRUE)
  )

# and we can use these in the weighted sum

psi_1 <- 1 * 5.36 + (-1) * 5.46 # no other group means are involved
# because their contrast coefficient is 0

# Now let's turn it into a sum of squares by squaring it, and then
# multiplying by the number of observations per group; the whole thing
# then gets divided by the sum of the squared weights (so that
# the choice of weights being -1/1 versus -200/200 doesn't affect
# our answer)

ss_psi1 <- (200 * psi_1^2) / (1^2 + (-1)^2)

# Let's turn this into a mean square by dividing by the df
# so that we can use it in a F test
# Recall: the df for any contrast is 1

ms_psi1 <- ss_psi1 / 1

# to calculate a F statistic, we just need MS_res from our earlier
# ANOVA output

f_psi1 <- ms_psi1 / 1.9887

# and let's use the pf() function to get a p-value!

pf(f_psi1, df1 = 1, df2 = 995, lower.tail = FALSE)
# recall, df_res = n - # groups, or 1000 - 5

# What do we conclude? Was the nerdiness of people in the
# Americas significantly different than that of people in Europe?
# Conclusion: No, because p > .05

# Let's take a look at our second contrast
# Comparison 2: Asia versus Everyone Else
# This is a complex linear contrast, because it involves
# multiple groups

# What are the weights now?

psi_2 <- -0.25 * (5.32 + 5.36 + 5.46 + 5.51) + 1 * 5.08

# The rest of the steps are the same as before

ss_psi2 <- (200 * psi_2^2) / ((-0.25)^2 + (-0.25)^2 + (-0.25)^2 + (-0.25)^2 + 1^2)

ms_psi2 <- ss_psi2 / 1

f_psi2 <- ms_psi2 / 1.9887

pf(f_psi2, df1 = 1, df2 = 995, lower.tail = FALSE)

# Now our p-value is tiny! So we can conclude that people
# who live in Asia have a different average level of nerdiness
# than the average nerdiness level of the remaining continents
# based on the group means, it seems the average nerdiness level
# of other continents is higher

# You may have noticed that I've been comparing my p-value
# to the usual alpha level of .05. Could I have done as many
# "planned" contrasts as I wanted, and kept comparing my p-value
# less than .05?

#### Write-Up
######################################

# We ran a one-way ANOVA and found a significant differences in self-reported
# nerdiness between continents, F(4,995) = 2.78, p = .03.
# We then ran two planned linear contrasts, which revealed there was
# no difference in self-reported nerdiness between the Americas (M = 5.36,
# SD = 1.41) and Europe (M = 5.46, SD = 1.32; F(1, 995) = 0.50, p = .48), but that people who lived
# in Asia (M = 5.08, SD = 1.55) reported lower levels of nerdiness than the average across
# the other 4 continents (M = 5.41, SD = 1.37; F(1, 995) = 8.89, p = .003).

# A Note On Inferences -------------

# Since this data is real data, and not data that I just made up
# we can have a little more faith in our results
# But of course, there are a number of confounding factors
# for why these results may not be real - for example, the people who
# are willing to volunteer to take a survey about their nerdiness,
# and have the access to complete a survey on this website
# could potentially be not representative of the people in that
# continent as a whole

#### You Try! ####

# In our example, we used self-reported nerdiness as our outcome
# It is possible that the nerdiness level found on the scale
# might be different (perhaps people like to think they're more nerdy
# than one would suspect based on their behavior)

# Let's test it out - run an ANOVA using nerdy_scale as your outcome
# variable. If necessary, run the same follow-up contrasts that we
# used before
