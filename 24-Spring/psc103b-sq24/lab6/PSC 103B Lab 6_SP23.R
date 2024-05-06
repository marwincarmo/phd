##################################
##          Lab 6:              ##
##    Multiple Comparisons      ## 
#              &                ## 
##      Factorial ANOVA         ##
##                              ##
##        Spring 2022           ##
##################################

##### Multiple Comparisons #####

# Last week, we conducted an one-way ANOVA to determine
# whether the average self-reported nerdiness level differed
# across the 5 continents: Africa, Americas, Asia, Europe, and Oceania

# If you recall, our analysis resulted in a significant omnibus test
# and we then performed two follow-up comparisons: one comparing
# the average nerdiness level of Europe and the Americas, and 
# a second comparing the average nerdiness of Asia to the average
# of the other continents

# However, we did not bother to correct for the fact that
# we were doing *multiple* comparisons, and that when you do
# multiple comparisons, your chances of finding a false positive
# are now higher

# Therefore, lab today is going to focus on correcting the comparisons
# you do after a significant ANOVA, so that the Type 1 error rate
# for the FAMILY of those tests remains below 5%

# First, let's load the necessary packages and read in our data

library(tidyverse)
setwd("C:/Users/dpsiegel/Box/PSC 103B/Labs/Lab 6")
nerdy = read.csv("NPAS_Lab6.csv") ##base R version of read_csv from tidyverse

# And to remind ourselves of what the dataset looks like:

head(nerdy)

##rerun one-way anova from last week's lab on new dataset:
# When doing an ANOVA save the output of the lm function
my.anova <- lm(formula = nerdy_selfreport ~ continent, data = nerdy)

# To get the information we need to know if we can reject the H0
# We can use the function anova()
anova(my.anova) 

##we can look at the regression version of the output by swapping the anova()
# function with the summary() function

##Good review: try to see if you can calculate all the values in the ANOVA table
# using the other values!

summary(my.anova)
##try and see where the values match between the outputs


##rerun linear contrasts with new means
nerdy %>% group_by(continent) %>% summarize(M = mean(nerdy_selfreport))

##contrast 1- Americas and Europe
psi_1 <- 1 * 5.52 + (-1) * 5.20
ss_psi1 <- (200 * psi_1^2) / (1^2 + (-1)^2)
ms_psi1 <- ss_psi1 / 1
f_psi1 <- ms_psi1 / 2.0992
pf(f_psi1, df1 = 1, df2 = 995, lower.tail = FALSE)


##contrast 2- Asia vs all other groups
psi_2 <- -0.25 * (5.33 + 5.52 + 5.20 + 5.50) + 1 * 5.1
ss_psi2 <- (200 * psi_2^2) / ((-0.25)^2 + (-0.25)^2 + (-0.25)^2 + (-0.25)^2 + 1^2)
ms_psi2 <- ss_psi2 / 1
f_psi2 <- ms_psi2 / 2.0992
pf(f_psi2, df1 = 1, df2 = 995, lower.tail = FALSE)


#### Multiple Comparisons: A Priori ####

# To go over the different types of corrections, we are going
# to walk through two different scenarios
# The first is where the comparisons you conduct had been planned
# in advance, before you even looked at the data
# These are known as a priori tests, and because you did not make
# your decision of which comparisons to make based on the data
# you only have to correct for the tests that you actually conducted

# The second scenario is when you made your decisions of which 
# contrasts to conduct *after* looking at the data, which are
# known as post-hoc tests. I will discuss more about those in a bit

# In this first scenario, let's assume that I had plannedd to do
# my 2 comparisons in advance

# Then to ensure that the total error rate for my comparisons
# remains below 5%, I simply divide 5% (or whatever my alpha
# is) by the number of tests that I conduct

# In this case, I only have two tests, so that's nice!
# My new alpha level is:

.05/2

# And now I can compare my p-values from last week's comparisons
# to this new alpha

pf(f_psi1, df1 = 1, df2 = 995, lower.tail = FALSE)

pf(f_psi2, df1 = 1, df2 = 995, lower.tail = FALSE)

# Do our conclusions stay the same?

##### Multiple Comparisons: Post-Hoc ####

# Now, let's go to the scenario where I *didn't* plan those
# two comparisons in advance
# Instead, I chose to make those comparisons based on how the means
# looked - for example, I had seen that the mean of Asia appeared
# much lower than the means of the other continents, so I 
# had wanted to see if that difference was significant

# This is known as a post-hoc test, and is typically more
# exploratory
# Instead of just correcting for the two comparisons that I 
# conducted, I now have to correct for all the tests I could
# have chosen to do if my data had looked any different

# There are two common post-hoc tests: Scheffe's and Tukey's
# We use the Scheffe correction if our comparisons involve
# any combination of complex *and* pairwise contrasts
# And the Tukey correction if we're only interested in 
# the pairwise comparisons

# Which correction is most appropriate for our situation?

# Since we have one complex contrast and one pairwise contrast,
# we use the Scheffe correction, which involves calculating
# a new critical F value that you compare your calculated
# F statistic to

# The new critical F value is (J - 1) * F_alpha
# Where F_alpha is the value that cuts off 5% of the F distribution
# with df (J-1, N-J)
# Note that F_alpha is not the same critical F you would calculate
# for a contrast, since that is the value that cuts off 5% of 
# the F distribution with df (1, N-J) [different numerator df]

# We had 5 groups, and 1000 participants, so our new critical F is

(5-1)*qf(.05, 4, 995, lower.tail = FALSE)

# Let us compare our calculated F statistics to this value

# We had to do a Scheffe correction because at least one
# of our comparisons was a complex contrast
# What if we were only interested in pairwise comparisons
# such as Europe vs the Americas, and Americas vs. Oceania?
# Then we can do a Tukey correction
# Since the Tukey correction is considering a smaller family
# of tests than Scheffe, it typically has higher power

# To do this in R, we first need to conduct our ANOVA
# differently than we did last week; last week, we used
# lm() and anova() functions. But the function we're going to
# use for the Tukey test requires us to use a different function, 
# called aov()

# the input is the same as the lm() function from last week
aov(nerdy_selfreport ~ continent, data = nerdy)

##is.list(replications(nerdy_selfreport ~ continent, nerdy))??

# Let's save this as an object:

my.anova = aov(nerdy_selfreport ~ continent, data = nerdy)

# if we had wanted the ANOVA table, we could use
# the summary() command with my.anova as input

summary(my.anova)

# Now, for the Tukey test, we use the TukeyHSD() function
# The input is: TukeyHSD(aov object)

TukeyHSD(my.anova)

# Now we can see all the possible pairwise comparisons
# Along with an estimate of the mean difference
# Confidence interval limits for that difference
# and (importantly for us) a p-value
# This is the p-value based on the Studentized Range distribution
# and we can compare it to our usual alpha level of .05

# Let's look for the two comparisons I mentioned above
# Are those pairs of continents significantly differently from
# each other?

#### Factorial ANOVA ####

# Up until now, we've done a one-way ANOVA, where we only had
# a single grouping variable
# What would we do if we had more than one grouping variable?

# In the case of multiple grouping variables, we would conduct a
# factorial ANOVA. We do a factorial ANOVA instead of just multiple
# one-way ANOVAs because we're not only interested in the effect
# of each grouping variable on their own, but also their
# potential interaction

# The NPAS dataset also had demographic information available
# including participants' gender, which could either be Male or Female

# What if we were interested in whether continent *and*
# gender had significant effects on self-reported nerdiness?
# One option would be to just run two separate ANOVAs
# But that wouldn't let us examine whether continent and
# gender have a significant interaction, which is why we need
# to run a factorial ANOVA

# Since our first grouping variable (continent) has 5 levels
# and our second grouping variable (gender) has 2 levels,
# this is a 5 x 2 ANOVA

# Let's look at the means of each group

nerdy %>%
  group_by(continent) %>%
  summarize("Mean" = mean(nerdy_selfreport))


# Let's plot these means

ggplot(data = nerdy, aes(x = continent, y = nerdy_selfreport))+
  geom_boxplot()+
  theme_light()+
  labs(x = "Continent", y = "Self-Reported Nerdiness")

nerdy %>%
  group_by(gender) %>%
  summarize(mean(nerdy_selfreport))

ggplot(data = nerdy, aes(x = gender, y = nerdy_selfreport))+
  geom_boxplot()+
  theme(legend.position = "none")+
  theme_light()+
  labs(x = "Gender", y = "Self-Reported Nerdiness")

nerdy %>%
  group_by(continent, gender) %>%
  summarize(mean(nerdy_selfreport))

ggplot(data = nerdy, aes(x = continent, y = nerdy_selfreport,
                             fill = gender))+
  geom_boxplot()+
  theme(legend.position = "none")+
  theme_light()+
  labs(x = "Gender", y = "Self-Reported Nerdiness")

# What's our null and alternative hypotheses here? 
# We have to have a set of H0 and H1 for each main effect
# and for the interaction

# For the main effect of continent
# H0: mu_americas = mu_africa = mu_asia = mu_europe = mu_oceania
# H1: at least one mean is different

# For the main effect of gender
# H0: mu_male = mu_female 
# H1: mu_male != mu_female
# Note that since gender only has 2 categories, our H1 can be
# specific

# For the interaction of continent x gender (we can write this in words)
# H0: no interaction between continent and gender
# H1: there's an interaction between continent and gender

# The hypotheses are the same as the hypotheses we had before
# Now we just have three sets instead of one

# Now let's run the factorial ANOVA test in R
my.anova1 = lm(nerdy_selfreport ~ continent + gender, # outcome~ groupingvar1 + groupingvar2
           data = nerdy)

anova(my.anova1)

# To get the interaction in the ANOVA, we need to specify
# it in the model! To do that, we use an asterisk symbol *
# Note that variable1 * variable2 is shorthand for
# variable1 + variable2 + variable1*variable2 (the main effects and interaction)
# If you didn't want the interaction, you would just write
# variable1 + variable2

my.anova2 <- lm(nerdy_selfreport ~ continent * gender, data = nerdy)

anova(my.anova2)

# Let's write up these results!

# We conducted a 5 (Continent: Americas, Asia, Africa, Europe, and Ocenia) x 
# 2 (Gender: Male, Female) factorial ANOVA. There was a statistically
# significant effect of continent (F(4, 990) = 3.24, p = .01). There
# was also a statistically significant effect of gender 
# (F(1, 990) = 6.23, p = .01), such that females self-reported as being
# more nerdy (M = 5.45, SD = 1.36) than males (M = 5.22, SD = 1.54).
# The interaction between continent and gender was not significant,
# F(4, 990) = 1.30, p = .27.

# You Try! Run the factorial ANOVA using nerdy_scale as your outcome
# and gender and continent as your predictors.