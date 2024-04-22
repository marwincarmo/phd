####################################################
##                                                ##
##                   103B Lab 4                   ##
##                Chi Square Tests                ##
##                                                ##
##          Created By Julia Bottesini            ##
##            Instructor: Marwin Carmo            ##
##                  Spring 2024                   ##
##                                                ##
####################################################

# Welcome to lab 4!

# First of all, lets load tidyverse
library(tidyverse)

# And set our working directory 
setwd("C:/Users/dpsiegel/Box/PSC 103B/Labs/Lab 4")

# Whenever you're writing an R script like this one (whether it's for
# the homework or something else) you should get into the habit of loading
# all the packages you'll need and setting your working directory
# at the top. This allows you to have all the functions you'll need
# as soon as you start writing your code, and is also useful if you
# need to share your code with anyone, because that person will
# know upfront if there are any packages they need to install.
# Overall, it's just a good coding practice!

# Today the data we'll use is in an .rds file. 
# This is a type of R file that allows you to save a little bit
# more information with your dataframe or other object
# (for example, if you have a column full of numbers that you
# want to be stored as type character instead, saving an .rds will
# allow you to do that). The downside to .rds files is that
# it's not possible to view it as easily outside of R,
# like a .csv file is (you could simply open it using any
# spreadsheet software). To create a .rds, we use this function:

# write_rds(object_iwant_to_save_here, path = 'FileNameHere.rds')

# To know more about this function, you can run
?write_rds

# We also need a different function to load this dataset:
cards <- read_rds('Lab4Data.rds')
# This is a dataset about cards, so let's call it that

# Let's look at our cards dataset first. What do you see?
# Can you guess what this dataset is about?
head(cards)

# This is a dataset about card selection. 
# The experiment to generate this data was as follows: 
# 200 people were asked to imagine a shuffled deck of cards
# and mentally pick one card at random. 
# Then they were asked to randomly pick another card. 
# This data set records the suit choice for the first and second
# choices for each participant. Importantly, participants didn't 
# physically pick a card, they were asked to 
# imagine randomly picking a card.

# Columns: 
# id: participant ID
# choice_1: card suit for first pick 
# choice_2: card suit for second pick 

# We can check that the last participant is indeed subj200
# using the "opposite" of the head() function:
tail(cards)



## Chi Square Goodness of Fit Test
########################################################

# Okay, we have our dataset, it's loaded and ready to go!
# Before we begin, let's define a few symbols I'll need
# throughout this lab.

# H0 is the null hypothesis
# H1 is the alternative hypothesis
# != means "not equal to" (we've seen this before when filtering data)
# P is the probability of something

# Let's look at the first choice people made

table(cards$choice_1)
# Any inital impressions? 
# What would it look like if people chose cards randomly?
# Do people really choose cards randomly?


# Let's say our theory is "people don't choose cards randomly" 
# We can translate that into statistical hypotheses (a H0 and H1) and 
# construct a statistical test of those hypotheses

# H0: All four suits are chosen with equal probability 
# Since we know we have 4 choices we can translate this into a mathematical hypothesis
# H0: P = (P_1, P_2, P_3, P_4)
# or
# H0: P = () #let's fill this in
# these numbers have to sum to 1. Why? 
# Why are there 4 probabilities?

# Let's save the null hypothesis probabilities in a vector.
# We're going to need it later

H0_prob <- c(clubs = .25, diamonds = .25, hearts = .25, spades = .25)


# What's our alternative hypothesis? 
#.
#.
#.
#.
#.
#.
#.
#.
#.
#.









# H1: *At least one* of the suit-choice probabilities isn't .25 
# or
# H1: P != (.25, .25, .25, .25)


# Ok so we have our observed frequencies for the first choice
observed_freq <- table(cards$choice_1) # This tells us how many people chose clubs as their first card,
                                        # how many people chose diamonds as their first card, and so on. 
observed_freq
# We also have our probabilities corresponding to H0 
H0_prob

# We can compare these two pieces of information to test our H0
# If the data doesn't look like what you would expect if H0 were true
# then H0 probably isn't true

# To make this easier, we can transform the H0 probabilities into
# the frequencies we would expect to see if H0 were true,
# given our sample size of N = 200
# To do that, we multiply the probabilities by that number

N <- 200 # our sample size
expected_freq <- H0_prob*N

# Look at the data again. Do you think H0 or H1 is more likely to be true?
# H0: All four suits are chosen with equal probability
# H1: *At least one* of the suit-choice probabilities isn't .25
observed_freq
expected_freq

# Preregister your guess here: 
# [  ] Reject H0
# [  ] Fail to Reject H0


# When you tried to compare the observed and expected frequencies,
# you might have wondered, how close is close enough?
# As with most processes that depend on chance, we wouldn't
# expect a perfect .25/.25/.25/.25 split even if those are the real
# probabilities. So how do we know how far is *too far* 
# from the expected values? 

# Statistics to the rescue!! (There's a test for that.)

# The test we're going to use is the chi-square goodness of fit test 
# This test measures how "close" the data is to H0
# and how likely it is that this frequency distribution:
table(cards$choice_1)
# was created by a process with the probabilities we assumed for H0
H0_prob

# What we want to do now is compare the expected results with the observed results
# If they are similar enough, we don't reject H0
# If they are very different, we reject H0 

# The Chi Square Goodness of Fit test can help us answer this question
# There are several steps in this test.

# Step 1: We compute "error" scores. 
# This tells us how big of a difference there is 
# between the expected and observed frequencies 

error_scores <- (observed_freq - expected_freq)^2 / expected_freq 
# Bigger numbers = more discrepancy between observed and expected frequency
error_scores

# We can use these error scores to calculate the goodness of fit statistic
# All we need to do is sum these error scores

goodness_of_fit <- sum(error_scores) # This is our goodness of fit statistic 
goodness_of_fit

# The closer the observed frequencies are to the prediction by H0 (expected frequencies),
# the __________ the goodness of fit statistic will get. [Larger or smaller?]


# Therefore, if H0 is true, do we expect the test statistic to be small or large? 
# What if H1 is true, do we expect the test statistic to be small or large? 

# So how large is large enough to reject H0?

# Step 2: Compare our statistic to the chi-square distribution.

# To answer this question we have to compare our test statistic 
# (the goodness of fit statistic) to the chi square sampling distribution

# What does the chi-square distribution look like?

# Don't worry about this plot, I just want to show you
# what the Chi Square distribution looks like
x1 <- seq(0, 30, by = .1)
y_df5 <- dchisq(x = x1, df = 5)
y_df10 <- dchisq(x = x1, df = 10)
y_df15 <- dchisq(x = x1, df = 15)

tibble(x1, y_df5, y_df10, y_df15) %>%
  pivot_longer(-x1, names_to = "dfs", names_prefix = "y_df", values_to = "value") %>%
  mutate(dfs = ordered(dfs, levels = c(5, 10, 15))) %>% 
  ggplot(aes(x = x1, y = value, color = dfs)) +
  geom_line(size = 1.5, alpha = .9) +
  labs(x = "", y = "density") +
  theme_classic()
  

# You'll notice that it changes as the degrees of freedom (df) increase.
# What other distribution did we learn about that also changed shape
# depending on the dfs?


# Back to our test, to compare our statistic to the chi-square distribution,
# we can use the pchisq() function. This function will take our goodness
# of fit statistic and the appropriate degrees of freedom, and tell us
# the probability of observing a statistic this big or bigger if the null 
# hypothesis (H0) were true.

# If we get a really large probability value, what can we conclude?
# What if we see a very small probability value, what can we conclude then?

# Let's do it!
pchisq(q = goodness_of_fit, # Our goodness of fit statistic
       df = 3, # degrees of freedom is the number of categories (4, in this case) minus 1
       lower.tail = FALSE) # We want the probability of getting this value or *more* extreme
                           # So we want information about the upper tail, so lower.tail = FALSE 

# Step 3: Decide if our p-value is big or small.

# As you know, in Psychology we use the cutoff of p < 0.05 for rejecting H0.

# Is the p value you got from the pchisq() function larger or smaller than .05? 
# Can we reject H0? 
# What does this mean?


# It's important to understand the steps that are necessary to run this test,
# but there's a function that does all of those steps automatically:
# The chisq.test() function in R

# The only information we need to give the function is the observed frequency 
# for each category

chisq.test(x = observed_freq) 

# The default for the chisq.test() function is to assume that our H0 
# is that each category has equal probability. But you can change this 
# by adding another argument, a vector of probabilities, like this:

chisq.test(x = observed_freq, p = c(.5, .3, .1, .1)) 
       # the p = argument allows you to manually input the probabilites 
       # for each category. They need to sum to 1.

# Can you write the line of code to get the same results as when 
# we did each step manually?


# How could we communicate the test we did and the results we got?

# Of the 200 participants in the experiment, 64 selected hearts as their first choice,
# 51 selected diamonds, 50 selected spades, and 35 selected clubs. A chi-square goodness
# of fit test was conducted to test whether the choice probabilities were identical for 
# all four suits. The results were statistically significant (x^2(3) = 8.44, p = .038)
# with alpha = .05. Therefore, we can reject the null hypothesis that all four suits
# are chosen with equal probability.

# Remember that rejecting the null hypothesis means your frequency distribution
# was probably not generated with the probabilities you tested. This doesn't tell
# us anything about the actual probabilities that generated the data!


# Now you do it! 

# Run the chi-square again, but this time with a different null hypothesis 
# Fill in the vector with your probabilities for each suit
# Remember, the probabilities need to add up to 1

nullProbs <- c(clubs = , diamonds = , hearts = , spades = )
nullProbs # This is your new null hypothesis. 

# Do a chi square goodness of fit to test the new H0 
# Can you reject the H0? 


## The Chi Square test of Independence
#########################################

# Independence is a key concept in probability.
# It describes a situation where knowing the value of one variable tells you 
# nothing about the value of another. 
# For example, what month you were born probably doesn't tell you anything 
# about what web browser you use. Those variables are independent. 

# The chi square test of independence tests whether two categorical variables
# are independent. In our case, we can test if the first choice of suit tells \
# us anything about the second choice.

# Our H0 is that the variables are independent 
# Our H1 is that they are not independent 

# Calculating the chi-square test for independence is a similar procedure 
# as the goodness-of-fit test, in that we're looking at the difference between 
# the expected values and the observed values

# You can use the table() function to get counts for a single variable,
# but you can also use it to get the number of observations in 
# for each combination of two variables, like this:

cont_tab<-table(Choice1 = cards$choice_1, Choice2 = cards$choice_2)
cont_tab

# the names Choice1 and 2 were added to make it easier to read the table
# but this works without them too

# If Choice1 and Choice2 are independent variables, knowing what someone
# picked as their first choice won't help us guess what they picked as their
# second choice. Another way to think about this is that the probability of 
# someone picking a suit for Choice1 is not related to the probability of 
# picking a suit for Choice2.

# This is a lot harder to spot than in the previous chi-square test we ran, 
# so we can use the chi-square test of independence to test this. 

# We can use the chisq.test() function. Since our goal is to see if two 
# variables are independent, we need to give the function both variables.
# When we do that, by default the function will do a chi-square test of 
# independence between the two categorical variables. Again, we compare the
# p-value here to our alpha of .05.


chisq.test(x=cards$choice_1, y = cards$choice_2)

# Can we reject the H0?

# What can we conclude about the two variables? 



# Here's how we might communicate that in a paper:

# The chi square test of independence revealed a significant association 
# between the first and second card choice (x^2(9) = 29.24, p < .001).
# This suggests that people's first choice of card suit is not independent
# from their second choice, and is therefore, not random.


# ### Extra Practice Exercise: M&Ms ans Skittles! ####

# When eating M&Ms or skittles, have you ever wondered about how the different
# colors are distributed? It turns out the proportion of each candy color is not
# just random! From this website:
# https://blogs.sas.com/content/iml/2017/02/20/proportion-of-colors-mandms.html

### Plain (and peanut) M&M's are now produced at two different factories in the US, 
### and the factories do not use the same mixture of colors! 
### You need to look on the packaging for the manufacturing code, 
### which is usually stamped inside a rectangle. In the middle of the code 
### will be the letters HKP or CLV. For example, the code might read 632GCLV20.
# 
### CLV: The Cleveland plant uses the following proportion of colors for plain M&M's:
###   Red=0.131, Orange=0.205, Yellow=0.135, Green=0.198, Blue=0.207, and Brown=0.124.
### HKP: The Hackettstown, NJ, plant uses the following proportion of colors for plain M&M's:
###   Red=0.125, Orange=0.25, Yellow=0.125, Green=0.125, Blue=0.25, and Brown=0.125.

# Mijke was very generous and personally opened a couple
# of M&Ms packs she had at home for us! Here are the contents:
# https://www.dropbox.com/s/ta0th5g4ft09il2/m%26ms.jpg?dl=0
# She couldn't find the manufacturer code though! Can you tell which
# one of the plants these came from?

# Create a vector of observed frequencies for the M&Ms in the pic
obs_freq_mms <- c(Red = , Orange = , Yellow = , Green = , Blue = , Brown = )

# Create probability vectors for each plant:
p_clv <- c(Red = , Orange = , Yellow = , Green = , Blue = , Brown = )
p_hpk <- c(Red = , Orange = , Yellow = , Green = , Blue = , Brown = )

# did they come from the Cleveland plant?
chisq.test(x =  , p =  )
# did they come from the Hackettstown plant?
chisq.test(x = , p =  )

# What can you conclude about where the M&Ms came from
# after running both tests?


# You can try this yourself with any pack of M&Ms or Skittles!
# If you have some M&Ms lying around, you can test the hypothesis that they conform to this
# pattern by counting the different colors, and doing a chi-square goodness of fit test
# using the correct proportions for your packet. If you can't find the plant information, 
# you can test both and see if this packet is more likely to have come from Cleveland
# or Hackettstown!

# Don't have any M&Ms lying around? Maybe you have some skittles!
# There is a debate on the internet about whether the five colors of skittles 
# are all equally likely to be in a bag. Can you test this hypothesis 
# using the skittles in your bag and a chi-Square goodness of fit test?

