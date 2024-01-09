##########################################################################
###                           PSC 103B                                 ###
###                         Lab Session 1                              ###
###                                                                    ###
###                 Introduction to R and Review                       ###
###                                                                    ###
###                  Instructor: Paprika Jiang                     ### 
##########################################################################

###Objects: 1. Basic R
########### 2. Review of Statistical Concepts
########### 3. In_Lab Assignment Time


# (1)

## R as a calculator ----
#########################


## Common mathematics operations in R:
#
# + :      addition
# - :      subtraction
# / :      division
# * :      multiplication
# ^ :      exponent
# sqrt() : square root 
# exp() :  exponential function

2 + 2

3 - 1

2 * 6

4 / 2

6^3

sqrt(16)

exp(0)


# To run a line of code just use CTRL + ENTER (that's COMMAND + ENTER if you're on a Mac)

# How would I ask R to divide 10 by 2? 
10/2

# How would I ask R to calculate 5 to the 4th power? 
5^4

# The R Calculator follows the PEMDAS rule
# Parenthesis, exponents, multiplication, division, addition, subtraction
# from left to right 

3 + 4 * 12


# Following PEMDAS, R will first multiply 4 by 12 and then add 3 to the product 
# If I want R to add 3 and 4 first and then multiple their sum by 12
# I have to add parenthesis 

(3 + 4) * 12

## Commenting----
##################

# You may have already noticed one convention in R -- how to write comments
# Anything that you type after the "#" will be disregarded by R
# so you can make notes to yourself and it will not interfere with your code

# This is considered very good practice when coding -- you should always
# make comments on your code to remember what each bit does. Your future self
# will thank you.


## Creating Objects----
#######################

# One of the most important things we can do in R 
# is store information as an object 
# You can think of an object as a label for a piece of information 
# or multiple pieces of information

# We can save information as variables by using the assignment operator: <- 
# All assignments follow a general pattern 
# ObjectLabel <- value 

a <- 2 + 2

# This line of code saves the result of 2 + 2 (which is 4)
# to the object named 'a'

# Now we can access this piece of information (the number 4) through the object label, a 
# This is described as 'calling' an object

a

# R is case sensitive 
# Trying to call this object by typing a capital A will not work 

A # Tip when you get an error message, read what the error message says
  # This will help you figure out what went wrong

# You can also overwrite objects simply by saving other information under 
# the same label 

a <- 3

a # Now the number 3 is saved under the label a

## Labeling conventions

1a <- 3

!a <- 3

a! <- 3

a1 <- 3

# Including spaces in your label names
# The actual "space" is not usually allowed in label names, instead, you want to use some other symbols such as "_","." or numbers as substitutions

a object<-3 # getting error message

a_object <- 3
a.object <- 3
aObject <- 3

## Data Types----
###################

# We just learned how to store information in R, but there 
# are different kinds of information. R will treat 
# different kinds of information differently

# Today we're going to talk about three different types
# of information or, as they're more commonly called, 
# data types

# Numeric: 
# The first data type is numeric. This is the data type 
# that we have worked with so far

3
-3
.00021

# These are all numeric 

# Character: 
# The second data type is character. Character data
# types must be surrounded by quotation marks 

"student"

"myFirstCharacter"

# These are all character datatypes
# What happens if you don't surround a character data type
# with quotation marks? 

student
# Without quotation marks, R thinks this is a object
# label. It looks in your global environment to see 
# if any objects are labeled student. And when R doesn't
# find any, it returns an error saying that the object
# was not found
# Without quotation marks R doesn't know you are trying
# to create a character data type

# Logical: 
# The third data type is a logical. There are only two 
# options TRUE (T) and FALSE(F)

# They must be in all caps

TRUE
FALSE

true

# You could also abbreviate TRUE to T and FALSE to F

T

F


## Vectors----
###############

# All of the objects we've created until this point have 
# only held one piece of information

# But we can also save multiple pieces of information 
# to an object

# To do this we can use the c() function
# We'll talk about functions in more depth in a bit, but for
# now know that the c stands for combine and this function
# combines different pieces of information together 

# The pieces of information you would like to combine go
# between the parenthesis, and each piece of information 
# must be separated by a comma 

c(1, 2, 3, 4)

# We can save these pieces of information to an object 
# the same way we would save a single piece of information
# to an object 

first_vector <- c(1, 2, 3, 4)

# We can look at this object by calling it through the 
# object label 

first_vector 

# We just created a vector
# Each piece of information we saved to the vector are 
# now called elements 
# We saved four pieces of information (for numbers) to the vector 
# this means my_vector has 4 elements
# The number 1 is the first element
# The number 2 is the second element etc...

# A vector is a one-dimensional collection of information. And all the 
# elements must be the same type 

# What is the datatype of the object we've just created? 
# The object first_vector ? 
class(first_vector)

str(first_vector)

# We could also create a character vector

character_vector <- c("one", "two", "three", "four")
str(character_vector)

# Subsetting vectors----
#########################

# Sometimes we want to pull out one element from a vector
# that has multiple elements
# To do this we need to subset the vector 
# We can use square brackets [ ] to subset

# We can use square brackets after the label name,
# with the element number we would like to remove inside the brackets

# What if I want to pull out the third element from a vector? 
character_vector[3]

# How would you take out the first element? 


# We can save this as a new object
third.element <- character_vector[3]

# We can also take out multiple elements at once

character_vector[c(2, 3, 4)]

# we can also remove some elements by adding "-" before c()
character_vector[-c(2)]

# How would you remove the first and third elements? 
first_vector[-c(1,3)]


# (2)
## Functions in R----
#####################

# We can also save tasks or lines of code under a label
# These objects are called functions 
# This is like a short cut. When we want to accomplish some task, 
# rather than writing out the code to do this task, we can call 
# a function by its label and it will complete that task 

# For example, let's say I wanted to calculate the mean (or average)
# of our numeric vector 

first_vector

# We calculate the mean of a set of numbers by adding 
# the numbers together and dividing the sum by the number
# of numbers you summed

# To calculate the mean of the numbers in first_vector 

(1 + 2 + 3 + 4) / 4 

# What if first_vector had 100 numbers not 4? 
# Or what if you had 100 vectors and wanted to 
# calculate the means of each one
# Suddenly, writing out the code to calculate the mean
# seems a lot more annoying and time-consuming than 
# before. 

# We can use functions to make this quicker and easier 
# We could use the sum() function and the length() function 

sum(first_vector)/length(first_vector)
  # sum() will add up all the numbers in a vector 
  # length() tells you how many elements are in a vector 

# Or we could use the mean() function 

mean(first_vector)

# When I talk about functions, you'll hear me use the word 'argument' 
# Arguments are the information we give the function so it can carry
# out its task 

# A function can have multiple arguments, but the arguments 
# must be separated by a comma

# functionLabel(argument1, argument2, argument3)

# For the mean function, the first argument was the data or the 
# number we wanted the mean of 

# Let's look at another function: round() which will round whatever number
# you give in the first argument

round(3.666)

# What if I want it to round the number to the second decimal point? 
# We can add another argument

round(3.666, 2) # The second argument tells it how many decimal points to round to

# Each argument has a label. For example, the argument labels for the previous function are...

round(x = 3.666, digits = 2) # You don't have to use labels... but it is helpful 

# If you use labels, the order of the arguments don't matter. 

round(digits = 2, x = 3.666) # same result, right? 

# Now try writing the "digits" argument first without the labels. 
# What happens? 

round(2, 3.666)

# Again, when you use labels, the order of the arguments don't matter
# When you don't use labels, the order really really really matters. 

# What if you're using a new function? Or if you don't remember the labels 
# or the order of the arguments? 

# One option is to go to the help page of that function

?round
?typeof

# Or you can press tab one your cursor is within the function parenthesis , and the arguments are labeled in purple 

round()

# Or you can google it. Example google search: 'round function R'


# Another useful function is class()
# The class function will tell you what kind of data type 
# is saved within an object

class(character_vector)
class(first_vector)

#str() gives the similar information but more in detail
str(first_vector)

## (3)
## Data Structures 
#####################################################################

# So far we've only worked with vectors:

# But there are other data structures that you need to be 
# familiar with 

# Matrix: 2-dimensional dataset (has columns and rows) of one data type 

# We can use a new function matrix() to create this data structure
# There are a couple of important arguments
# matrix(data, nrow, ncol, byrow)

m <- matrix(data = c(1:6), # The matrix will contain elements 1, 2, 3, 4, 5, 6
            nrow = 2, # this matrix will have two rows
            ncol = 3, # this matrix will have three columns
            byrow = T) # we fill the elements by the row
                          # What happens if you change byrow = FALSE ?

m
# New function!
# dim() tells you the dimension of a matrix. How many columns and rows

dim(m) # First element is number of rows, second element is number of columns

## rbind() and cbind()
# You can also create a matrix by combining vectors

vec1 <- 1:5
vec1
vec2 <- 6:10
vec2
# If I want to make each vector their own column in a matrix 
# I can use the cbind() function 

col_matrix <- cbind(vec1, vec2)
col_matrix
# Or I could make each vector their own row in a matrix 
# I can use the rbind() function

row_matrix <- rbind(vec1, vec2)

row_matrix

# Data frames: Allow you to have multiple data types. 

# We can use the data.frame() function to create a data frame
# Each argument is a different column
# We can also add labels to each column 

dat <- 	data.frame(
  schoolyear = c("Freshman", "Sophomore","Freshman"), 
  height = c(152, 171.5, 165), 
  weight = c(81,93, 78),
  age = c(18,20,19),
  stringsAsFactors = FALSE) 
# we need this last argument (stringsAsFactors = FALSE) 
# so our variable schoolyear will be read in as the type of  
# character (and not factor). Don't worry about what this means yet;
# next week we'll learn about a similar function to data.frame()
# that doesn't require this argument.

# PS: **NEVER** call your data object data. data() is already an R
# function, and if you have an object and a function with the same name,
# R might get confused. There are many alternatives, like df, dat, dt, my_data,
# or literally anything else you can think of.

            
## Subsetting Two-Dimensional Objects----
##########################################

# We've used square brackets [] to subset vectors 
# We can use square brackets [] to subset two-dimensional objects
# like a matrix or data frame, but we have to change the
# information we provide

#twoDimObject[row#, column#]

# If I want to subset the element in row 3 and column 2
dat[3, 2]

# If I want to subset the entire 3rd column

dat[, 3]

# I could also subset by column name 

dat[, "weight"] # this subsets all the rows from the weight column

dat[1, "weight"] # this subsets the first row from the weight column

# If we just want to subset a column we can use the $ operator 

# twoDimObject$ColumnName

dat$schoolyear # Subsets the column labeled schoolyear
dat$height
# This is not the only way to subset variables, and we will learn
# other ways next week, but now you have all the information you need
# to do some basic stats! So let's do that!


## Review of Statistical Concepts ----
##############

# Central tendency: Mean, Median, and Mode ---

# There are many measures of central tendency, but these 3 are the most
# common. They are used to describe a distribution of observations
# (e.g., all the grades on an exam) in one number that best represents
# that distribution. Let's see how this works.

# First, let's create some variables!
# Suppose we asked a bunch of UC Davis students how many hours per week
# they spent watching Netflix, and how many hours they spent exercising
# during Winter break. We record their answers in two separate vectors:

netflix <- c(2,6,1,7,2,4,11,40,7,0,NA,4,5,2,15)
exercise <- c(2,2,6,2,12,45,8,3,2,6,4,0,1,3,0)

# Notice that someone refused to tell us how much time they spent on Netflix...
# How many observations are in each variable?
length(netflix) # notice that this is not affected by that missing value
length(exercise)

# We have 15 people in our dataset now. Let's take a look at the average
# time each student spent on these activities:

mean(netflix) # you'll get an error here, because there's a missing value
mean(netflix, na.rm = T) # use the argument na.rm = TRUE to ignore missing values 
mean(exercise, na.rm = T) # this works when you don't have any NAs too,
                          # and in other functions!

# Unsurprisingly, students exercised less than they watched netflix, on average. 
# But is the mean a good representation of these data? 
# Check out that person who looks like their job
# is watching Netflix. 40 hours a week? What about that
# athlete who exercised 45 hours per week over the break??

# When we have outliers, sometimes the median is a better representation of 
# the data we have. To find the median, you first order the elements:
sort(exercise)
# And then you find the middle element, which is 3
# For the netflix question, since we have missing data,
# we have only 14 observations, so the median is the average of the two
# middle observations, or (4+5)/2 = 4.5
sort(netflix)
median(netflix,na.rm = T)

# That's pretty different from what we had before!

# Sometimes, we can't do arithmetic on the data we have. For example,
# if we had asked our 15 participants what their favorite flavor
# of ice cream was (character data), we would not be able to describe that distribution using a mean or a median. That's when the Mode is useful:
# The mode is just the most frequent value. It's not used very often
# so R doesn't have a function for that. But you can use another
# very useful function to find the mode: table()

table(netflix)

# table() gives you the number of times each element shows up in an object.
# by looking at the results here, we can see that 2 shows up 3 times, so it's
# the mode of "netflix".
# To make it easier to see, you can use the sort function on the result
# of the table function to order it. Here's that for "exercise". What's
# the mode?

sort(table(exercise))


# Spread: Variance and Standard Deviation ---

# Imagine that I told you the mean number of hours students spent
# exercising each week over the break was 6.4 hours. You would
# know something about these students' exercise habits, but not a lot.
# Do all the students exercise about the same? Or do some students
# exercise a lot while others don't? These are the types of questions
# that measures of spread try to tackle: how are the observations
# spread out around the mean or median?

# We're gonna look at two different kinds that are related:
# variance and standard deviation.
# To get the variance:
# 1) calculate the mean
mean(exercise) # 6.4
# 2) find the distance from each observation to the mean
# R can do this for us pretty easily, because it is used to
# working with vectors. So when we do this:
exercise - mean(exercise)
# it will subtract 6.4 from each observation in "exercise". 
# Let's save that.
diffs <- exercise - mean(exercise)

# 3) square the differences
# just as we did before, this is also very easy in R:
diffs_sq <- diffs^2

# 4) sum everything and divide by N-1 
sum(diffs_sq)/14

# The variance of "exercise" is understandably a large number.
# Remember how that one athlete was very far from the mean?
# We can check our answers using var()
var(exercise)

# To get the standard deviation, we just get the square root of
# the variance:
ex_var <- var(exercise)
sqrt(ex_var)

# Or, R has a sd() function:
sd(exercise)


?var()

# Relationships between variables: correlation and covariance ----

# Remember that in our imaginary example, we asked each person about
# both their exercise and netflix activity over the break. Therefore,
# each person gave us two bits of information (except that one person...)
# Let's organize our data into a dataframe to better keep track of it.

df <- data.frame(Netflix = netflix, Exercise = exercise, stringsAsFactors = F) 
# here, the capitalized names are the variable names,
# and I'm assigning the objects "netflix" and "exercise"
# to those variables.
# You can look at df by clicking on it, using View(), typing it in the console,
# or using functions like head() and tail(). Can you tell what those do?
df
View(df)
head(df)
tail(df)

# Let's look at covariance first. The covariance between two
# variables is a measure of how the two variables change together.
# It only makes sense if there's some connection between the two
# variables. In this case, each row came from the same person 
# (not really, but let's pretend!)

# The covariance is a bit like the variance, but instead of squared
# differences from the mean, we multiply these differences from the mean
# by each other. Let's use the variables in our new dataframe, df.
# Here are the steps:

# 1) get the differences from the mean for each variable:
diff_nfx <- df$Netflix - mean(df$Netflix, na.rm = TRUE)
diff_ex <- df$Exercise - mean(df$Exercise, na.rm = T)

# you can check this is right -- the differences will sum to 0
sum(diff_ex)

# 2) multiply them by each other
# (by row -- R does this but you can check by hand,
# or compare the objects to see it)
mult_diffs <- diff_nfx * diff_ex

# 3) sum all these multiplied differences
# (don't forget that NA!)
sum_diffs <- sum(mult_diffs, na.rm = TRUE)

# 4) divide by N - 1
# (but remember -- because we can't use that observation 
# with the missing value, N in this case is the 14 complete observations,
# or 14. So N-1 is 13.)
cov_NetEx <- sum_diffs/13
cov_NetEx
# You can verify this yourself by using the cov() function
cov_NetEx <- cov(df$Netflix, df$Exercise, use = "complete.obs")

cov_NetEx
# For cov(), the use = "complete.obs" argument acts similarly to na.rm = T

# We see that the covariance is negative, indicating that the
# Netflix and Exercise variables are inversely related to each other:
# higher values for Netflix tend to go with lower values for Exercise,
# and vice versa!

# But how strong is this association? We don't know, because covariances
# have arbitrary scales based on the scales of the original variables.
# We don't know how big they could get so we don't know if this value is
# large or small. That's not very helpful for us, so we need something
# we know the scale of: correlations.

# Correlations can only range between -1 and 1, so they're easier to 
# interpret. We'll standardize the covariance to get a correlation.
# Standardizing in this case means dividing by the variables' 
# standard deviations. We already know how to get that:

sd_N <- sd(df$Netflix, na.rm = T)
sd_E <- sd(df$Exercise, na.rm = T)

# We divide the covariance by the product of the variances
# to get a correlation:
cov_NetEx/(sd_N*sd_E)

# And we can check this using the cor() function:
cor(netflix, exercise, use = "complete.obs")

# UH OH! What happened? We got -0.151 from our calculations,
# but -0.146 from the function!
# That's because of the missing observation. The cor() function
# doesn't use that whole row/observation (that is, it removes both 
# the NA from Netflix *and* the the corresponding "4" from Exercise) 
# when calculating the correlation,
# but we used the "4" from Exercise when calculating the SD.
# let's remove that whole row to see what happens:

complete.cases(df) # this vector tells us which rows are complete in df
df[complete.cases(df),] # so we use it to tell R which rows we want (and all columns)

# That looks good, let's try again:
df_noNA <- df[complete.cases(df),]

sd_N <- sd(df_noNA$Netflix, na.rm = T)
sd_E <- sd(df_noNA$Exercise, na.rm = T)

cov_NetEx <- cov(df_noNA$Netflix, df_noNA$Exercise, use = "complete.obs")

cov_NetEx/(sd_N*sd_E)

cor(df_noNA$Netflix, df_noNA$Exercise, use = "complete.obs")
# They match!!

# So, is -0.146 a meaningful correlation between time spent watching Netflix
# and the time spent exercising? What can we conclude? 
# Nothing! I made up these data.
# Even if I hadn't, we only asked 15 imaginary students, 
# and one of them refused to tell us how much Netflix he watched!
# But what if we had sampled randomly from the UC Davis undergrad population
# and had 500 responses from students, how do we know if a correlation
# this size is likely to happen just by accident in 500 observations?
# Stay tuned for next week, when we'll talk about that :)
