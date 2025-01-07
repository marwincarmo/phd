##########################################################################
###                           PSC 103B                                 ###
###                         Lab Session 1                              ###
###                                                                    ###
###                       Introduction to R                            ###
###                                                                    ###
###                     Instructor: Marwin Carmo                       ### 
##########################################################################


## R as a calculator ----
#########################


# # Common mathematic operations in R:
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

sqrt(16)

# To run a line of code just use CTRL + ENTER (that's COMMAND + ENTER if you're on a Mac)

# How would I ask R to divide 10 by 2? 

# How would I ask R to calculate 5 to the 4th power? 

# The R Calculator follows the PEMDAS rule
# Parentheseis, exponents, multiplication, division, addition, subraction
# from left to right 

3 + 4 * 12

# Follwing PEMDAS, R will first multiply 4 by 12 and then add 3 to the product 
# If I want R to add 3 and 4 first and then multiple their sum by 12
# I have to add parenthesis 

(3 + 4) * 12

## Commenting----
##################

# You may have already noticed one convention in R -- how to write comments
# Anything that you type after the "#" will be disregarded by R
# so you can make notes to yourself and it will not interfere with your code


## Creating Objects----
#######################

# One of the most important things we can do in R 
# is store information as an object 
# You can think of an object as a label for a piece of information 
# or multiple pieces of information

# We can save information as variables by using the assignment operator: <- 
# All assignements follow a general pattern 
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
# options TRUE and FALSE

# They must be in all caps

TRUE

true

# You could also abbreviate TRUE to T and FALSE to F

T

F

# However, I personally do not recommend abbreviating TRUE and FALSE
# to T and F, because you might accidentally assign an object with the
# label T or F (for example, the test statistic in ANOVA is called
# the F statistic, so students are tempted to label it F when 
# calculating it by hand in R)
# However, that now overwrites the original meaning of T and F, and you
# may not have been aware that you did it

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

# We could also create a character vector

character_vector <- c("one", "two", "three", "four")

# Subsetting vectors----
#########################

# Sometimes we want to pull out one element from a vector
# that has multiple elements
# To do this we need to subset the vector 
# We can use sqaure brackets [] to subset

# We can use square brackets after the label name,
# with the element number we would like to remove inside the brackets

# What if I want to pull out the third element from a vector? 
character_vector[3]

# How would you take out the first element? 

# We can save this as a new object
third.element <- character_vector[3]

# We can also remove multiple elements at once

character_vector[c(2, 3, 4)]

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

# Or you can press tab one your cursor is within the function parenthesis 

round()

# Or you can google it. Example google search: 'round function R'


# Another useful function is class()
# The class function will tell you what kind of data type 
# is saved within an object

class(character_vector)
class(first_vector)

## Data Structures 
#####################################################################

# So far we've only worked with vectors:

# But there are other data structures that you need to be 
# familiar with 

# Matrix: 2-dimensional dataset (has columns and rows) of one data type 

# We can use a new function matrix() to create this data structure
# There are a couple important arguments
# matrix(data, nrow, ncol, byrow)

m <- matrix(data = c(1:6), # The matrix will contain elements 1, 2, 3, 4, 5, 6
            nrow = 2, # this matrix will have two rows
            ncol = 3, # this matrix will have three columns
            byrow = TRUE) # we fill the elements by the row
# What happens if you change byrow = FALSE ?

# New function!
# dim() tells you the dimension of a matrix. How many columns and rows

dim(m) # First element is number of rows, second element is number of columns

## rbind() and cbind()
# You can also create a matrix by combining vectors

vec1 <- 1:5
vec2 <- 6:10

# If I want to make each vector their own column in a matrix 
# I can use the cbind() function 

col_matrix <- cbind(vec1, vec2)

# Or I could make each vector their own row in a matrix 
# I can use the rbind() function

row_matrix <- rbind(vec1, vec2)

# Data frames: Allow you to have multiple data types. 

# We can use the data.frame() function to create a data frame
# Each argument is a different column
# We can also add labels to each column 

dat <- 	data.frame(
  schoolyear = c("Freshman", "Sophomore","Freshman"), 
  height = c(152, 171.5, 165), 
  weight = c(81,93, 78),
  age = c(18,20,19)
)

## Subsetting Two-Dimensional Objects----
##########################################

# We've used square brackets [] to subset vectors 
# We can use square brackets [] to subset two-dimensional objects
# like a matrix or data frame, but we have to change the
# information we provide

#twoDimObject[row#, column#]

# If I want to subset the element in from 3 and column 2
dat[3, 2]

# If I want to subset the entire 3rd column

dat[, 3]

# I could also subset by column name 

dat[, "weight"] # this subsets all the rows from the weight column

dat[1, "weight"] # this subsets the first row from the weight column

# If we just want to subset a column we can use the $ operator 

# twoDimObject$ColumnName

dat$schoolyear # Subsets the column labeled schoolyear