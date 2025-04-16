############################################################################################################
# Week 1: Data Management and Descriptives 
############################################################################################################

############################################################################################################
# R Basics
############################################################################################################
# 1. Codes starting with "#" are comments
# 2. If you want to turn a block of codes into comments, use Ctrl+Shift+c
# 3. R code is case sensitive! 
A <- 1
a <- 2
# 4. Use class() to identify the type of objects
class(A)
B <- array(1:4)
class(B)
C <- matrix(1:4,nrow=2,ncol=2,byrow=TRUE)
class(C)
# 5. You can remove an object after you create it
rm(A)
rm(list=ls()) # This would remove everything
# 6. You can look up a function using ?
?matrix
# 7. If you can't find a function using ?, try ??
??describe
# 8. When specifying a folder, the backward slash '\' needs to be changed into a forward slash '/' or double slash '\\'
# 9. Installing and loading an R package
install.packages("psych")
library(psych)

############################################################################################################
# Inputing and Managing Data
############################################################################################################

### Reading in a csv file
# Inputing data
# Here I include code for importing csv and excel files. 
# If your data set is in a different format, you would need a different function.
# Google is your best friend!
wiscraw <- read.csv('C:/Users/siwei-admin/Dropbox/HDE 205/WISC data/wisc3raw.csv',na.strings="NA")


library(readxl)
wiscraw_excel <- read_excel("C:/Users/siwei-admin/Dropbox/HDE 205/WISC data/wisc3raw.xls",na="NA")

# WISC (Wechsler Intelligence Scale for Children) data: 204 children measured in grade 1, 2, 4, and 6
# verb - verbal score (Information, Comprehension, Similarities, Vocabulary subscales)
# perfo - performance score (Picture Completion, Matrix Reasoning, etc.)
# momed - mother's education (continuous)
# grad - mother graduated from high school


# Identifying the type of object
class(wiscraw)

# Dimensions of the data
dim(wiscraw)

# Look at variable names
names(wiscraw)

# Select a variable by name
wiscraw$verb1

# Select a column or a row
wiscraw[,1]
wiscraw[1,]

# Select a range of values
wiscraw[1,1:5]
wiscraw[1,c(1:5,18)]

# Creating a simpler data set that contains the verb variables only
verb <- wiscraw[,1:5] 

# Look at first few rows of the new data set
head(verb)
head(verb,10)

# Look at the last few rows of the new data set
tail(verb)

# Summary of the data
summary(verb)

# Want more descriptive statistics?
describe(verb)

# Correlations
corr.test(verb[,2:5])

# Correlations with no p-value adjustments, 3 decimal places, and confidence intervals
print(corr.test(verb[,2:5],adjust="none"),digits=3,short=FALSE)


# Transform from wide to long
list1 <- 2:5 # indicators for verb
list2<- 6:9 # indicators for perfo
wiscraw_long <- reshape(wiscraw,idvar="id",v.names=c("verb","perfo"),varying=list(list1,list2),times=c(1,2,4,6), 
                        direction="long")
wiscraw_long <- wiscraw_long[order(wiscraw_long$id),] # sort data by id


# Transform from long to wide
wiscraw_wide <- reshape(wiscraw_long,idvar="id",timevar="time",v.names=c("verb","perfo"),direction="wide")
colnames(wiscraw_wide)[13:20] <- c("verb1","perfo1","verb2","perfo2","verb4","perfo4","verb6","perfo6")


############################################################################################################
# Plotting Data
############################################################################################################
library(ggplot2) # Remember: you will need to first install the package if you have not already done so!

# Define base for the graph and store in object 'p'
p <- ggplot(data = wiscraw_long, aes(x = time, y = verb, group = id))

# Plotting data points
p + geom_point()

# Spaghetti plot
p + geom_line()

# Plotting both
p + geom_point() + geom_line()

# If your sample size is very big, you may want to plot a subset of the sample only
# Randomly select half of the sample
half <- wiscraw[sample(204, 102), ] 

# Transform to long format
half_long <- reshape(half,idvar="id",v.names=c("verb","perfo"),varying=list(list1,list2),times=c(1,2,4,6), 
                        direction="long")
half_long <- half_long[order(half_long$id),] # sort data by id

# Plot half sample
ph <- ggplot(data = half_long, aes(x = time, y = verb, group = id))
ph + geom_point() + geom_line()

# see fancier options here: https://stats.oarc.ucla.edu/r/faq/how-can-i-visualize-longitudinal-data-in-ggplot2/


############################################################################################################
# Saving Data on the Computer
############################################################################################################
write.csv(wiscraw_long,"C:/Users/sweliu/Dropbox/HDE 205/WISC data/wisc3raw_long.csv")
