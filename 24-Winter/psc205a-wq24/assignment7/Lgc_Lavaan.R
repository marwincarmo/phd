#=================================================#
# LGC Lavaan
# http://lavaan.ugent.be/
#=================================================#

#Install Lavaan
install.packages("lavaan", dependencies=TRUE)

#Checking
library(lavaan)
example(cfa)

# Open script from R
# Ctrl+R for sending commands

# Setting the directory and importing data
setwd('C:/Teaching/PSC 205A/Session_08_Longitudinal');

getwd(); # with
getwd; # without
dir();

# Help for importing data
?read.table;

# Importing with missing data
wisc <-read.table('wiscraw.dat', header=FALSE, na.strings='.');

# Alternative to load the data
wisc<-read.table("C:/Teaching/PSC 205A/Session_08_Longitudinal/wiscraw.dat", na.string='.')

# Give the variable names
names(wisc)<-c('id','v1','v2','v4','v6','nv1','nv2','nv4','nv6','momed','wisc1','wisc2','wisc4','wisc6','cte');

# Clear the screen
Ctrl+L

# Descriptives
summary(wisc)

# Attach the data
attach(wisc)

# In case we want to select variables
myvars <- c("v1", "v2", "v3")
newdata <- mydata[myvars]

# Selecting Observations
temp1 <- estimates[ which(estimates$task < 4 & estimates$female==0), ]
summary(temp1)


### NO GROWTH MODEL

myModel = "
	# factor loadings
	level =~ 1*wisc1
	level =~ 1*wisc2
	level =~ 1*wisc4
	level =~ 1*wisc6
	# means
	level ~ 1
	wisc1 ~ 0
	wisc2 ~ 0
	wisc4 ~ 0
	wisc6 ~ 0
	# variances
	wisc1 ~~ resvar*wisc1
	wisc2 ~~ resvar*wisc2
	wisc4 ~~ resvar*wisc4
	wisc6 ~~ resvar*wisc6
	level ~~ level
	"
fit.nogrowth = sem(myModel,data=wisc,fixed.x=FALSE)
summary(fit.nogrowth,fit.measures=T)



### LINEAR GROWTH MODEL

myModel = "
	#factor loadings
	level =~ 1*wisc1
	level =~ 1*wisc2
	level =~ 1*wisc4
	level =~ 1*wisc6
	# linear growth
	slope =~ 0*wisc1
	slope =~ 1*wisc2
	slope =~ 3*wisc4
	slope =~ 5*wisc6
	# means
	level ~ 1
	slope ~ 1
	wisc1 ~ 0
	wisc2 ~ 0
	wisc4 ~ 0
	wisc6 ~ 0
	# variances
	wisc1 ~~ resvar*wisc1
	wisc2 ~~ resvar*wisc2
	wisc4 ~~ resvar*wisc4
	wisc6 ~~ resvar*wisc6
	level ~~ level
	slope ~~ slope
	level ~~ slope
	"
fit.linear = sem(myModel,data=wisc,fixed.x=FALSE)
summary(fit.linear,fit.measures=T)



### LATENT BASIS MODEL

myModel = "#factor loadings
	level =~ 1*wisc1
	level =~ 1*wisc2
	level =~ 1*wisc4
	level =~ 1*wisc6
	# linear growth
	slope =~ 0*wisc1
	slope =~ wisc2
	slope =~ wisc4
	slope =~ 1*wisc6
	# not yet
	# means
	level ~ 1
	slope ~ 1
	wisc1 ~ 0
	wisc2 ~ 0
	wisc4 ~ 0
	wisc6 ~ 0
	# variances
	wisc1 ~~ resvar*wisc1
	wisc2 ~~ resvar*wisc2
	wisc4 ~~ resvar*wisc4
	wisc6 ~~ resvar*wisc6
	level ~~ level
	slope ~~ slope
	level ~~ slope
	"
fit.latentbasis = sem(myModel,data=wisc,fixed.x=FALSE)
summary(fit.latentbasis,fit.measures=T)


### LINEAR GROWTH MODEL WITH COVARIATE

myModel = 
      "#factor loadings
	level =~ 1*wisc1
	level =~ 1*wisc2
	level =~ 1*wisc4
	level =~ 1*wisc6
	# linear growth
	slope =~ 0*wisc1
	slope =~ 1*wisc2
	slope =~ 3*wisc4
	slope =~ 5*wisc6
	# means
	level ~ 1
	slope ~ 1
	wisc1 ~ 0
	wisc2 ~ 0
	wisc4 ~ 0
	wisc6 ~ 0
	# variances
	wisc1 ~~ resvar*wisc1
	wisc2 ~~ resvar*wisc2
	wisc4 ~~ resvar*wisc4
	wisc6 ~~ resvar*wisc6
	level ~~ level
	slope ~~ slope
	level ~~ slope
	# regressions
	level ~ momed
	slope ~ momed
	"
fit.covariate = sem(myModel,data=wisc,fixed.x=FALSE)
summary(fit.covariate,fit.measures=T)

