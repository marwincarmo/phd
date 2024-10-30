#psc 290
#lecture 5: nonnormal/ordinal/missing multivariate data 
library(GGally)
library(MASS)
#goals for this week: 


## use Cholesky trick to transform univariate data ############################# 
set.seed(243872389)
orthDat <- matrix(runif(4000, 0, 1), 1000, 4) #generate orthogonal variables
orthDat <- (orthDat - mean(orthDat))/sd(orthDat) #rescale to standardize
round(cov(orthDat), 4)                  #check their cov matrix
#standardize these variables to have variance 1: 


covmat <- matrix(.5, 4, 4) #create 4-by-4 covariance matrix
diag(covmat) <- 1:4        #put some new variances, put in the diagonal
Lt <- chol(covmat)         #chol() gives Cholesky matrix
cordat <- orthDat %*% Lt   #transform the orthogonal data
cov(cordat)                #examine its new covariance matrix

#ok cool so we transformed our uniform variables to have the
# correlation structure we want. let's look at the distributions: 
df <- data.frame(cordat)
ggpairs(df, aes(alpha = 0.4), 
        diag = list(continuous = wrap("barDiag", binwidth=0.3)))
#problem: marginal distributions are no longer uniform
################################################################################ 

## start with MVN data and then go backward: ################################### 
covmat <- matrix(.5, 4, 4) #create 4-by-4 covariance matrix
diag(covmat) <- 1          #make this a correlation matrix
set.seed(3363663)
df2 <- mvrnorm(Sigma = covmat, mu = rep(0, 4), n = 1000)

df2.p <- pnorm(df2) #make it uniform
df2.q <- data.frame(qexp(df2.p))

#ggpairs(df2.p, aes(alpha = 0.4), 
#        diag = list(continuous = wrap("barDiag", binwidth=0.1)))

ggpairs(df2.q, aes(alpha = 0.4), 
        diag = list(continuous = wrap("barDiag", binwidth=0.3)))
#problem: correlations are too low
################################################################################ 

## NORTA method ################################################################ 
#this code from Oscar Olvera Astivia's lecture notes#

#to install from Po Su's github page (but it won't work):
#remotes::install_github("superdesolator/NORTARA")
#library(nortaRA) #NORTA method for generating nonnormal data 

# code to install NORTARA code from Po Su in ShangHai 
# this package was uploaded to github in 2014 and not touched
# there is now a line of code that throws an error
# to fix it I put all the functions in "nortaraFunctions.R"
# to use the package, source that file: 
source("nortaraFunctions.R")

#create a vector of distribution families for each variable
quant_margs <- c("qunif", "qf", "qchisq", "qbeta") 

#create a list that contains the distribution parameters for each variable
param_margs <- list(
  munif = list(min=0, max=1),
  mf = list(df1=1, df2=30),
  mchisq = list(df=1),
  mbeta = list(shape1 = .5, shape2 = 1)                 
)

#create 4-by-4 correlation matrix
R <- matrix(c(1, .3, .4, .5,
              .3,  1, .7, .8,
              .4, .7,  1, .7,
              .5, .8, .7,  1 ), 4, 4)

df3 <- data.frame(gennortaRA(n = 5000, cor_matrix = R, invcdfnames = quant_margs, 
                   paramslists = param_margs))

ggpairs(df3, aes(alpha = 0.4),
        diag = list(continuous = wrap("barDiag", binwidth=.1)))
################################################################################ 


## in-class exercise ########################################################### 
source("nortaraFunctions.R") #save this file in your working directory

quant_margs <- c("qexp", "qexp") 

#create a list that contains the distribution parameters for each variable
param_margs <- list(
  mexp = list(rate = 1),
  mexp = list(rate = 3)                 
)

#create 2-by-2 correlation matrix
R <- matrix(c(1, .8, 
              .8,  1), 2, 2)

corvec <- NULL
for (i in 1:1000){
  df <- data.frame(gennortaRA(n = 100, cor_matrix = R, invcdfnames = quant_margs, 
                              paramslists = param_margs))
  
  corvec[i] <- cor(df)[1,2]
}

hist(corvec, breaks = seq(0, 1, by = .01))
################################################################################ 

## ordinal data plot (conceptual) ############################################## 
x <- rnorm(200, mean = 5, sd = 1)
y <- rnorm(200, mean = x, sd = 2)

plot(x,y, pch = 20,
     xlab = ("how often do you go to parties"),
     ylab = ("how much do you like people"),
     xaxt = 'n', yaxt = 'n')

abline(h = c(2, 3, 7))
abline(v = c(4, 4.5, 6.5))

text(x = c(2.5, 4.25, 5.5, 7.5), 
     y = rep(-2, 4), 
     c("never", "rarely", "sometimes", "often"))

text(x = rep(2, 4), 
     y = c(-.5, 2.5, 5, 9), 
     c("not at all", "a little", "somewhat", "a lot"))

################################################################################ 


## simulating ordinal data ##################################################### 
################################################################################ 
