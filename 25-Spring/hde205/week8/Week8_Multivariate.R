##########################################################################
###                 HDE 205: Longitudinal Data                         ###
###                       Multivariate LGM                             ### 
##########################################################################


##### Load 'Lavaan package' #####
library(lavaan)

#####  Reading in data  #####
# Specifying the location of the file:
my.data <- read.csv(file="C:\\Users\\siwei-admin\\Dropbox\\HDE 205\\WISC data\\wisc3raw.csv",
                    header=TRUE,na.strings=" ")

## Multivariate Linear LGM

MLlgm <- '
#Level/Intercept (all constraint to 1)
InterV =~ 1*verb1 + 1*verb2 + 1*verb4 + 1*verb6;
InterP =~ 1*perfo1 + 1*perfo2 + 1*perfo4 + 1*perfo6;

#Slope
SlopeV =~ 0*verb1 + 1*verb2 + 3*verb4 + 5*verb6;
SlopeP =~ 0*perfo1 + 1*perfo2 + 3*perfo4 + 5*perfo6;

#Residuals (Equality constraints)
verb1 ~~ label(VeV)*verb1;
verb2 ~~ label(VeV)*verb2;
verb4 ~~ label(VeV)*verb4;
verb6 ~~ label(VeV)*verb6;

perfo1 ~~ label(VeP)*perfo1;
perfo2 ~~ label(VeP)*perfo2;
perfo4 ~~ label(VeP)*perfo4;
perfo6 ~~ label(VeP)*perfo6;

#Intercept & slope means
InterV~1;
InterP~1;
SlopeV~1;
SlopeP~1;

#Intercept & slope variances
InterV ~~ InterV;
SlopeV ~~ SlopeV;
InterV ~~ SlopeV;
InterP ~~ InterP;
SlopeP ~~ SlopeP;
InterP ~~ SlopeP;
InterV ~~ InterP;
InterV ~~ SlopeP;
SlopeV ~~ InterP;
SlopeV ~~ SlopeP;
'

#creating a new object 'MLlgm' & running a SEM model
fit_MLlgm <- lavaan(MLlgm, data=my.data)

#getting summary of model estimates (fit & parameters)
summary(fit_MLlgm, fit.measures=TRUE)


## Bivariate Autoregressive Model (Stability Model)
# Construct correlation matrix of variables of interests
variables <- my.data[,c(3:5,7:9)]  # Selecting variables in grades 2, 4, and 6 only
dat.cor <- cor(variables) # Correlation matrix
dat.cor

BAR <- '
verb4 ~ verb2
verb6 ~ verb4
perfo4 ~ perfo2
perfo6 ~ perfo4

#Variances and Covariances
verb2 ~~ perfo2
verb2 ~~ 1*verb2
perfo2 ~~ 1*perfo2
verb4 ~~ verb4
verb6 ~~ verb6
perfo4 ~~ perfo4
perfo6 ~~ perfo6
verb4 ~~ perfo4
verb6 ~~ perfo6
'

fit_BAR <- lavaan(BAR, sample.cov=dat.cor,sample.nobs=204)

#getting summary of model estimates (fit & parameters)
summary(fit_BAR, fit.measures=TRUE)

## Adding equality constraints to stability coefficients for parsimony
BAR_EQ <- '
verb4 ~ label(ARV)*verb2
verb6 ~ label(ARV)*verb4
perfo4 ~ label(ARP)*perfo2
perfo6 ~ label(ARP)*perfo4

#Variances and Covariances
verb2 ~~ perfo2
verb2 ~~ 1*verb2
perfo2 ~~ 1*perfo2
verb4 ~~ verb4
verb6 ~~ verb6
perfo4 ~~ perfo4
perfo6 ~~ perfo6
verb4 ~~ perfo4
verb6 ~~ perfo6
'

fit_BAR_EQ <- lavaan(BAR_EQ, sample.cov=dat.cor,sample.nobs=204)

#getting summary of model estimates (fit & parameters)
summary(fit_BAR_EQ, fit.measures=TRUE)

#Relative model fit test
anova(fit_BAR,fit_BAR_EQ) #Constraints did not significantly worsen the fit. Proceed with contraints.



## Lagged Regression P Predicted by V
LagPV_EQ <- '
verb4 ~ label(ARV)*verb2
verb6 ~ label(ARV)*verb4
perfo4 ~ label(ARP)*perfo2 + verb2
perfo6 ~ label(ARP)*perfo4 + verb4

#Variances and Covariances
verb2 ~~ perfo2
verb2 ~~ 1*verb2
perfo2 ~~ 1*perfo2
verb4 ~~ verb4
verb6 ~~ verb6
perfo4 ~~ perfo4
perfo6 ~~ perfo6
verb4 ~~ perfo4
verb6 ~~ perfo6
'

fit_LagPV_EQ <- lavaan(LagPV_EQ, sample.cov=dat.cor,sample.nobs=204)

#getting summary of model estimates (fit & parameters)
summary(fit_LagPV_EQ, fit.measures=TRUE)

#Relative model fit test
anova(fit_BAR_EQ,fit_LagPV_EQ)



## Lagged Regression V Predicted by P
LagVP_EQ <- '
verb4 ~ label(ARV)*verb2 + perfo2
verb6 ~ label(ARV)*verb4 + perfo4
perfo4 ~ label(ARP)*perfo2 
perfo6 ~ label(ARP)*perfo4 

#Variances and Covariances
verb2 ~~ perfo2
verb2 ~~ 1*verb2
perfo2 ~~ 1*perfo2
verb4 ~~ verb4
verb6 ~~ verb6
perfo4 ~~ perfo4
perfo6 ~~ perfo6
verb4 ~~ perfo4
verb6 ~~ perfo6
'

fit_LagVP_EQ <- lavaan(LagVP_EQ, sample.cov=dat.cor,sample.nobs=204)

#getting summary of model estimates (fit & parameters)
summary(fit_LagVP_EQ, fit.measures=TRUE)

#Relative model fit test
anova(fit_BAR_EQ,fit_LagVP_EQ)


## Cross-Lag Regression 
CLM_EQ <- '
verb4 ~ label(ARV)*verb2 + perfo2
verb6 ~ label(ARV)*verb4 + perfo4
perfo4 ~ label(ARP)*perfo2 + verb2
perfo6 ~ label(ARP)*perfo4 + verb4

#Variances and Covariances
verb2 ~~ perfo2
verb2 ~~ 1*verb2
perfo2 ~~ 1*perfo2
verb4 ~~ verb4
verb6 ~~ verb6
perfo4 ~~ perfo4
perfo6 ~~ perfo6
verb4 ~~ perfo4
verb6 ~~ perfo6
'

fit_CLM_EQ <- lavaan(CLM_EQ, sample.cov=dat.cor,sample.nobs=204)

#getting summary of model estimates (fit & parameters)
summary(fit_CLM_EQ, fit.measures=TRUE)

#Relative model fit test
anova(fit_BAR_EQ,fit_CLM_EQ)



## Cross-Lag Regression with constraints
CLM_EQ2 <- '
verb4 ~ label(ARV)*verb2 + label(VP)*perfo2
verb6 ~ label(ARV)*verb4 + label(VP)*perfo4
perfo4 ~ label(ARP)*perfo2 + label(PV)*verb2
perfo6 ~ label(ARP)*perfo4 + label(PV)*verb4

#Variances and Covariances
verb2 ~~ perfo2
verb2 ~~ 1*verb2
perfo2 ~~ 1*perfo2
verb4 ~~ verb4
verb6 ~~ verb6
perfo4 ~~ perfo4
perfo6 ~~ perfo6
verb4 ~~ perfo4
verb6 ~~ perfo6
'

fit_CLM_EQ2 <- lavaan(CLM_EQ2, sample.cov=dat.cor,sample.nobs=204)

#getting summary of model estimates (fit & parameters)
summary(fit_CLM_EQ2, fit.measures=TRUE)

#Relative model fit test
anova(fit_CLM_EQ,fit_CLM_EQ2)




