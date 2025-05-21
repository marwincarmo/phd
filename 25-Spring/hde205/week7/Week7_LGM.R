##########################################################################
###                 HDE 205: Longitudinal Data                         ###
###                       Univariate LGM                               ### 
##########################################################################


##### Install & load 'Lavaan package' #####

install.packages("lavaan")
library(lavaan)

??lavaan
?library

## SEM operators

# Define an object (Note the difference between upper and lower case letters!)
A <- 3
a <- array(1:5)
A
a

class(A)
class(a)

#Latent variable
lv =~ var.x + var.y

#Regression
var.y ~ covariates

#Residual, variance, covariance
lv ~~ lv #variance
var.y ~~ var.y #residual
var.y ~~ var.x #covariance

#Means, intercetps
lv ~ 1
var.y ~ 1

#Constraint
.5*var.y #fixing this coefficient to .5
w*var.y; w*var.x; #equality constraint

#####  Reading in data  #####
# Specifying the location of the file:
my.data <- read.csv(file="C:\\Users\\sweliu\\Desktop\\WISC data\\wisc3raw.csv",
                    header=TRUE,na.strings=" ")

# Alternatively, browse computer for file:
my.data <- read.csv(file = file.choose(), header = TRUE)

#####  Univariate LGM  #####

## Unconditional means LGM

Ulgm <- '

#Level/Intercept (all constraint to 1)
Inter =~ 1*verb1 + 1*verb2 + 1*verb4 + 1*verb6;

#Residuals (equality constraints)
verb1 ~~ label(Ve)*verb1;
verb2 ~~ label(Ve)*verb2;
verb4 ~~ label(Ve)*verb4;
verb6 ~~ label(Ve)*verb6;

#Intercept mean
Inter~1; 

#Intercept variance
Inter~~Inter;
'

#creating a new object 'fit_Ulgm' & running a SEM model
fit_Ulgm <- lavaan(Ulgm, data=my.data)

#getting summary of model estimates (fit & parameters)
summary(fit_Ulgm, fit.measures=TRUE)

#getting model parameter estimates only
parameterEstimates(fit_Ulgm)

#getting model fit estimates only
fitMeasures(fit_Ulgm)

## Linear LGM

Llgm <- '
#Level/Intercept (all constraint to 1)
Inter =~ 1*verb1 + 1*verb2 + 1*verb4 + 1*verb6;

#Slope
Slope =~ 0*verb1 + 1*verb2 + 3*verb4 + 5*verb6;

#Residuals (Equality constraints)
verb1 ~~ label(Ve)*verb1;
verb2 ~~ label(Ve)*verb2;
verb4 ~~ label(Ve)*verb4;
verb6 ~~ label(Ve)*verb6;

#Intercept & slope means
Inter~1; 
Slope~1;

#Intercept & slope variances
Inter ~~ Inter;
Slope ~~ Slope;
Inter ~~ Slope;
'

#creating a new object 'Llgm' & running a SEM model
fit_Llgm <- lavaan(Llgm, data=my.data)

#getting summary of model estimates (fit & parameters)
summary(fit_Llgm, fit.measures=TRUE)

#Relative model fit test
anova(fit_Ulgm, fit_Llgm)


## Quadratic LGM

Qlgm <- '
#Level/Intercept (all constraint to 1)
Inter =~ 1*verb1 + 1*verb2 + 1*verb4 + 1*verb6;

#Linear Slope
Lin =~ 0*verb1 + 1*verb2 + 3*verb4 + 5*verb6;

#Quadratic Slope
Qua =~ 0*verb1 + 1*verb2 + 9*verb4 + 25*verb6;

#Residuals (Equality constraints)
verb1 ~~ label(Ve)*verb1;
verb2 ~~ label(Ve)*verb2;
verb4 ~~ label(Ve)*verb4;
verb6 ~~ label(Ve)*verb6;

#Intercept & slope means
Inter~1; 
Lin~1;
Qua~1;

#Intercept & slope variances
Inter ~~ Inter;
Lin ~~ Lin;
Qua ~~ Qua;
Inter ~~ Lin;
Inter ~~ Qua;
Lin ~~ Qua;
'

#creating a new object 'fit_Qlgm' & running a SEM model
fit_Qlgm <- lavaan(Qlgm, data=my.data)

#getting summary of model estimates (fit & parameters)
summary(fit_Qlgm, fit.measures=TRUE)

#Relative model fit test
anova(fit_Llgm, fit_Qlgm)

#examinng modification indices
modindices(fit_Qlgm) # Caution: Only change your model if you have theoretical support!


# Latent basis model
LBlgm <- '
#Level/Intercept (all constraint to 1)
Inter =~ 1*verb1 + 1*verb2 + 1*verb4 + 1*verb6;

#Lateng basis slope
Slope =~ 0*verb1 + 1*verb2 + verb4 + verb6;

#Residuals (Equality constraints)
verb1 ~~ verb1;
verb2 ~~ verb2;
verb4 ~~ verb4;
verb6 ~~ verb6;

#Intercept & slope means
Inter~1; 
Slope~1;


#Intercept & slope variances
Inter ~~ Inter;
Slope ~~ Slope
Inter ~~ Slope
'

#creating a new object 'fit_LBlgm' & running a SEM model
fit_LBlgm <- lavaan(LBlgm, data=my.data)

#getting summary of model estimates (fit & parameters)
summary(fit_LBlgm, fit.measures=TRUE)


## Linear LGM with Group

Llgm_group <- '
#Level/Intercept (all constraint to 1)
Inter =~ 1*verb1 + 1*verb2 + 1*verb4 + 1*verb6;

#Slope
Slope =~ 0*verb1 + 1*verb2 + 3*verb4 + 5*verb6;

#Residuals (Equality constraints)
verb1 ~~ label(Ve)*verb1;
verb2 ~~ label(Ve)*verb2;
verb4 ~~ label(Ve)*verb4;
verb6 ~~ label(Ve)*verb6;

#Intercept & slope 
Inter ~ 1 + factor(grad); 
Slope ~ 1 + factor(grad);

#Intercept & slope variances
Inter ~~ Inter;
Slope ~~ Slope;
Inter ~~ Slope
'

#creating a new object 'Ulgm.L' & running a SEM model
fit_Llgm_group <- lavaan(Llgm_group, data=my.data)

#getting summary of model estimates (fit & parameters)
summary(fit_Llgm_group, fit.measures=TRUE)


