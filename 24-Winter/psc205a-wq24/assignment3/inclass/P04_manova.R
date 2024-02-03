# PSC 205A 
# MANOVA

# Getting the data (MHS)
#mhs <- read.table("C:/Teaching/PSC 205A/Session_04_MANOVA/mhs_manova_short.csv", na.strings = '.') 
mhs <- read.table("C:/Teaching/PSC 205A/Session_04_MANOVA/mhs_manova_short.dat", na.strings = '.')
colnames(mhs) <- c("id","female","age_gr","pc1","mot1","pw1","app1")
#mhs = subset(mhs, select = c("female","age_gr","pc1","mot1","pw1","app1"))
summary(mhs)
mhs

table(mhs$female)
with (mhs, table(female, age_gr))
table(mhs$female, mhs$age_gr)
with(mhs, xtabs(~ female + age_gr))

#EDA
pairs(mhs[,2:7], pch = 16)
cor(mhs[,2:7])

# Combining the DVs
Y <-cbind(mhs[,4], mhs[,5], mhs[,6], mhs[,7])

ggdensity(mhs$mot1,
          main = "Density plot of Motivation", xlab = "mot1")

# Specifying factors
female=factor(mhs[,2], labels=c("male","female"))
age_gr=factor(mhs[,3], labels=c("young","old"))

female=factor(mhs[,2])
age_gr=factor(mhs[,3])

# Fit the model
fit <- manova(Y ~ age_gr + female + age_gr*female)
summary(fit, test = "Wilks") # Multivariate test
summary.aov(fit, test = "Wilks") # Univariate test

library(HoRM)
SSCP.fn(fit)

# No interaction; examining  the various group means
fit <- manova(Y ~ age_gr + female)
summary(fit, test = "Wilks") 
by (Y, age_gr, mean)
by (Y, female, mean)
by (Y, list(female, age_gr), mean)

# Linear Discriminant Analysis
library(MASS)
fit1 <-lda(Y ~ female, data=mhs)
fit1

fit2 <-lda(Y ~~ female + age_gr + female*age_gr, data=mhs, na.action = "na.omit", CV=TRUE)
fit2


model<-lda(Y ~~ female + age_gr + female*age_gr, data=mhs, na.action = "na.omit", CV=TRUE)
fit
model


# make predictions
predictions<-model%>%predict(test.transformed) 
# model accuracy
mean(predictions$class==test.transformed$Y)


# Assess the accuracy of the prediction
# percent correct for each category of Y
ct <- table(mydata$Y, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))






# Getting the data (Tabachnick & Fidell)
tf <- read.table("C:/Teaching/PSC 205A/Session_04_MANOVA/manova_tf.dat") 
colnames(tf) <- c("id", "read", "math", "iq", "disable", "treatment")
summary(tf)

table(tf$disable)
with (tf, table(disable, treatment))
table(tf$disable, tf$treatment)
with(tf, xtabs(~ disable + treatment))

#EDA
pairs(tf[,2:6], pch = 16)
cor(tf[,2:6])

# Combining the DVs
Y <-cbind(tf[,2], tf[,3])
Y <-cbind(tf[,2], tf[,3], tf[,4])

ggdensity(df$read,
          main = "Density plot of read", xlab = "read")

# Specifying factors
disable=factor(mhs[,5], labels=c("mild", "medium", "severe"))
treatment=factor(mhs[,6], labels=c("control", "treatment"))

disable=factor(mhs[,5])
treatment=factor(mhs[,6])

# Fit the model
fit <- manova(Y ~ treatment + disable + treatment*disable)
summary(fit, test = "Wilks") # Multivariate test
summary.aov(fit, test = "Wilks") # Univariate test
SSCP.fn(fit)


# No interaction; examining  the various group means
fit <- manova(Y ~ treatment + disable)
summary(fit, test = "Wilks") 
by (Y, treatment, mean)
by (Y, disable, mean)
by (Y, list(disable, treatment), mean)