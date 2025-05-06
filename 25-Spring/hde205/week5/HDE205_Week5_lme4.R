############################################################################################################
# Week 5: Growth Modeling - Basic Models, Polynomial Models, and Conditional Models
############################################################################################################*This script provides 3 ways to conduct ANOVA analysis on repeated measures,

library(psych)

# Inputing data
#wiscraw <- read.csv('C:/Users/siwei-admin/Dropbox/HDE 205/WISC data/wisc3raw.csv')
wiscraw <- read.csv('C:/Users/sweliu/Dropbox/HDE 205/WISC data/wisc3raw.csv')

# Transform from wide to long
list1 <- 2:5 # indicators for verb
list2<- 6:9 # indicators for perfo
wiscraw_long <- reshape(wiscraw,idvar="id",v.names=c("verb","perfo"),varying=list(list1,list2),times=c(1,2,4,6), 
                        direction="long")
wiscraw_long <- wiscraw_long[order(wiscraw_long$id),] # sort data by id

#### plot data
library(ggplot2) 

# define base for the graph and store in object 'p'
p <- ggplot(data = wiscraw_long, aes(x = time, y = verb, group = id))

# spaghetti plot
p + geom_point() + geom_line()

#### Running a regression for each individual separately - and saving parameters;
#### Within-person regressions;
#### Equation: verb(t) = B0 + B1*time(t) + e(t);

# create data frame to store individual intercepts and slopes
istats <- data.frame(id=1:204,iintercept=rep(NA,204),islope=rep(NA,204))

# run individual regressions
for (i in 1:204){
  datai <- wiscraw_long[which(wiscraw_long$id == i),] # subset data for each individual
  
  istats$iintercept[i] <- coefficients(lm(verb~time,data=datai))[1]
  istats$islope[i] <- coefficients(lm(verb~time,data=datai))[2]
  rm(datai)
}

# descriptives and correlation
describe(istats[,2:3])
corr.test(istats[,2:3])

# merge istats with verb_long
wiscraw_long <- merge(wiscraw_long,istats,by="id")

# calculate predicted values using the individual regression parameters obtained above
# Y(t) = B0 + B1*time(t) + e(t);
wiscraw_long$verbipred <- wiscraw_long$iintercept+wiscraw_long$islope*wiscraw_long$time + 0

# plot predicted individual change
p <- ggplot(data = wiscraw_long, aes(x = time, y = verbipred, group = id))
p + geom_point(color="blue") + geom_line(color="blue")

############################################################################################################
# Multilevel Model - Unconditional Means Model
############################################################################################################
# Used for calculating ICC (relative amount of between and within variance)
# Also can be thought of as a no-growth model
library(lmerTest) 
# We use the lmer function in the lmerTest package because it includes p-values of fixed effects
# Alternatively, you can use the lme4 package, which does not include the p-values

m1 <- lmer(verb ~ 1 + (1|id), REML=FALSE, data=wiscraw_long)
summary(m1)

# ICC= 12.79/(12.79+127.73) = .09
# 9% between, 91% within

# Saving predicted values and residuals for plots
wiscraw_long$verbpred_m1 <- predict(m1)
wiscraw_long$verbres_m1 <- wiscraw_long$verb - wiscraw_long$verbpred_m1

# plot predicted values
p <- ggplot(data = wiscraw_long, aes(x = time, y = verbpred_m1, group = id))
p + geom_point(color="blue") + geom_line(color="blue") +
    labs(title = "Predicted Values from Unconditional Model",
         y = "Verbal Score") +
    theme(plot.title = element_text(hjust = 0.5))

# plot residuals
p <- ggplot(data = wiscraw_long, aes(x = time, y = verbres_m1, group = id))
p + geom_point(color="grey") + geom_line(color="grey") +
    labs(title = "Residuals from Unconditional Model",
         y = "Verbal Score") +
    theme(plot.title = element_text(hjust = 0.5))

############################################################################################################
# Linear Model of Change
############################################################################################################
# random intercept and slope
m2 <- lmer(verb ~ 1 + time + (1+time|id), REML=FALSE, data=wiscraw_long)
summary(m2)

# There is no direct test for the random effects components 
# However, we could compare nested models using the likelihood ratio test
# Nested model 1: random intercept only
m2_ns <- lmer(verb ~ 1 + time + (1|id), REML=FALSE, data=wiscraw_long)
summary(m2_ns)
anova(m2,m2_ns)
# Removing the random slopes significantly worsened the fit.

# Nested model 2: uncorrelated random intercept and slope 
# The symbol "||" means no correlation between the random effects
m2_nc <- lmer(verb ~ 1+time+ (1+time||id), REML=FALSE,data=wiscraw_long)
summary(m2_nc)

anova(m2,m2_nc)
# The difference is significant. So the correlation is significantly different from zero.

# Plots for m2
wiscraw_long$verbpred_m2 <- predict(m2)
wiscraw_long$verbres_m2 <- wiscraw_long$verb - wiscraw_long$verbpred_m2


# plot predicted values
p <- ggplot(data = wiscraw_long, aes(x = time, y = verbpred_m2, group = id))
p + geom_point(color="blue") + geom_line(color="blue") +
    labs(title = "Predicted Values from Linear Model",
         y = "Verbal Score") +
    theme(plot.title = element_text(hjust = 0.5))

# plot residuals
p <- ggplot(data = wiscraw_long, aes(x = time, y = verbres_m2, group = id))
p + geom_point(color="grey") + geom_line(color="grey") +
    labs(title = "Residuals from Linear Model",
         y = "Verbal Score") +
    theme(plot.title = element_text(hjust = 0.5))

############################################################################################################
# Linear + Quadratic Model of Change
############################################################################################################
# create the quadratic term
wiscraw_long$timesq <- wiscraw_long$time^2

m3 <- lmer(verb ~ 1 + time + timesq + (1+time+timesq|id), REML=FALSE, data=wiscraw_long)
# Did not converge

# Without random effect on quadratic
m3_nq <- lmer(verb ~ 1 + time + timesq + (1+time|id), REML=FALSE, data=wiscraw_long)
summary(m3_nq)


# Plots for m3
wiscraw_long$verbpred_m3 <- predict(m3_nq)
wiscraw_long$verbres_m3 <- wiscraw_long$verb - wiscraw_long$verbpred_m3

# plot predicted values
p <- ggplot(data = wiscraw_long, aes(x = time, y = verbpred_m3, group = id))
p + geom_point(color="blue") + geom_line(color="blue") +
    labs(title = "Predicted Values from Quadratic Model",
         y = "Verbal Score") +
    theme(plot.title = element_text(hjust = 0.5))

# plot residuals
p <- ggplot(data = wiscraw_long, aes(x = time, y = verbres_m3, group = id))
p + geom_point(color="grey") + geom_line(color="grey") +
    labs(title = "Residuals from Quadratic Model",
         y = "Verbal Score") +
    theme(plot.title = element_text(hjust = 0.5))


############################################################################################################
# Linear + Quadratic + Cubic Model of Change
############################################################################################################
# create the cubic term
wiscraw_long$timecu <- wiscraw_long$time^3

# Without random effect on quadratic and cubic
m4 <- lmer(verb ~ 1 + time + timesq + timecu + (1+time|id), REML=FALSE, data=wiscraw_long)
summary(m4)

# Plots for m4
wiscraw_long$verbpred_m4 <- predict(m4)
wiscraw_long$verbres_m4 <- wiscraw_long$verb - wiscraw_long$verbpred_m4

# plot predicted values
p <- ggplot(data = wiscraw_long, aes(x = time, y = verbpred_m4, group = id))
p + geom_point(color="blue") + geom_line(color="blue") +
    labs(title = "Predicted Values from Cubic Model",
         y = "Verbal Score") +
    theme(plot.title = element_text(hjust = 0.5))

# plot residuals
p <- ggplot(data = wiscraw_long, aes(x = time, y = verbres_m4, group = id))
p + geom_point(color="grey") + geom_line(color="grey") +
    labs(title = "Residuals from Cubic Model",
         y = "Verbal Score") +
    theme(plot.title = element_text(hjust = 0.5))

############################################################################################################
# Inclusion of Interindividual Differences Predictor, Categorical
############################################################################################################
# making grad a categorical variable
wiscraw_long$grad <- factor(wiscraw_long$grad)

mp1 <- lmer(verb ~ 1 + time*grad + (1+time|id), REML=FALSE, data=wiscraw_long)
summary(mp1)


# Plots for mp1
wiscraw_long$verbpred_mp1 <- predict(mp1)

# plot predicted values
p <- ggplot(data = wiscraw_long, aes(x = time, y = verbpred_mp1, group = id, color=grad))
p + geom_point() + geom_line(linetype="dashed") +
  scale_color_manual(values=c("0"="#0072B2","1"="#E69F00"),
  labels = c("0" = "NonGrad", "1" = "Grad")) +
  geom_abline(intercept=fixef(mp1)[1],slope=fixef(mp1)[2],color="#0072B2",linetype="solid",size=2)+ # Average trajectory for grad=0
  geom_abline(intercept=(fixef(mp1)[1]+fixef(mp1)[3]),slope=(fixef(mp1)[2]+fixef(mp1)[4]),color="#E69F00",linetype="solid",size=2)+ # Average trajectory for grad=1
  labs(title = "Predicted Values with Grad Variable",
       y = "Verbal Score") +
  theme(plot.title = element_text(hjust = 0.5))

############################################################################################################
# Inclusion of Interindividual Differences Predictor, Continuous
############################################################################################################
describe(wiscraw$momed)

wiscraw_long$momedc <- wiscraw_long$momed - mean(wiscraw$momed)

mp2 <- lmer(verb ~ 1 + time*momedc + (1+time|id), REML=FALSE, data=wiscraw_long)
summary(mp2)



# Plots for mp1
wiscraw_long$verbpred_mp2 <- predict(mp2)
msd <- sd(wiscraw$momed) # one standard deviation of momed

# plot predicted values
p <- ggplot(data = wiscraw_long, aes(x = time, y = verbpred_mp2, group = id, color=momedc))
p + geom_point() + geom_line(linetype="dashed") +
  scale_color_gradient(low = "#0072B2", high = "#E69F00") +  # Blue to orange
  geom_abline(intercept=fixef(mp2)[1],slope=fixef(mp2)[2],color="black",linetype="solid",size=2)+ # Predicted trajectory for momedc=0
  geom_abline(intercept=(fixef(mp2)[1]+fixef(mp2)[3]*msd),slope=(fixef(mp1)[2]+fixef(mp1)[4]*msd),color="#E69F00",linetype="solid",size=2)+ # Predicted trajectory for 1sd above mean in momedc
  geom_abline(intercept=(fixef(mp2)[1]-fixef(mp2)[3]*msd),slope=(fixef(mp1)[2]-fixef(mp1)[4]*msd),color="#0072B2",linetype="solid",size=2)+ # Predicted trajectory for 1sd below mean in momedc
  labs(title = "Predicted Values with Momed Variable",
       y = "Verbal Score") +
  theme(plot.title = element_text(hjust = 0.5))
