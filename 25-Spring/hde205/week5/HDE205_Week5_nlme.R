############################################################################################################
# Week 5: Growth Modeling with nlme
############################################################################################################*This script provides 3 ways to conduct ANOVA analysis on repeated measures,

# Inputing data
#wiscraw <- read.csv('C:/Users/siwei-admin/Dropbox/HDE 205/WISC data/wisc3raw.csv')
wiscraw <- read.csv('C:/Users/sweliu/Dropbox/HDE 205/WISC data/wisc3raw.csv')

# Transform from wide to long
list1 <- 2:5 # indicators for verb
list2<- 6:9 # indicators for perfo
wiscraw_long <- reshape(wiscraw,idvar="id",v.names=c("verb","perfo"),varying=list(list1,list2),times=c(1,2,4,6), 
                        direction="long")
wiscraw_long <- wiscraw_long[order(wiscraw_long$id),] # sort data by id

############################################################################################################
# Multilevel Model - Unconditional Means Model
############################################################################################################
# Used for calculating ICC (relative amount of between and within variance)
# Also can be thought of as a no-growth model
library(nlme)
m1 <- lme(verb ~ 1, random = ~ 1|id, method="ML", data=wiscraw_long)
summary(m1)

# Estimated variance components
VarCorr(m1)

# ICC= 12.79/(12.79+127.73) = .09
# 9% between, 91% within

# Saving predicted values and residuals for plots
wiscraw_long$verbpred_m1 <- predict(m1)
wiscraw_long$verbres_m1 <- wiscraw_long$verb - wiscraw_long$verbpred_m1

# plot predicted values
library(ggplot2)
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
m2 <- lme(verb ~ time, random = ~ time|id, method="ML", data=wiscraw_long)
summary(m2)
VarCorr(m2)

# Testing for variance components using likelihood ratio test
# Nested model 1: random intercept only
m2_ns <- lme(verb ~ time, random = ~ 1|id, method="ML", data=wiscraw_long)
summary(m2_ns)
anova(m2,m2_ns)

# Unfortunately nlme does not allow uncorrelated random effects

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

m3 <- lme(verb ~ time+timesq, random = ~ time+timesq|id, method="ML", data=wiscraw_long)
# Did not converge

# Without random effect on quadratic
m3_nq <- lme(verb ~ time+timesq, random = ~ time|id, method="ML", data=wiscraw_long)
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

m4 <- lme(verb ~ time+timesq+timecu, random = ~ time|id, method="ML", data=wiscraw_long)
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

mp1 <- lme(verb ~ time*grad, random = ~ time|id, method="ML", data=wiscraw_long)
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

mp2 <- lme(verb ~ time*momedc, random = ~ time|id, method="ML", data=wiscraw_long)
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
