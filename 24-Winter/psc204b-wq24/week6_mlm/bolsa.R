## BOLSA
library(nlme)
library(multilevel)
library(foreign)
bolsa <- read.spss("week6_mlm/bolsa.sav",
  use.value.labels=F, to.data.frame=T)
names(bolsa)
#UNIV

#install.packages()

head(bolsa)
sum(bolsa$NOBS >= 5)


bolsa.univ <- make.univ(bolsa, bolsa[,c(5:10)], tname="trial", outname="ds")[,c(1, 2, 3, 4, 19, 20)]
b2 = make.univ(bolsa, bolsa[,c(11:16)], tname="trial", outname="bd")
head(b2 )
bolsa.univ$bd = b2$bd


bolsa.univ$time=ifelse(bolsa.univ$trial==5, 15, 
       ifelse(bolsa.univ$trial==4, 11, 
              ifelse(bolsa.univ$trial==3, 7, 
                     ifelse(bolsa.univ$trial==2, 4,
                            ifelse(bolsa.univ$trial==1,2, 0)))))

head(bolsa.univ, n=20)

sum(bolsa$KOHORTE)
bolsa.univ$age_t0 <- ifelse(bolsa.univ$KOHORTE == 1, 72.4, 63.3)

library(ggplot2)
# Creating the ggplot
bolsa.univ$VPN.f=as.factor(bolsa.univ$VPNR)

ggplot(bolsa.univ, aes(x = time, y = ds, group = VPN.f, color = VPN.f)) +
  geom_line() +  # Line plot with specified line width
  geom_point(size = 0.5, shape = 19) +  # Adding points with specified size and shape
  scale_x_continuous(name = "Time (in years since study entry)", breaks = waiver()) +  # X-axis label and breaks
  scale_y_continuous(name = "DS", breaks = waiver())+
  theme( legend.position = "none")

ggsave(filename = "~/Dropbox/Lehre/PSC204B/2024/Class/Week_6-Multilevel Model/Day_1/figures/bolsa1.pdf", width = 5, height = 4)

ggplot(bolsa.univ, aes(x = age_t0+time, y = ds, group = VPN.f, color = VPN.f)) +
  geom_line() +  # Line plot with specified line width
  geom_point(size = 0.5, shape = 19) +  # Adding points with specified size and shape
  scale_x_continuous(name = "Age", breaks = waiver()) +  # X-axis label and breaks
  scale_y_continuous(name = "DS", breaks = waiver())+
  theme( legend.position = "none")

ggsave(filename = "~/Dropbox/Lehre/PSC204B/2024/Class/Week_6-Multilevel Model/Day_1/figures/bolsa1_age.pdf", width = 5, height = 4)


ggplot(bolsa.univ, aes(x = time, y = bd, group = VPN.f, color = VPN.f)) +
  geom_line() +  # Line plot with specified line width
  geom_point(size = 0.5, shape = 19) +  # Adding points with specified size and shape
  scale_x_continuous(name = "Time (in years since study entry)", breaks = waiver()) +  # X-axis label and breaks
  scale_y_continuous(name = "DS", breaks = waiver())+
  theme( legend.position = "none")

ggsave(filename = "~/Dropbox/Lehre/PSC204B/2024/Class/Week_6-Multilevel Model/Day_1/figures/bolsa_bd1.pdf", width = 5, height = 4)

ggplot(bolsa.univ, aes(x = age_t0+time, y = bd, group = VPN.f, color = VPN.f)) +
  geom_line() +  # Line plot with specified line width
  geom_point(size = 0.5, shape = 19) +  # Adding points with specified size and shape
  scale_x_continuous(name = "Age", breaks = waiver()) +  # X-axis label and breaks
  scale_y_continuous(name = "DS", breaks = waiver())+
  theme( legend.position = "none")

ggsave(filename = "~/Dropbox/Lehre/PSC204B/2024/Class/Week_6-Multilevel Model/Day_1/figures/bolsa_bd1_age.pdf", width = 5, height = 4)


## Use only complete cases
library(data.table )
comp <- bolsa.univ[bolsa.univ$NOBS >= 5, ]
## Convert to data.table
setDT(comp)
# Keep only the first 5 rows for each VPNR
comp_filtered <- comp[, .SD[1:5], by = VPNR]

length(unique(comp_filtered$VPNR ))

ggplot(comp_filtered, aes(x = time, y = ds, group = VPN.f, color = VPN.f)) +
  geom_line() +  # Line plot with specified line width
  geom_point(size = 0.5, shape = 19) +  # Adding points with specified size and shape
  scale_x_continuous(name = "Time (in years since study entry)", breaks = waiver()) +  # X-axis label and breaks
  scale_y_continuous(name = "DS", breaks = waiver())+
  geom_smooth(method = "lm",  se = FALSE, aes(group = 1), lwd = 2) +
  theme( legend.position = "none")

ggsave(filename = "~/Dropbox/Lehre/PSC204B/2024/Class/Week_6-Multilevel Model/Day_1/figures/bolsa_fixed.pdf", width = 5, height = 4)

library(lme4)
72.4-63.3
fit0 <- lmer(ds ~  time + (1 + time | VPNR),  data = comp_filtered )
summary(fit0 )

fitlm <- lm(ds ~ + time ,  data = comp_filtered )
summary(fitlm )
var(residuals(fitlm))


comp_filtered$fitted0 <- predict(fit0 )

ggplot(comp_filtered, aes(x = time, y = fitted0, group = VPN.f, color = VPN.f)) +
  geom_line() +  # Line plot with specified line width
  geom_point(size = 0.5, shape = 19) +  # Adding points with specified size and shape
  scale_x_continuous(name = "Time (in years since study entry)", breaks = waiver()) +  # X-axis label and breaks
  scale_y_continuous(name = "Predicted DS scores", breaks = waiver())+
  theme( legend.position = "none")

ggsave(filename = "~/Dropbox/Lehre/PSC204B/2024/Class/Week_6-Multilevel Model/Day_1/figures/bolsafit.pdf", width = 5, height = 4)


## Add Cohort
fitC <- lmer(ds ~ KOHORTE * time + (1 + time | VPNR),  data = comp_filtered )
summary(fitC )

AIC(fitC,  fit0 )
anova( fitC,  fit0 )

comp_filtered$fittedC <- predict(fitC )

ggplot(comp_filtered, aes(x = time, y = fittedC, group = VPN.f, color = VPN.f)) +
  geom_line() +  # Line plot with specified line width
  geom_point(size = 0.5, shape = 19) +  # Adding points with specified size and shape
  scale_x_continuous(name = "Time (in years since study entry)", breaks = waiver()) +  # X-axis label and breaks
  scale_y_continuous(name = "Predicted DS scores", breaks = waiver())+ facet_grid(.~KOHORTE )
  theme( legend.position = "none")






?lag.plot
lag.plot(subset(bolsa.univ, ds>=0, ds), lags=3, diag=T) 
acf(subset(bolsa.univ, ds>=0, ds), lags=3, diag=T)
?diff
library(nlme)

names(bolsa.univ)
mod0 <- lme(fixed =ds~1, data=bolsa.univ,
            random=~1|VPNR, na.action=na.omit)
   VarCorr(mod0)
   as.numeric(VarCorr(mod0)[1,1])/(as.numeric(VarCorr(mod0)[1,2])+as.numeric(VarCorr(mod0)[1,1])) #ICC
mod0.t <- update(mod0, fixed=ds~trial)
   as.numeric(VarCorr(mod0.t)[1,1])/(as.numeric(VarCorr(mod0.t)[1,2])+as.numeric(VarCorr(mod0.t)[1,1]))
   # the inclusion of trial does not explain further variance cf. ICC of mod0 with modo.t
mod0.t2 <- update(mod0.t, random=~1+trial|VPNR)
   VarCorr(mod0.t2)
   summary(mod0.t2)

# PLOT FITTED MODEL
fitted <- data.frame(ds.fit=cbind(mod0.t2$fitted[,2],
                       trial=subset(bolsa.univ, ds>=0, trial)[,1], subject=subset(bolsa.univ, ds>=0, VPNR )[,1]) )
xyplot(ds.fit.V1~ds.fit.trial, group=ds.fit.subject, data=fitted, type="l")
#-----------------------------------------

modo.t3 <- update(mod0.t2, correlation=corSymm())

# separate intercept for each school
mod.school <- lme(fixed=ds~trial + factor(SCHULE), data=bolsa.univ, na.action=na.omit,
                  random=~1+trial|VPNR)
   summary(mod.school)
   VarCorr(mod.school)

mod2 <- update(mod.school, weights=varFixed(~I(trial+1)), correlation=corAR1())
  summary(mod2)
  VarCorr(mod2) #random effect variance
  plot(mod2)
  mod2$sigma
  mod2$fitted
acf(resid(mod2)) #Maindonald Braun p. 249 ff
qqnorm(resid(mod2, type="normalized"))
beta <- summary(mod2$modelStruct)$corStruct
plot(ARMAacf(ar=beta, lag.max=5), type="h")


# Mit neuem Paket - weights und correlations can't yet be specified
library(lme4)
lmer.0 <- lmer(formula=ds~trial+(trial|VPNR), data=bolsa.univ, na.action=na.omit)
summary(lmer.0)
VarCorr(lmer.0)

lmer.1 <- update(lmer.0, formula=ds~trial+factor(SCHULE)+(trial|VPNR))
summary(lmer.1)
VarCorr(lmer.1)

lmer.2 <- update(lmer.1)
summary(lmer.2)
