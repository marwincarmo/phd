# PSC 205A 
# Structural Equation Modeling

# Getting the data (mhs)
mhs <- read.csv("C:/Teaching/PSC 205A/Session_07_SEM/mhs_t1.csv", na.string='.') 
summary(mhs)
mhs


# Getting the data (Alternative)
mhs <- read.table("C:/Teaching/PSC 205A/Session_07_SEM/mhs_t1.dat", na.string='.') 
colnames(mhs) <- c("id", "age", "male", "female",
                   "pcomp_1", "appear_1", "motiv_1", "pworth_1", "gworth_1",
                   "parent_1", "teach_1", "mate_1", "friend_1", "affect_1", "auton_1",
                   "pc1", "pc2", "pc3", "pc4", "mot1", "mot2", "mot3", "mot4", "mot5",
                   "psw1", "psw2", "psw3", "psw4",
                   "ts1", "ts2", "ts3", "ts4", "ts5", "enj1", "enj2", "enj3")
summary(mhs)
plot(mhs[,5:7])
cor(mhs[,5:7])

library(lavaan)
library(lavaanPlot)

#Model 1: Measurement Model
latent_01 = "#factor loadings
	pcomp =~ pc1  + pc2  + pc3  + pc4
	worth =~ psw1 + psw2 + psw3 + psw4
	teach =~ ts1  + ts2  + ts3  + ts4 + ts5
	motiv =~ mot1 + mot2 + mot3 + mot4 + mot5
	
	# means
	pcomp ~ 0
	worth ~ 0
	teach ~ 0
	motiv ~ 0
	
	# variances
	pc1 ~~ pc1
	pc2 ~~ pc2
	pc3 ~~ pc3
	pc4 ~~ pc4
	
	psw1 ~~ psw1
	psw2 ~~ psw2
	psw3 ~~ psw3
	psw4 ~~ psw4

	ts1 ~~ ts1
	ts2 ~~ ts2
	ts3 ~~ ts3
	ts4 ~~ ts4
	ts5 ~~ ts5
	
	mot1 ~~ mot1
	mot2 ~~ mot2
	mot3 ~~ mot3
	mot4 ~~ mot4
	mot5 ~~ mot5
	
	# latent variable variances
	pcomp ~~ start(1.1)*pcomp
	worth ~~ worth
	teach ~~ teach
	motiv ~~ motiv

  # covariances
	pcomp ~~ worth
	pcomp ~~ teach
	pcomp ~~ motiv
	worth ~~ teach
	worth ~~ motiv
	teach ~~ motiv
"
latent_01 = cfa(latent_01,data=mhs,fixed.x=FALSE,missing="fiml")
summary(latent_01,fit.measures=T,standardized=TRUE)
lavaanPlot(model = latent_01, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = T)

#Model 2: Measurement and Structural
latent_02 = "#factor loadings
	pcomp =~ pc1  + pc2  + pc3  + pc4
	worth =~ psw1 + psw2 + psw3 + psw4
	teach =~ ts1  + ts2  + ts3  + ts4 + ts5
	motiv =~ mot1 + mot2 + mot3 + mot4 + mot5
	
	# means
	pcomp ~ 0
	worth ~ 0
	teach ~ 0
	motiv ~ 0
	
	# variances
	pc1 ~~ pc1
	pc2 ~~ pc2
	pc3 ~~ pc3
	pc4 ~~ pc4
	
	psw1 ~~ psw1
	psw2 ~~ psw2
	psw3 ~~ psw3
	psw4 ~~ psw4

	ts1 ~~ ts1
	ts2 ~~ ts2
	ts3 ~~ ts3
	ts4 ~~ ts4
	ts5 ~~ ts5
	
	mot1 ~~ mot1
	mot2 ~~ mot2
	mot3 ~~ mot3
	mot4 ~~ mot4
	mot5 ~~ mot5
	
	# latent variances
	pcomp ~~ start(1.1)*pcomp
	worth ~~ worth
	teach ~~ teach
	motiv ~~ motiv

	# structural relations
	worth ~ pcomp + teach
	motiv ~ worth
	
	# covariances
	pcomp ~~ 0*worth
	worth ~~ 0*teach
	worth ~~ 0*motiv
	pcomp ~~ teach
	pcomp ~~ 0*motiv
	teach ~~ 0*motiv
	
"
latent_02 = cfa(latent_02,data=mhs,fixed.x=FALSE,missing="fiml")
summary(latent_02,fit.measures=T,standardized=TRUE)
lavaanPlot(model = latent_02, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = T)

#Model 3: Alternative
latent_03 = "#factor loadings
	pcomp =~ pc1  + pc2  + pc3  + pc4
	worth =~ psw1 + psw2 + psw3 + psw4
	teach =~ ts1  + ts2  + ts3  + ts4 + ts5
	motiv =~ mot1 + mot2 + mot3 + mot4 + mot5
	
	# means
	pcomp ~ 0
	worth ~ 0
	teach ~ 0
	motiv ~ 0
	
	# variances
	pc1 ~~ pc1
	pc2 ~~ pc2
	pc3 ~~ pc3
	pc4 ~~ pc4
	
	psw1 ~~ psw1
	psw2 ~~ psw2
	psw3 ~~ psw3
	psw4 ~~ psw4

	ts1 ~~ ts1
	ts2 ~~ ts2
	ts3 ~~ ts3
	ts4 ~~ ts4
	ts5 ~~ ts5
	
	mot1 ~~ mot1
	mot2 ~~ mot2
	mot3 ~~ mot3
	mot4 ~~ mot4
	mot5 ~~ mot5
	
	# latent variances
	pcomp ~~ start(1.1)*pcomp
	worth ~~ worth
	teach ~~ teach
	motiv ~~ motiv

	# structural relations
	worth ~ pcomp + teach
	motiv ~ worth + pcomp + teach
	
	# covariances
	pcomp ~~ 0*worth
	worth ~~ 0*teach
	worth ~~ 0*motiv
	pcomp ~~ teach
	pcomp ~~ 0*motiv
	teach ~~ 0*motiv
	
"
latent_03 = cfa(latent_03,data=mhs,fixed.x=FALSE,missing="fiml")
summary(latent_03,fit.measures=T,standardized=TRUE)


# Multiple-group SEM
mg.sem = "
#factor loadings
pcomp =~ pc1  + pc2  + pc3  + pc4
worth =~ psw1 + psw2 + psw3 + psw4
teach =~ ts1  + ts2  + ts3  + ts4 + ts5
motiv =~ mot1 + mot2 + mot3 + mot4 + mot5

# means
pcomp ~ 1
worth ~ 1
teach ~ 1
motiv ~ 1

# variances
pc1 ~~ pc1
pc2 ~~ pc2
pc3 ~~ pc3
pc4 ~~ pc4

psw1 ~~ psw1
psw2 ~~ psw2
psw3 ~~ psw3
psw4 ~~ psw4

ts1 ~~ ts1
ts2 ~~ ts2
ts3 ~~ ts3
ts4 ~~ ts4
ts5 ~~ ts5

mot1 ~~ mot1
mot2 ~~ mot2
mot3 ~~ mot3
mot4 ~~ mot4
mot5 ~~ mot5

# latent variances
pcomp ~~ start(1.1)*pcomp
worth ~~ worth
teach ~~ teach
motiv ~~ motiv

# structural relations
worth ~ pcomp + teach
motiv ~ worth

# covariances
pcomp ~~ 0*worth
worth ~~ 0*teach
worth ~~ 0*motiv
pcomp ~~ teach
pcomp ~~ 0*motiv
teach ~~ 0*motiv
"
fit.configural = sem(mg.sem,data=mhs,fixed.x=FALSE,group="female", group.equal=c(""))
fit.loadings   = sem(mg.sem,data=mhs,fixed.x=FALSE,group="female", group.equal=c("loadings"))
fit.intercepts = sem(mg.sem,data=mhs,fixed.x=FALSE,group="female", group.equal=c("loadings","intercepts"))
fit.varcov     = sem(mg.sem,data=mhs,fixed.x=FALSE,group="female", group.equal=c("loadings","intercepts", "lv.variances","lv.covariances"))
fit.regress    = sem(mg.sem,data=mhs,fixed.x=FALSE,group="female", group.equal=c("loadings","intercepts", "lv.variances","lv.covariances","regressions"))
fit.invariant  = sem(mg.sem,data=mhs,fixed.x=FALSE,group="female",
                     group.equal=c("loadings","intercepts", "lv.variances","lv.covariances","regressions","residuals"))

summary(fit.configural,fit.measures=T,standardized=TRUE)
summary(fit.loadings,fit.measures=T)
summary(fit.intercepts,fit.measures=T)
summary(fit.varcov,fit.measures=T)
summary(fit.regress,fit.measures=T)
summary(fit.invariant,fit.measures=T)

anova(fit.configural, fit.loadings, fit.intercepts, fit.varcov, fit.regress, fit.invariant)
