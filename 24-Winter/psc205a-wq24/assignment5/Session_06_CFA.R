# PSC 205A 
# Confirmatory Factor Analysis
# https://stats.idre.ucla.edu/r/seminars/rcfa/
# http://lavaan.ugent.be/tutorial/cfa.html


# Getting the data (emotion)
emotion <- read.csv("assignment5/emotion_short.csv") 
summary(emotion)
emotion

data1 = subset(emotion, select = c("id","female","interested","excited","inspired","enthusiastic","irritable","distressed","upset","hostile"))
summary(data1)


# Splitting the data into two groups
male = subset(data1, female == "0")
summary(male)
male
round(cor(male[,3:10]),3)

female = subset(data1, female == "1")
summary(female)

plot(female[,3:10])
plot(female)
cov(female[,3:10])
cor(female[,3:10])

library(lavaan)
library(lavaanPlot)

#Males
#Model 1: One Factor
male.cfa1 = "#factor loadings
	ly1 =~ interested + excited + inspired + enthusiastic +
	       irritable + distressed + upset + hostile
	
	# means
	ly1 ~ 0
	
	# variances
	interested ~~ interested
	excited ~~ excited
	inspired ~~ inspired
	enthusiastic ~~ enthusiastic
	irritable ~~ irritable
	distressed ~~ distressed
	upset ~~ upset
	hostile ~~ hostile
	
	# latent variable variances
	ly1 ~~ start(1.1)*ly1
	"
fit.male.cfa1 = cfa(male.cfa1,data=male,fixed.x=FALSE,missing="fiml")
summary(fit.male.cfa1,fit.measures=T,standardized=TRUE)

#Model 2: Two Factors
male.cfa2 = "#factor loadings
	ly1 =~ interested + excited + inspired + enthusiastic
	ly2 =~ irritable + distressed + upset + hostile
	
	# means
	ly1 ~ 0
	ly2 ~ 0
	
	# variances
	interested ~~ interested
	excited ~~ excited
	inspired ~~ inspired
	enthusiastic ~~ enthusiastic
	irritable ~~ irritable
	distressed ~~ distressed
	upset ~~ upset
	hostile ~~ hostile
	
	# latent variable variances
	ly1 ~~ start(1.1)*ly1
	ly2 ~~ start(1.1)*ly2
  
  # covariances
	ly1 ~~ ly2
	"
fit.male.cfa2 = cfa(male.cfa2,data=male,fixed.x=FALSE,missing="fiml")
summary(fit.male.cfa2,fit.measures=T)

# Multiple-group CFA
mg.cfa = "
#factor loadings
ly1 =~ interested + excited + inspired + enthusiastic
ly2 =~ irritable + distressed + upset + hostile

# means
ly1 ~ 0
ly2 ~ 0

# variances
interested ~~ interested
excited ~~ excited
inspired ~~ inspired
enthusiastic ~~ enthusiastic
irritable ~~ irritable
distressed ~~ distressed
upset ~~ upset
hostile ~~ hostile

# latent variable variances
ly1 ~~ start(1.1)*ly1
ly2 ~~ start(1.1)*ly2

# covariances
ly1 ~~ ly2
"
fit.invariant = cfa(mg.cfa,data=data1,fixed.x=FALSE,group="female",
                    group.equal=c("loadings","means","intercepts","residuals","lv.variances","lv.covariances"))
fit.means = cfa(mg.cfa,data=data1,fixed.x=FALSE,group="female",
                group.equal=c("loadings","residuals","lv.variances","lv.covariances"))
fit.varcov = cfa(mg.cfa,data=data1,fixed.x=FALSE,group="female",
                 group.equal=c("loadings","residuals"))
fit.loadings = cfa(mg.cfa,data=data1,fixed.x=FALSE,group="female",
                 group.equal=c("residuals"))
fit.all = cfa(mg.cfa,data=data1,fixed.x=FALSE,group="female",
                   group.equal=c(""))
summary(fit.invariant,fit.measures=T)
summary(fit.means,fit.measures=T)
summary(fit.varcov,fit.measures=T)
summary(fit.loadings,fit.measures=T)
summary(fit.all,fit.measures=T,standardized=TRUE)
