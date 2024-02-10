# PSC 205A 
# PCA and PFA 

# Getting the data (emotion)
emotion <- read.csv("C:/Teaching/PSC 205A/Session_05_PCA/emotion.csv") 
summary(emotion)
data1 = subset(emotion, select = c("interested","excited","inspired","enthusiastic","irritable","distressed","upset","hostile"))
summary(data1)
plot(data1[,1:8])

#PCA
data1.pca <- prcomp(data1[,c(1:8)], center = TRUE,scale = TRUE)
summary(data1.pca,loadings = TRUE)
fviz_eig(data1.pca)
str(data1.pca)


emotion.pca <-princomp(x = data1)
emotion.pca
summary(emotion.pca, loadings = TRUE)

#Correlation and Covariance
cor(data1)
cov(data1)

X <-data1[,1:8]
Y <-data1[,1:8]
cor.data1 <-cor(X,Y)
cov.data1 <-cov(X,Y)
cor.data1
cov.data1

#PCA - Covariance matrix
cov.pca <-princomp(covmat = cov.data1)
summary(cov.pca, loadings = TRUE)

#PCA - Correlations matrix
cor.pca <-princomp(covmat = cor.data1)
summary(cor.pca, loadings = TRUE)


#Calculating PCA scores
plot(cor.pca$sdev^2, xlab = "Component number", ylab = "Component variance", 
     type = "l", main = "Scree diagram")

plot(log(cor.pca$sdev^2), xlab = "Component number", ylab = "log(Component variance)", 
     type = "l", main = "Log(eigenvalue) diagram")





library(factoextra)
eig.val <- get_eigenvalue(data1.pca)
eig.val


#Plotting the PCA
install.packages("factoextra")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(data1.pca)
ggbiplot(data1.pca, labels=rownames(data1))

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")


#PFA
sapply(1:5, function(f)
  factanal(data1, factors = f, method ="mle")$PVAL)

factanal(x = data1, factors = 2, method ="mle")

(scores <- factanal(data1, factors = 2, method = "mle",
           scores = "regression")$scores)
cor(scores)


#PFA using covariance and correlation matrices
factanal(covmat = cov.data1, factors = 2, method = "mle", n.obs = 364)

factanal(covmat = cor.data1, factors = 2, method = "mle", n.obs = 364)



