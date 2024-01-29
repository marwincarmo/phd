# PSC 205A 
# Canonical Correlation

# Required packages
install.packages("CCA")
install.packages("candisc")

# Getting the data (Tabachnick & Fidell)
mm <- read.table("assignment2/cca_tf.dat") 
colnames(mm) <- c("x1", "x2", "y1", "y2")
summary(mm)

X <- mm[, 1:2] # x <- mm[, 1:2]
Y <- mm[, 3:4] # y <- mm[, 3:4]

# Describing the data
xtabs(~y2, data = mm)
ggpairs(X)
ggpairs(Y)

# Correlations
matcor(X, Y)

# Canonical correlation
cc1 <- cc(X, Y)

# Raw canonical coefficients
cc1[1:4]

# Compute canonical loadings
cc2 <- comput(X, Y, cc1)

# Display canonical loadings
cc2[3:6]

# Tests of canonical dimensions
ev <- (1 - cc1$cor^2) 

n <- dim(X)[1]
p <- length(X)
q <- length(Y)
k <- min(p, q)
m <- n - 3/2 - (p + q)/2
w <- rev(cumprod(rev(ev))) #Likelihood ratio of CCs (Eigenvalues)

# Initialize
d1 <- d2 <- f <- vector("numeric", k)

for (i in 1:k) {
  s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
  si <- 1/s
  d1[i] <- p * q
  d2[i] <- m * s - p * q/2 + 1
  r <- (1 - w[i]^si)/w[i]^si
  f[i] <- r * d2[i]/d1[i]
  p <- p - 1
  q <- q - 1
}

pv <- pf(f, d1, d2, lower.tail = FALSE)
(dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))

# Standardized canonical coefficients (X) diagonal matrix of X sd's
s1 <- diag(sqrt(diag(cov(X))))
s1 %*% cc1$xcoef

# Standardized canonical coefficients(Y)  diagonal matrix of Y sd's
s2 <- diag(sqrt(diag(cov(Y))))
s2 %*% cc1$ycoef

# Creating linear combinations
# As matrices
A <- data.matrix(mm)
Ax1 <- A[, 1]
Ax2 <- A[, 2]
Ay1 <- A[, 3]
Ay2 <- A[, 4]

rawx <- cc1[3]
rawy <- cc1[4]

lcx1 <- (Ax1 * (-.229790)) + (Ax2 * (.248813))
lcx2 <- (Ax1 *  (.292956)) + (Ax2 * (.270373))
lcy1 <- (Ay1 * (-.169640)) + (Ay2 * (.372108))
lcy2 <- (Ay1 *  (.308762)) + (Ay2 * (.180401))

# Checking correlations between linear combinations
cor (lcx1, lcx2) # this should be 0
cor (lcy1, lcy2) # this should be 0

cor (lcx1, lcy1) # this should be the 1st cc 
cor (lcx2, lcy2) # this should be the 2nd cc


