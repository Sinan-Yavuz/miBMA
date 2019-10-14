
##miBMA package example

rm(list = ls())

library(miBMA)
library(mice)
library(BMA)

sample.size = 100 
per = .40 # percentage of missingnes
m = 20 # number of imputations
maxit = 10 # number of iterations

r = 0.2 # correlation btw variables
R = matrix(r, nrow=10, ncol=10) + diag(10)*(1-r)
U = t(chol(R))
nvars = dim(U)[1]
numobs = sample.size

set.seed(5959)
random.normal = matrix(rnorm(nvars*numobs,0,1), nrow=nvars, ncol=numobs);
X = U %*% random.normal
data <- as.matrix(t(X))
colnames(data) = c("Var1","Var2","Var3","Var4","Var5","Var6","Var7","MAR1","MAR2","MAR3")

initial.data <- data # initial data has no missingness, this data will be compared to imputed results

data[,c("MAR1")][order(data[,c("Var1")], decreasing = T)[1:(sample.size*per)]] <- NA
data[,c("MAR2")][order(data[,c("Var2")], decreasing = T)[1:(sample.size*per)]] <- NA
data[,c("MAR3")][order(data[,c("Var3")], decreasing = T)[1:(sample.size*per)]] <- NA

mibma.imp <- mice(data, method = "miBMA", m = m, maxit = maxit)

md.pattern(complete(mibma.imp, 1))
