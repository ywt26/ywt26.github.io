'''
setwd("G:/ThirdyearⅡ/微观计量经济学/Assignment6")
data <- read.csv("Ketchup.csv")
View(data)
prop.table(table(data$choice))

library(mlogit)
data.long <- mlogit.data(data,shape = "wide", varying = 2:5, choice = "choice")
'''

data("Fishing", package = "mlogit")
View(Fishing)
prop.table(table(Fishing$mode))

library(mlogit)
library(AER)

#reshape data
Fish.long <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
head(Fish.long,5)

# multinomial probit with one var normalized to zero 
pro_fit <- mlogit(mode~ price|income,Fish.long,reflevel = "beach",probit = TRUE)
coeftest(pro_fit)

# covariance matrix using "beach" as referenc
pro_fit$omega$beach

library(nnet)
set.seed(5)
n <-length(Fishing[,1])
samp <- sample(1:n,n/5)
traind <- Fishing[-samp,c(2:10)]
train1 <-as.numeric(Fishing[-samp,1])
train2 <-Fishing[-samp,1]
testd <- Fishing[samp,c(2:10)]
test1 <- as.numeric(Fishing[samp,1])
test2 <- Fishing[samp,1]

# linout = T
fit <- nnet( traind , train1 , maxiter = 1000,
            linout = TRUE, size = 2, decay = 0.1)

# Define an activation func for multinomial probit model
# Using the mlogit to define the dist of error term
mode_predict <- predict(fit, testd, type = "raw")
mode_predict
1-abs(mean(test1 - mode_predict))

# almost all results in fit_multinomial probit regression are significant
# 1: beach(benchmark)  2: pier  3: boat  4: charecter
# uti_ <- [uti_pier, uti_boat, uti_charecter]
b0 <- c(2.9661e-01, -1.1658e-01, 4.7498e-01)     # intercept
b1 <- c(-6.9660e-03, -6.9660e-03, -6.9660e-03)  # pirce
b2 <- c(-3.7634e-05, 3.9809e-05, -4.6794e-05 )  # income 
coeff <- cbind(b0,b1,b2)
coeff <- as.matrix(coeff)

uti<- matrix(rep(0),nrow = 946, ncol = 3 ,byrow =T)
for (i in 1:946){
  ind <- as.vector(traind[i,])
  p <- as.vector(ind[,c(2,3,4)])
  x <- rbind(c(1,1,1),p,as.vector(ind[,9]))
  uti[i,] <- c(coeff[1,]%*%(x[,1]),coeff[2,]%*%x[,2],coeff[3,]%*%x[,3]) 
}
uti_<-cbind(rep(0),uti)

pro <- matrix(rep(0),nrow = 946, ncol = 4 ,byrow =T)
output <-matrix(rep(0),nrow = 946, ncol = 1 ,byrow =T)
for(i in 1:946){
  pro[i,] <- 1-pnorm(uti_[i,],0,1)
  output[i,] <- which(pro[i,]==max(pro[i,]),arr.ind = T)
}
1-abs(mean(train1 - output))


######################################################
###    Form the generalized activation function 
######################################################


b0 <- c(2.9661e-01, -1.1658e-01, 4.7498e-01)     # intercept
b1 <- c(-6.9660e-03, -6.9660e-03, -6.9660e-03)  # pirce
b2 <- c(-3.7634e-05, 3.9809e-05, -4.6794e-05 )  # income 
coeff <- cbind(b0,b1,b2)
coeff <- as.matrix(coeff)


actfun <-function(x){
  len <- length(x[,1])
  uti<- matrix(rep(0),nrow = len, ncol = 3 ,byrow =T)
  for (i in 1:len){
    ind <- as.vector(x[i,])
    p <- as.vector(ind[,c(3,4,5)])
    beta <- rbind(c(1,1,1),p,as.vector(ind[,10]))
    uti[i,] <- c(coeff[1,]%*%(beta[,1]),coeff[2,]%*%beta[,2],coeff[3,]%*%beta[,3]) 
  }
  uti_<-cbind(rep(0),uti)
  pro <- matrix(rep(0),nrow = len, ncol = 4 ,byrow =T)
  output <-matrix(rep(0),nrow = len, ncol = 1 ,byrow =T)
  for(i in 1:len){
    pro[i,] <- 1-pnorm(uti_[i,],0,1)
    output[i,] <- which(pro[i,]==max(pro[i,]),arr.ind = T)
  }
  return(pro)
}
head(actfun(Fishing))


