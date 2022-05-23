library(pROC)
library(glmnet)
library(ggplot2)
#The list of the csv is group and features' name of icc>0.75, pearson<0.9 
m <- read.csv("D:/data/outputicc and pearson.csv",header = T)
m[,1] = as.factor(m[,1])
set.seed(3)
fit<-glmnet(x=as.matrix(m[,2:1129]),y=m[,1],alpha=1,family="binomial",nfolds=10,nlambda=100)
plot(fit,xvar="lambda", label = TRUE,cex.axis=1.5,
     cex.lab=1.5,
     cex.main=2,
     cex.sub=2,)  
fit<-cv.glmnet(x=as.matrix(m[,2:1129]),y=m[,1],alpha=1,family="binomial",
               nlambda=100,nfolds=10,type.measure = c("deviance"))
plot(fit,cex.axis=1.5,
     cex.lab=2,
     cex.main=2,
     cex.sub=2,)

fit.best <- fit$glmnet.fit #Corresponding optimal model
fit.coef <- coef(fit$glmnet.fit, s = fit$lambda.min) #coefficient,radiomics score
fit.coef[which(fit.coef != 0)] #Selected features
summary(fit.coef) #show the names and coefs of variations


