library(Hmisc)
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(rms)
#The list of the training group is group, age, CA125, radscore.
test <- read.csv("D:/data/training group.csv",header = T)
head(test)
dd=datadist(test)
options(datadist="dd")
f1 <- lrm(group~., data = test, x=T,y=T)
f1
nom <- nomogram(f1, fun= function(x)1/(1+exp(-x)),
                lp=F,
                funlabel="Risk")
plot(nom)
#The validation of test group is to import the data of test group to f1 then make the validation
validate(f1,method="boot",B=1000,dxy=T)
cal<-calibrate(f1,method="boot",B=1000)
plot(cal, xlab = "Nomogram Predicted Survival", ylab = "Actual Survival",main = "Calibration Curve")
cal1 <- calibrate(f1, cmethod="hare", method="boot", B=1000)
