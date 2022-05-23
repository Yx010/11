rm(list = ls())
library(survival)

library(PredictABEL)
#The list of the csv is YS1 for radiologist1 without radiomics nomogram and YS1H for radiologist1 with the help of radiomics nomogram
egData <- read.csv("D:/data/radiologist diagnosis.csv",fileEncoding = "UTF-8-BOM")
mstd = glm(group~YS1, family=binomial('logit'), data=egData,x=TRUE)
mnew = glm(group~YS1H, family=binomial('logit'), data=egData,x=TRUE)
pstd<-mstd$fitted.values
pnew<-mnew$fitted.values
reclassification(data=egData, cOutcome = 1, predrisk1 = pstd, predrisk2 = pnew, cutoff = c(0, 0.5,1))



