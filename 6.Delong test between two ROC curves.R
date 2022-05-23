library(e1071)
library(pROC)
#the Delong test of radiomics signature and radiomics nomogram between two test ROC curve. 
#For the radiomics signature, the list of the csv is group, radscore.For the radiomics nomogram, the list of the csv is group, age, CA125, radscore
train1 <- read.csv("D:/data/training group.csv",header = T)
test1 <- read.csv("D:/data/test group.csv",header = T)
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Arial"))
par(mar = c(4.7,5.1,2.1,1),mfrow=c(1,1),oma = c(0,0,0,0),family="A")
mx_radiomics <- glm(group ~ Radscore, family= binomial(link="logit"),data=train1,control=list(maxit=1000)) 
summary(mx_radiomics)
trainR <- predict(mx_radiomics,type='response')
roc1 <- lines.roc(train1$group, trainR, percent=TRUE, col="1",)

#ROC curve of test group
pre_radiomics <- predict(mx_radiomics,test1,type='response')
roc2 <- plot.roc(test1$group, pre_radiomics, family="A", print.auc=T, print.auc.x=75, print.auc.y=92, col="blue", lty=2, percent=TRUE,cex.axis=1.5,
                 cex.lab=1.5,
                 cex.main=1.5,
                 cex.sub=1.5)
#Put into the group to be compared with Delong test
train2 <- read.csv("D:/data/training group regroup by Tesla.csv",header = T)
test2 <- read.csv("D:/data/test group regroup by Tesla.csv",header = T)

mx_clinical <- glm(group ~ radscore, 
                   family= binomial(link="logit"),data=train2,control=list(maxit=1000)) 
summary(mx_clinical)
trainC <- predict(mx_clinical,type='response')

#ROC curve of test group regroup by Tesla
pre_clinical <- predict(mx_clinical,test2,type='response')
roc4 <- plot.roc(test2$group,pre_clinical,add=TRUE, family="A", print.auc=T, print.auc.x=80, print.auc.y=78, col="black", lty=1, percent=TRUE,cex.axis=1.5,
                 cex.lab=1.5,
                 cex.main=1.5,
                 cex.sub=1.5)
#the Delong test between these two test group ROC curves
testobj<- roc.test(roc2,roc4,method = "delong")
roc.test(roc2,roc4,method = "delong")
text(50, 50, labels=paste("Delong p-value =", formatC(testobj$p.value,format='f',digits=3)), adj=c(0, .5), cex=1.1)
legend(68, 11, 
       bty = "n", legend=c("Radiomics signature of the original test group", "Radiomics signature of the test group for cross-validation"), col=c("blue", "black"), lwd=2, lty=c(2,1), cex=1.15)

