library(rmda)
#The list of the test group is group,age, CA125, radscore and radiologist(diagnostic results of radiologist without radiomics nomogram), radiologist2(diagnostic results of radiologist with the help of radiomics nomogram).
m <- read.csv("D:/data/test group.csv",header = T)


head(m)
Radiomics<- decision_curve(group~Radscore,data = m, 
                        family = binomial(link ='logit'),thresholds= seq(0,1, by = 0.01), 
                        confidence.intervals =0.95,study.design = 'case-control',population.prevalence = 0.5)
Doctor<- decision_curve(group~radiologist,data = m, 
                          family = binomial(link ='logit'),thresholds= seq(0,1, by = 0.01), 
                          confidence.intervals =0.95,study.design = 'case-control',population.prevalence = 0.5)
Nomograme <- decision_curve(group~Radscore+age+CA125,data = m, 
                           family = binomial(link ='logit'),thresholds= seq(0,1, by = 0.01), 
                           confidence.intervals =0.95,study.design = 'case-control',population.prevalence = 0.5)
List<- list(Radiomics, Doctor, Nomograme)
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Arial"))
par(mar = c(4.7,5.1,2.1,1),mfrow=c(1,1),oma = c(0,0,0,0),family="A")
plot_decision_curve(List,curve.names= c('Radiomics','Doctor','Nomograme'),
                     cost.benefit.axis = F,col = c('red','green','blue'),
                     confidence.intervals = FALSE,standardize = FALSE, lty = c(1,1,1),
                     lwd = c(3,2, 2),legend.position ='bottomleft',cex.lab=1.3)#,cex.axis=1.5,
                   # cex.lab=1.5,
                    #cex.main=1.5,
                    #cex.sub=1.5)
summary(Radiomics, measure= 'NB')
Li<- list(Radiomics, Nomograme)
plot_clinical_impact(Nomograme,population.size = 1000,cost.benefit.axis = T,
                     
                     n.cost.benefits= 8,col = c('red','blue'),
                     
                     confidence.intervals= T, legend.position='none',family="A",cex.lab=1.3)#,cex.axis=1.5,
                     #cex.lab=1.5,
                     #cex.main=1.5,
                     #cex.sub=1.5)
legend(0.36,1000,
       c("Radiomics Nomogram Predict","Number DMI"),
       lty = c(1,1),
       lwd = c(1,1),
       col = c("blue","red"),
       bty = "n",cex=1.2)#,cex=1.5) # 


Doctor1<- decision_curve(group~radiologist2,data = m, 
                        family = binomial(link ='logit'),thresholds= seq(0,1, by = 0.01), 
                        confidence.intervals =0.95,study.design = 'case-control',population.prevalence = 0.5)
plot_clinical_impact(Doctor1,population.size = 1000,cost.benefit.axis = T,
                     
                     n.cost.benefits= 8,col = c('red','blue'),
                     
                     confidence.intervals= T, legend.position='none',family="A",cex.lab=1.3)#,cex.axis=1.5,
#cex.lab=1.5,
#cex.main=1.5,
#cex.sub=1.5)
legend(0.5,1000,
       c("Radiologist Predict","Number DMI"),
       lty = c(1,1),
       lwd = c(1,1),
       col = c("blue","red"),
       bty = "n",cex=1.2)#,cex=1.5) # "o"Îª¼Ó±ß¿ò