library(pROC)
#My figure was based on this code by performing the clinic model, radiomics signature, radiomics nomogram data once each
#The list of the csv is group and features selected by lasso regression
#calculate the radscore by the intercepts and coefficients obtained from logistic regression
train <- read.csv("D:/data/training group.csv",header = T)
test <- read.csv("D:/data/test group.csv",header = T)   
train[,1] = as.factor(train[,1])  
test[,1] = as.factor(test[,1])

performance<- function(table,n=2){
  if(!all(dim(table)==c(2,2)))
    stop("Must be a 2*2 table")
  tn=table[1,1]
  fn=table[2,1]
  tp=table[2,2]
  fp=table[1,2]
  sensitivity=tp/(tp+fn)
  specificity=tn/(tn+fp)
  ppp=tp/(tp+fp)
  npp=tn/(tn+fn)
  hitrate=(tp+tn)/(tp+tn+fp+fn)
  F1=2*sensitivity*ppp/(ppp+sensitivity)
  result<- rbind(sensitivity,specificity,ppp,npp,hitrate,F1)
  rownames(result)<- c("sensitivity","specificity","positivive predictive value","negtive predictive value","accuracy","F1")
  colnames(result)<- c("model")
  return(result)
}

fit.logit<- glm(group~.,data = train,family = binomial())
summary(fit.logit)
prob.train<- predict(fit.logit,train,type="response")
prob.test<- predict(fit.logit,test,type="response")
#prob.test<- predict(fit.logit,test,type="link") 
pred.logit.train<-  factor(prob.train>0.5,levels = c(FALSE,TRUE),labels = c("0","1"))
pred.logit.test<-  factor(prob.test>0.5,levels = c(FALSE,TRUE),labels = c("0","1"))
pref.logit.train<-table(train$group,pred.logit.train,dnn=c("Actual","Predicted"))
pref.logit.test<-table(test$group,pred.logit.test,dnn=c("Actual","Predicted"))
pref.logit.train
pref.logit.test

#logistic confusion matrix
per.logit.train<- performance(pref.logit.train)
per.logit.test<- performance(pref.logit.test)
#logistic ROC curve
library(pROC)
pre.logit.train <- predict(fit.logit,train,type='response')
pre.logit.test <- predict(fit.logit,test,type='response')
#training group ROC
preroc.logit.train <- roc(train$group,pre.logit.train)
ci_de.logit.train<-ci(preroc.logit.train,method="delong")
ci_de.logit.train
round(ci_de.logit.train,3)
plot(preroc.logit.train,                    
     print.auc=T,                    
     print.thres=T,                   
     auc.polygon=T,                   
     max.auc.polygon=T,               
     grid=c(0.2),                     
     grid.col=c("green", "red"),      
     auc.polygon.col="skyblue",        
     main="",  
     col="1") 
#test group ROC
preroc.logit.test <- roc(test$group,pre.logit.test)
ci_de.logit.test<-ci(preroc.logit.test,method="delong")
ci_de.logit.test
round(ci_de.logit.test,3)
plot(preroc.logit.test,                  
     print.auc=T,                      
     print.thres=T,                  
     auc.polygon=T,                
     max.auc.polygon=T,             
     grid=c(0.2),                     
     grid.col=c("green", "red"),      
     auc.polygon.col="skyblue",        
     main="",  
     col="1") 
