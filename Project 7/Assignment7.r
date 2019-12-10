#loading in all needed packeges, ISLR, gam, randomForest, rpart, tree, pROC.
library(ISLR)
library(gam)
library(randomForest)
library(rpart)
library(tree) 
library(pROC)

#storing Wage in variable Health_Data
Health_Data <- Wage
#dividing the data into training and testing data sets
#Elements 1-2700 are part of the training set.
#Elements 2700-3000 are part of the testing set.
Health.train <- Health_Data[1:2700,-8]
Health.test <- Health_Data[2701:3000,-8]

#We decided to work with the health_ins binary variable.

#Creates a logit model using the glm function (package), is then stored in Log_Reg.
Log_Reg<- glm(health_ins ~ age + maritl + wage + logwage + race + education + jobclass , family=binomial(link=logit), data = Health.train)
#Puts out results of glm which were stored in Log_Reg.
summary(Log_Reg)

#Creates a logit model using the gam function (package), is then stored in Gam_Reg.
Gam_Reg <- gam(health_ins ~ s(age) + s(logwage)+ s(wage) + maritl + race + education + jobclass, family = binomial(link = logit), data = Health.train)
#Puts out results of gam which were stored in Gam_Reg.
summary(Gam_Reg)

#Creates a model using the randomForest method, is then stored in RandomForest_Reg.
RandomForest_Reg <- randomForest(health_ins ~ age + maritl+ wage + logwage + race + education + jobclass ,data = Health.train)
#Puts out results of randomForest which were stored in RandomForest_Reg.
summary(RandomForest_Reg)

#Creates a model using the tree method, is then stored in Tree2_Reg.
Tree2_Reg<- tree(health_ins ~ age + maritl + wage + logwage + race + education + jobclass ,data = Health.train)
#Puts out results of tree which were stored in Tree2_Reg.
summary(Tree2_Reg)

#Creates a model using the rpart method, is then stored in Rpart_Reg.
Rpart_Reg<- rpart(health_ins ~ age + maritl + wage + logwage + race + education + jobclass ,data = Health.train)
#Puts out results of rpart which were stored in Rpart_Reg.
summary(Rpart_Reg)

#The below section contains the portion of the R code which runs predictions on the test data set.
#Each section will contain a commented title indicating which method of regression modeling is being run on that respective testing data set.

#TREE
tree.pred<-predict(Tree2_Reg,Health.test,type="response")
Health.train$tree.pred = tree.pred
tree <- roc(Health.test$health_ins, tree.pred, plot = T)

#GLM
log.pred<-predict(Log_Reg,Health.test,type="response")
Health.train$log.pred = log.pred
log<-roc(Health.test$health_ins, log.pred, plot=T)

#GAM
gam.pred <-predict(Gam_Reg, Health.test, type=c("response"))
roc(Health.test$health_ins,gam.pred,plot=T)
gam<-roc(Health.test$health_ins,gam.pred)

#RPART
Rpart_Reg.pred<-predict(Rpart_Reg,Health.test,type=c("prob"))
roc(Health.test$health_ins,Rpart_Reg.pred[,2],plot=T)

Rpart<-roc(Health.test$health_ins,Rpart_Reg.pred[,2])
lines.roc(Rpart,type="lines",col=2)
print(Rpart)

#randomForest
RF.pred<-predict(RandomForest_Reg,Health.test,type=c("prob"))
roc(Health.test$health_ins,RF.pred[,2],plot=T)

# **
RF<-roc(Health.test$health_ins,Rpart_Reg.pred[,2])
lines.roc(RF,type="lines",col=2)
print(RF)

#The code below enables us to graph GLM, GAM, and TREE predictions togther on one graph.
#In combination with the code above indicated with '**'
lines.roc(log, col = 1, lty =2, main = "ROC")
lines.roc(gam, col = 2, lty =3, add = TRUE)
lines.roc(tree, col = 3, lty =4, add = TRUE)

#Together with the code given in '**' and the three lines directly above this comment, we
#get a graph of all 5 ROC plots in one graph.