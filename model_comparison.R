library(MASS)
library(ISLR)
#program to compare classification models(LDA,QDA,KNN)
#Logistic Regression
#creation of training data
attach(Smarket)
train<-(Year<2005)
#test data
Smarket.2005<-Smarket[!train,]
Direction.2005<-Direction[!train]
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs<-predict(glm.fit,Smarket.2005,type="response")
glm.preds<-rep("Down",252)
glm.preds[glm.probs>0.5]<-"Up"
#gives confusion matrix
table(glm.preds,Direction.2005)
mean(glm.preds==Direction.2005)
#imrpove models by just including variables that are significant (low-p values)
glm.fit<-glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs<-predict(glm.fit,Smarket.2005,type="response")
glm.preds<-rep("Down",252)
glm.preds[glm.probs>0.5]<-"Up"
table(glm.preds,Direction.2005)
mean(glm.preds==Direction.2005)
########################################################################
#LDA
lda.fit<-lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.pred<-predict(lda.fit,Smarket.2005)
#names(lda.pred)
# class posterior x

#confusion matrix
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
#Market will go down
sum(lda.pred$posterior[,1]>=0.5)
#market will go up
sum(lda.pred$posterior[,1]<0.5)
mean(lda.pred$class==Smarket.2005)#similar to the logistic regression model
################################################
#QDA model
qda.fit<-qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
#linear distriminant not contained here as the function is quadratic
qda.predict<-predict(qda.fit,Smarket.2005)
table(qda.predict$class,Direction.2005)
mean(qda.predict$class==Direction.2005)# Accuracy of 59%
####################################################################
#KNN
library(class)
train.mat<-cbind(Lag1,Lag2)[train,]
test.mat<-cbind(Lag1,Lag2)[!train,]
result<-Direction[train]
knn.model<-knn(train.mat,test.mat,result,k=1)
table(knn.model,Direction.2005)
mean(knn.model==Direction.2005)#50%
#increasing k=3
knn.model1<-knn(train.mat,test.mat,result,k=3)
table(knn.model1,Direction.2005)
mean(knn.model1==Direction.2005)#increased to 52.7%
#k=5
knn.model2<-knn(train.mat,test.mat,result,k=5)
table(knn.model2,Direction.2005)
mean(knn.model2==Direction.2005)#decreased to 48.4%
mean(knn.model2==Direction.2005)#increased to 52.7%













