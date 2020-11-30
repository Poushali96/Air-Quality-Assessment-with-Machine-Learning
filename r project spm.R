library(caret)
library(pROC)
library(mlbench)
library(class)
library(mice)
library(VIM)
setwd("C:\\Users\\Poushali Sengupta\\Desktop\\research project\\ujwal kaka")
data<-read.csv("RPM10.csv",header = T)
data
str(data)
class(data)
summary(data)
data1<-data[,-1]
data1
View(data1)
tail(data1)
#data pertition#
set.seed(1234)
training <- data1[1:108,]
test <- data1[109:120,]
View(data1)
trcontrol<-trainControl(method = 'repeatedcv',number = 10,repeats = 3)
set.seed(185)
fit <- train(PM10_DUNLOP ~.,data = training, tuneGrid= expand.grid(k=1:10),method = 'knn',trControl=trcontrol, preProc=c('center','scale'))
fit
plot(fit)
varImp(fit)
pred<- predict(fit,newdata = test)
pred
RMSE(pred,test$PM10_DUNLOP)
par(mfrow=c(1,2))
plot(fit, main="Graph for repeatedcv vs neighbours of DUNLOP region")
plot(pred~test$PM10_DUNLOP, xlab="PM FOR GARIAHAT", ylab ="prediction", main="Graph for predition values of PM of DUNLOP in various months")
actual<-test[,3]
actual
A=data.frame(pred,actual)
A
plot(pred,actual, main="the LINEAR REG graph for DUNLOP")
abline(lm(pred~actual),data=A, col="RED")
cor(pred,actual)
mod<-lm(pred~actual)
summary(mod)

##pred with with lat lon##


setwd("C:\\Users\\Poushali Sengupta\\Desktop\\research project\\ujwal kaka")
data2<-read.csv("PM10LAT.csv",header = T)
data2
str(data2)
class(data2)
summary(data2)
data4<-data2[1:252,]
data5<-data4[,-(1:7)]
data3<-data5[,c(2,3,4,5)]
str(data3)
View(data3)
data3
tail(data3)
set.seed(1234)
training <- data3[,-(127:198)]
training
test <- data3[127:144,]
test
trcontrol<-trainControl(method = 'repeatedcv',number = 10,repeats = 3)
set.seed(185)
fit <- train(January ~.,data = training, tuneGrid= expand.grid(k=1:16),method = 'knn',trControl=trcontrol, preProc=c('center','scale'))
fit
varImp(fit)
pred<- predict(fit,newdata = test)
pred
plot(fit)

