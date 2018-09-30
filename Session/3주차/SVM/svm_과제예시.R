######1 Ffire 

rm(list=ls())
#PACKAGES
library(dplyr)
library(kernlab)
library(ROCR)
library(caret)
library(e1071)

#INPUT
setwd("C:/Users/eunse/Desktop/svm_10th_es")
data<-read.csv("Ffires.csv")

str(data)
dim(data)

#분석의 시작은 그림!
hist(data$area)
rug(data$area)
#상당히 왼쪽으로 치우쳐 있음.

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))  
}

data$temp <- normalize(data$temp)
data$rain <- normalize(data$rain)
data$RH <- normalize(data$RH)
data$wind <- normalize(data$wind)

sum(data$area < 5) 

sum(data$area >= 5)

#고르지 않게 나뉜다.

data$size <- NULL
data$size <- factor(ifelse(data$area < 5, 1, 0),
                    labels = c("small", "large"))
#tail(data[, c("size", "area")])  #  checks out

#train/test
idx<- sample(1:nrow(data),round(nrow(data)*0.7),rep=F)
train<-data[idx,]
test<-data[-idx,]

m.poly <- ksvm(size ~ temp + RH + wind + rain,
               data = train,
               kernel = "polydot", C = 1)


m.poly

m.rad <- ksvm(size ~ temp + RH + wind + rain,
              data = train,
              kernel = "rbfdot", C = 1)
m.rad

m.tan <- ksvm(size ~ temp + RH + wind + rain,
              data = train,
              kernel = "tanhdot", C = 1)

m.tan

#가장 성능이 괜찮은 RBF

pred.1 <- predict(m.rad, newdata = test, type = "response")
pred.2 <- predict(m.rad, newdata = test, type = "votes")
pred.3 <- predict(m.rad, newdata = test, type = "decision")

#각각 다른 type값들에 따라 다른 형태의 결과값 도출
pred.1
pred.2
pred.3

table(pred.1, test[,"size"])

confusionMatrix(table(pred.1, test[, "size"]), positive = "small")


########2 SN_ad data
rm(list=ls())
getwd()
dir()
sn<-read.csv("SN_ad.csv")
str(sn) #400 X 5
head(sn)

#필요한 변수만 
sn<-sn[,-c(1,2)] #ID , Gender 제거!
head(sn)

#scale
sn[,1:2]<-scale(sn[,1:2])
head(sn)

#train/test
i<-sample(1:nrow(sn),round(nrow(sn)*0.7),rep=F)
train<-sn[i,]
test<-sn[-i,]

#install.packages("e1071")

#적합(linear/ radial)
library(e1071)

##linear
classifier.linear<-svm(formula =Purchased ~., 
                data= train,
                type = "C-classification",
                kernel = "linear")
summary(classifier.linear)

#예측
pre.linear<-predict(classifier.linear, newdata= test[,-3])

pre.linear

#acc 확인
acct<-addmargins(table(test[,3],pre.linear))
(acct[1]+acct[5])/acct[9]


##RBF
classifier.radial<-svm(formula =Purchased ~., 
                       data= train,
                       type = "C-classification",
                       kernel = "radial")
summary(classifier.radial)

#예측
pre.radial<-predict(classifier.radial, newdata= test[,-3])

#acc 확인
acct<-addmargins(table(test[,3],pre.radial))
(acct[1]+acct[5])/acct[9]


#linear에 비해 상승

####시각화하기

#install.packages("ElemStatLearn")
library(ElemStatLearn)

#linear Kernel
#plot for train
X1 <- seq(min(train[,1]) - 1, max(train[,1]) + 1, by = 0.01)
X2 <- seq(min(train[,2]) - 1, max(train[,2]) + 1, by = 0.01)
grid.set <- expand.grid(X1, X2)
colnames(grid.set) = c('Age','EstimatedSalary')
y.grid = predict(classifier.linear, newdata = grid.set)
plot(train[,-3],
     main = 'Classifier_linear Model (Training Set)',
     xlab =  'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y.grid), length(X1), length(X2)), add = TRUE)

points(grid.set, pch = '.', col = ifelse(y.grid == 1, 'springgreen3', 'tomato'))
points(train, pch = 21, bg= ifelse(train[,3] == 1,'green4', 'red3'))

#plot for test
X1 <- seq(min(test[,1]) - 1, max(test[,1]) + 1, by = 0.01)
X2 <- seq(min(test[,2]) - 1, max(test[,2]) + 1, by = 0.01)
grid.set <- expand.grid(X1, X2)
colnames(grid.set) = c('Age','EstimatedSalary')
y_grid = predict(classifier.linear, newdata = grid.set)
plot(test[,-3],
     main = 'Classifier linear Model (Test Set)',
     xlab =  'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y.grid), length(X1), length(X2)), add = TRUE)

points(grid.set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(test, pch = 21, bg= ifelse(test[,3] == 1,'green4', 'red3'))


##RBF 시각화
#plot for train
X1 <- seq(min(train[,1]) - 1, max(train[,1]) + 1, by = 0.01)
X2 <- seq(min(train[,2]) - 1, max(train[,2]) + 1, by = 0.01)
grid.set <- expand.grid(X1, X2)
colnames(grid.set) = c('Age','EstimatedSalary')
y.grid = predict(classifier.radial, newdata = grid.set)
plot(train[,-3],
     main = 'Classifier_RBF Model (Training Set)',
     xlab =  'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y.grid), length(X1), length(X2)), add = TRUE)

points(grid.set, pch = '.', col = ifelse(y.grid == 1, 'springgreen3', 'tomato'))
points(train, pch = 21, bg= ifelse(train[,3] == 1,'green4', 'red3'))

#plot for test
X1 <- seq(min(test[,1]) - 1, max(test[,1]) + 1, by = 0.01)
X2 <- seq(min(test[,2]) - 1, max(test[,2]) + 1, by = 0.01)
grid.set <- expand.grid(X1, X2)
colnames(grid.set) = c('Age','EstimatedSalary')
y_grid = predict(classifier.radial, newdata = grid.set)
plot(test[,-3],
     main = 'Classifier RBF Model (Test Set)',
     xlab =  'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y.grid), length(X1), length(X2)), add = TRUE)

points(grid.set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(test, pch = 21, bg= ifelse(test[,3] == 1,'green4', 'red3'))