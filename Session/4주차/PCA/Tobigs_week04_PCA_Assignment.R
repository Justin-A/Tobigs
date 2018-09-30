##########################################
## Principal Component Analysis(PCA)    ##
## 2018-08-09 Tobigs Week 04 Assignment ##
##########################################

### 1. 디렉토리 및 라이브러리 설정 ###
rm(list=ls())
setwd("C:/Users/lady1/Desktop/Tobigs/180718- 10주 세미나/180808 4주차 수업/PCA/PCA_Dataset/Dataset/Training")

## 필요 라이브러리
if(!require(rgl)) install.packages("rgl"); library(rgl) # scatter plot 을 3D로 표현하는 library
if(!require(caret)) install.packages("caret"); library(caret) # confusionMatrix
if(!require(ElemStatLearn)) install.packages("ElemStatLearn"); library(ElemStatLearn) # 시각화 라이브러리 
if(!require(psych)) install.packages("psych"); library(psych) # 시각화 라이브러리 
if(!require(car)) install.packages("car"); library(car) # vif 사용 

### 2. 데이터 확인 ###
train_data <- read.csv("Features_Variant_5.csv") 
test_data <- read.csv("Features_TestSet.csv") 
colnames(train_data) <- c("Page Popularity","Page Checkin","Page talking about","Page Category","Derived1","Derived2","Derived3","Derived4","Derived5","Derived6","Derived7","Derived8","Derived9","Derived10","Derived11","Derived12","Derived13","Derived14","Derived15","Derived16","Derived17","Derived18","Derived19","Derived20","Derived21","Derived22","Derived23","Derived24","Derived25","CC1","CC2","CC3","CC4","CC5","Base time","Post length","Post Share Count","Post Promotion Status","H Local","Sunday Post","Monday Post","Tuesday Post","Wednesday Post","Thursday Post","Friday Post","Saturday Post","Sunday Base","Monday Base","Tuesday Base","Wednesday Base","Thursday Base","Friday Base","Saturday Base","Target") # 변수명 설정 
colnames(test_data) <- c("Page Popularity","Page Checkin","Page talking about","Page Category","Derived1","Derived2","Derived3","Derived4","Derived5","Derived6","Derived7","Derived8","Derived9","Derived10","Derived11","Derived12","Derived13","Derived14","Derived15","Derived16","Derived17","Derived18","Derived19","Derived20","Derived21","Derived22","Derived23","Derived24","Derived25","CC1","CC2","CC3","CC4","CC5","Base time","Post length","Post Share Count","Post Promotion Status","H Local","Sunday Post","Monday Post","Tuesday Post","Wednesday Post","Thursday Post","Friday Post","Saturday Post","Sunday Base","Monday Base","Tuesday Base","Wednesday Base","Thursday Base","Friday Base","Saturday Base","Target") # 변수명 설정 
train_data <- train_data[,-c(5:29)] # 불필요한 파생변수 제거 
test_data <- test_data[,-c(5:29)] # 불필요한 파생변수 제거 

str(train_data) # 데이터 구조 확인
head(train_data) # 데이터 내용 확인 
summary(train_data) # 데이터 요약
# 29개의 변수로 이루어진 199029개의 데이터, 변수가 많으니 차원축소가 필요해보인다. 
# 종속변수 Y인 Target은 factor 변환
# 변수들이 모두 연속형이지만, 파생변수를 제외해도 연관된 변수들이 보인다! 연관된 변수들끼리 상관성을 확인해보자  

# pairs.panels(train_data[,-c(15:28)], pch = 20) # 변수 간 상관성 확인  
# 종속 변수와 상관계수가 높은 변수들이 보인다. 일단 요일변수 빼고 시각화 

train <- train_data[, -29] ; test <- test_data[, -29] # 종속변수가 빠진 train/test data 
train_label <- train_data[, 29] ; test_label <- test_data[, 29] # 종속변수만 남은 train/test data

### 3. 데이터 전처리 ###
## 3-1. PCA 적용 
dev.off() # plot 갱신 
trainPCA <- prcomp(train, scale = T) # 종속변수를 제외한 데이터 PCA 
# 앗 cannot rescale a constant/zero column to unit variance 오류가 뜬다. 분산이 0인 열은 제거해주자. 
which(apply(train, 2, var)==0) # 13번째 열 
train <- train[, apply(train, 2, var) != 0]
test <- test[,-c(13)] 

trainPCA <- prcomp(train, center=T, scale = T) # 종속변수를 제외한 데이터 PCA 
trainPCA$rotation # rotation이 계수값

## 3-2. 변수의 개수 결정(Elbow point & Cumulative Proportion)
plot(trainPCA, type="l") # 2번째나 6번째에서 끊으면 되겠다.
summary(trainPCA) # Elbow point인 PC6까지로는 45%정도만 설명할 수 있다..? 뭔가 부족해보인다. 

# 80이 넘어가는 변수 기준 => 15개 선택
min(which(summary(trainPCA)[[6]][3,] >= 0.80)) # 누적비율을 원하는 비율까지 올려서 누적설명력이 80% 이상인 주성분의 index = 16

trainPCR <- as.matrix(train) %*% trainPCA$rotation
testPCR <- as.matrix(test) %*% trainPCA$rotation

trainF <- cbind(as.data.frame(trainPCR), train_label) # trainset 형성
testF <- cbind(as.data.frame(testPCR), test_label) # testset 형성
str(trainF)

### 4. 모델링 ###
## 만든 주성분으로 회귀분석
# Elbow Point로 !
fit <- lm(train_label~PC1+PC2+PC3+PC4+PC5+PC6, data = trainF)
summary(fit) # 모든 주성분변수의 중요도가 ***으로 나옴..!

# 80이 넘어가는 변수 기준 => 15개 선택
train1 <- trainF[,c(1:15,28)] # 라벨포함 16개
fit1 <- lm(train_label~., data=train1)
summary(fit1) # PC3,4,7빼고 모든 주성분변수의 중요도가 ***으로 나옴..!

### 5. 예측 ###
# RMSE (root mean squared error), MAE (median absolute error) 함수 정의 
rmse <- function(yi, yhat_i){
  sqrt(mean((yi - yhat_i)^2))
}
mae <- function(yi, yhat_i){
  mean(abs(yi - yhat_i))
}

fit_pred <- predict(fit, type="response", newdata = testF)
test_pred <- round(fit_pred)

fit_pred1 <- predict(fit1,type="response",newdata=testF)
test_pred1 <- round(fit_pred1)

mae(test_label, test_pred) 
rmse(test_label, test_pred
mae(test_label,test_pred1) 
rmse(test_label,test_pred1) 

## 최종 모델 ##
# 차원축소를 목표로 한다면 train_label~PC1+PC2+PC3+PC4+PC5+PC6
# 정확도를 목표로 한다면 train_label~PC1+PC2+PC5+PC6+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15