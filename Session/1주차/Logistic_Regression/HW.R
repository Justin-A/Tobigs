setwd("~/Desktop/Justin/Tobigs/10/1주차/Logistic_Regression/")

##1
library(mlbench)
data("BreastCancer")
str(BreastCancer)

bc = na.omit(BreastCancer[-1])
bc$Class = as.numeric(bc$Class)-1
bc = apply(bc, 2, as.numeric)
bc = data.frame(bc)

library(caret)

#Logistic Reg
accuracy.log = c()
for(i in 1:50){
  idx = createDataPartition(bc$Class, p =0.7, list = F)
  train = bc[idx,]
  test = bc[-idx,]
  model.log = glm(Class ~ ., data = train, family = binomial)
  pred = as.numeric(predict(model.log, test, type = "response") > 0.5)  
  cfm = confusionMatrix(as.factor(test$Class), as.factor(pred))
  accuracy.log = c(accuracy.log,cfm$overall[1])
}

mean(accuracy.log)

##2
load("psub.RData")
data = psub
unique(data$SCHL)
str(data)

#bachdeg
data$bachdeg = 1

#학사학위 미만은 0으로
data[data$SCHL == "some college credit, no degree",]$bachdeg = 0
data[data$SCHL == "Regular high school diploma",]$bachdeg = 0
data[data$SCHL == "no high school diploma",]$bachdeg = 0
data[data$SCHL == "GED or alternative credential",]$bachdeg = 0

#수치형 데이터 전환
data$AGEP = as.numeric(data$AGEP)
data$PINCP = as.numeric(data$PINCP)

#사용할 변수만 선택
data = subset(data, select = c(bachdeg, AGEP, SEX, COW, PINCP, SCHL))

library(caret)

#train/test 분할
idx = createDataPartition(data$SCHL, p = 0.7, list = F)
train = data[idx,]
test = data[-idx,]

#모델 적합
model = glm(bachdeg ~ AGEP + SEX + COW + PINCP , data = train, family = 'binomial')

#예측
test$pred = as.numeric(predict(model, test[1:5], type = 'response') > 0.5)

#실제 값과 예측값 비교
confusionMatrix(as.factor(test$bachdeg), as.factor(test$pred))