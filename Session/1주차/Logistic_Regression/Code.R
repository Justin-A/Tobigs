rm(list=ls())
Sys.setlocale('LC_CTYPE', 'ko_KR.UTF-8')
setwd('~/Desktop/Justin/Tobigs/10/1주차/Logistic_Regression/')

#1
library(boot)
data(nodal)
str(nodal)
?nodal
table(nodal$m)
rd = nodal[,-1]

table(rd$r)

model = glm(r~., data = rd, family = binomial) # Family = binomial : Logistic Regression
summary(model) # 회귀계수는 exp 씌워서 결과 확인

predict(model) #output은 logit

sigmoid = function(x) {
  return(exp(x)/(1+exp(x))) 
}

sigmoid(predict(model)) #sigmoid를 걸어야 원하는 확률값이 나온다!
predict(model, type = "response") #output은 sigmoid를 거친 값!

#2
#은행 예금에 가입할 것인지 예측을 위한 데이
bank = read.csv("bank-additional.csv", sep = ";")
str(bank)

#Feature Selection by Hand
select1 = colnames(bank)[c(1,2,3,6,7,8:10,12,15,17:19,21)]
select11 = colnames(bank)[c(1,2,3,6,7,8:10,12,15,17:19)]
formula1 = formula(paste("y~",paste(select11, collapse=" + ")))

bank = bank[select1]
bank$y = as.factor(ifelse(bank$y == "no",0,1)) # YES, NO -> 0, 1
str(bank)

#train/test partition
library(caret)
idx = createDataPartition(bank$y, p = 0.7, list = F)
banktrain = bank[idx,]
banktest = bank[-idx,]

##Model1
# Accuracy가 높지만, 보험을 가입할 사람을 맞춰야 한다. 이에 적합한 지표는 특이도인데, 특이도가 너무 낮다.
model.glm1 = glm(formula1, banktrain, family = binomial)
pred.glm1 = as.numeric(predict(model.glm1, banktest, type = "response") > 0.5)
confusionMatrix(as.factor(pred.glm1),as.factor(banktest$y))
table(pred.glm1)

##Model2 
model.glm2 = glm(formula1, banktrain, family = binomial)
pred.glm2 = as.numeric(predict(model.glm2, banktest, type = "response") > 0.3)
confusionMatrix(as.factor(pred.glm2),as.factor(banktest$y))
table(pred.glm2)

#Upsample
table(banktrain$y)
banktrain_up = upSample(subset(banktrain, select=-y), banktrain$y)
table(banktrain_up$Class)
formula2 = formula(paste("Class~",paste(select11, collapse=" + ")))

##Model3
model.glm3 = glm(formula2, banktrain_up, family = binomial)
pred.glm3 = as.numeric(predict(model.glm3, banktest, type = "response") > 0.5)
confusionMatrix(as.factor(pred.glm3),banktest$y)
table(pred.glm3)

#ROC
library(ROCR)
pred_glm <- prediction(as.numeric(pred.glm3),as.numeric(banktest$y))
perf_glm <- performance(pred_glm, measure = "tpr", x.measure = "fpr") # measure 인자 맞춰야 ROC Curve가 나온다.
plot(perf_glm, main = "ROC curve for GLM", col = "blue", lwd = 2)

#AUC
auc_glm = performance(pred_glm, measure = "auc") ; auc_glm
auc_glm@y.values[[1]]
