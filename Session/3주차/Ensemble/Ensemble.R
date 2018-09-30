rm(list=ls())

# apt 가격 데이터 셋
### setwd 
setwd("~/Desktop/Justin/Tobigs/10/3주차/Ensemble/")

### packages
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(lime)) install.packages("lime"); library(lime)
if(!require(mice)) install.packages("mice"); library(mice)

### data 불러오기
dat = read.csv("dat.csv")
head(dat)
str(dat)
dat$is_cancer = as.factor(dat$is_cancer)
dat <- dat[-1]

### train / test 분할
set.seed(1)
table(dat$is_cancer)
idx = createDataPartition(dat$is_cancer, p = 0.7, list=F)
train = dat[idx,]
test = dat[-idx,]
table(train$is_cancer)
table(test$is_cancer)
str(train)

### randomforest
control = trainControl(method='cv',
                       search='random',
                       number=5,
                       verbose = TRUE) # Random Exploring 

rf.model <- train(
  is_cancer ~ .,
  data = train,
  tuneLength = 10,
  trControl = control,
  method = "rf") # Training


rf.grid = expand.grid(
  .mtry = c(1,3,5)
) # Grid

control = trainControl(method = 'cv',
                       search = 'grid',
                       number = 5,
                       verbose = TRUE) # Grid Exploring

rf.model <- train(
  is_cancer ~ .,
  data = train,
  tuneGrid = rf.grid,
  trControl = control,
  method = 'rf'
)

rf.model

pred.rf <- predict(rf.model,test[-31])
confusionMatrix(pred.rf, test[,31])

### xgboost
## random 탐색
control = trainControl(method='cv', search='random', number=5,verbose = TRUE)
xgb.model <- train(
  is_cancer ~ .,
  data = train,
  tuneLength = 10,
  trControl = control,
  method="xgbTree")

xgb.model

pred.xgb <- predict(xgb.model, test[-31])
confusionMatrix(pred.xgb, test[,31])

## 격자 탐색
xgb.grid = expand.grid(
  nrounds = c(300,500),
  eta = c(0.03,0.05),
  gamma = c(3,5),
  max_depth = c(4,6),
  min_child_weight = c(6,8),
  colsample_bytree = c(0.3,0.5), 
  subsample = c(0.2,0.6)
)

control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
xgb.model <- train(
  is_cancer ~ .,
  data = test,
  tuneGrid = xgb.grid,
  trControl = control,
  method = 'xgbTree'
)

### gbm
## random 탐색
control = trainControl(method='cv', search='random', number=2,verbose = TRUE)
gbm.model <- train(
  is_cancer ~ .,
  data = train,
  tuneLength = 3,
  trControl = control,
  method="gbm")

gbm.model$bestTune

pred.gbm <- predict(gbm.model,test[-31])
confusionMatrix(pred.gbm, test[,31])

## 격자 탐색
gbm.grid = expand.grid(
  shrinkage = c(0.1,0.3),
  interaction.depth = c(3,6,9),
  n.minobsinnode = c(5,10,15),
  n.trees = c(500,100,1500)
)

control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
gbm.model <- train(
  is_cancer ~ .,
  data = test,
  tuneGrid = gbm.grid,
  trControl = control,
  method = 'gbm'
)

pred.gbm <- predict(gbm.model,test[-31])
confusionMatrix(pred.gbm, test[,31])


dim(train)
### lime
#### lime ####
explainer <- lime(train[,-31], xgb.model)
explanation <- explain(test[1:5,-31],            # test data # 많이 넣으면 튀니까 적게 넣어줘야 함 
                       explainer,                # lime 적용 
                       labels = NULL,            # 모델이 분류기일 경우 지정
                       n_labels = 1,          # 모델이 분류기일 경우 지정
                       n_features = 3,           # 각 설명에 사용할 기능의 수
                       #n_permutations = 2,      # 각 설명에 사용할 순열 수
                       feature_select = 'auto',  # auto, highest_weighs, none,
                       # forward_selection, lasso_path, tree
                       dist_fun = 'gower',       # 거리 함수
                       kernel_width = NULL       # dist_fun이 gower일 경우 NULL
)
explanation[,1:9]
plot_features(explanation, ncol = 2)
?explain



##################################################################################3
################과제###############################################################
#####################################################################################
### 여러가지 모델을 형성 하고 stacking 적용하는 함수 만들기




# 데이터 불러오기 (수정하지 말것)
# df <- read.csv('apt.csv')  # 학습용 데이터
# test <- read.csv('mid_X.csv')  # 테스트용 데이터(실제 테스트는 중간테스트 데이터와 파일명은 같고 내용은 다름)
apt_train <- read.csv('apt_train.csv')
apt_test <- read.csv('apt_test.csv')


# 학습 및 예측 (수정하여 자신의 모형을 학습시킬 것)
# hint1. 결측치가 많은 열은 제거, 20%이하 일 경우 mice 함수로 대체
# mc <- mice(apt[,!names(apt) %in% 'price'], method='rf')
# miceOutput <- complete(mc)
# apt2 <- cbind(miceOutput,price=apt$price)


# hint2. 범주형 변수는 더미화
# heat_source_dm = dummyVars('~ heat_source', apt2)
# heat_source_dm = data.frame(predict(heat_source_dm, apt2))
#
# heat_type_dm = dummyVars('~ heat_type', apt2)
# heat_type_dm = data.frame(predict(heat_type_dm, apt2))
#
# apt4 <- apt2[!colnames(apt2) %in% c('heat_source','heat_type')]
# apt4 <- cbind(apt4,heat_source_dm,heat_type_dm)