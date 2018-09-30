# packages install
library(caret)
library(lime)
library(mice)

# 데이터 불러오기 
train <- read.csv('train_apt.csv')
train <- train[-1]
test <- read.csv('test_apt.csv')  

# 데이터 전처리
set.seed(1234)

for (i in 1:ncol(train)){
  print(sum(is.na(train[,i])))
}   
for (i in 1:ncol(test)){
  print(sum(is.na(test[,i])))
}   
    ####train, test 모두 결측치가 많기 때문에 하나의 데이터셋으로 묶어서 결측치 처리

test$price <- 0   # train과 합치기 위해 임의로 0값 기입
apt <- rbind(train,test)
dim(apt)

# NA가 아닌 ""으로 되있는것도 NA로 변경
apt$permission_date <- as.numeric(substr(as.character(apt$permission_date),1,4))
apt$asile_type <- ifelse(apt$asile_type=="",NA,apt$asile_type)
apt$earthquake <- ifelse(apt$earthquake=="",NA,apt$earthquake)
apt$heat_source <- ifelse(apt$heat_source=="",NA,apt$heat_source)
apt$heat_type <- ifelse(apt$heat_type=="",NA,apt$heat_type)

# 결측치 3000개 이상 제거
na_col = c()
for (i in 1:ncol(apt)){
  na_col = c(na_col, colnames(apt)[i][sum(is.na(apt[,i]))>3000])
}
apt <- apt[!(colnames(apt) %in% na_col) ]

# 범주형 변수 factor변경, 숫자 numeric 변경
apt$heat_source <- as.factor(apt$heat_source)
apt$heat_type <- as.factor(apt$heat_type)
apt$min_dist_mart <- as.numeric(apt$min_dist_mart)
apt$num_bus <- as.numeric(apt$num_bus) 
apt$num_kindergarten <- as.numeric(apt$num_kindergarten)
apt$num_nursery_school <- as.numeric(apt$num_nursery_school)
apt$num_subway <- as.numeric(apt$num_subway)
apt$price <- as.numeric(apt$price)

# mice로 결측치 채우기
mc <- mice(apt[,!names(apt) %in% 'price'], method='rf',1)   # mice 적용할때 종속변수 빼고 적용, default 값 5로하면 너무 오래걸림, method는 다양함
miceOutput <- complete(mc)
apt2 <- cbind(miceOutput,price=apt$price)

# factor 더미화 (warning 무시)
heat_source_dm = dummyVars('~ heat_source', apt2)
heat_source_dm = data.frame(predict(heat_source_dm, apt2))

heat_type_dm = dummyVars('~ heat_type', apt2)
heat_type_dm = data.frame(predict(heat_type_dm, apt2))

apt3 <- apt2[!colnames(apt2) %in% c('heat_source','heat_type')]
apt3 <- cbind(apt3,heat_source_dm,heat_type_dm)

# train, test로 다시 분리
data.train = apt3[1:28663, ]
data.test = apt3[28664:nrow(apt3), !colnames(apt3) %in% c('price')] # 0으로 채웠던 price 제거

###### 학습 1
xgb.grid = expand.grid(
  nrounds = 721,
  eta = 0.0337,
  gamma = 3.41,
  max_depth = 10,
  min_child_weight = 2,
  colsample_bytree = 0.463,
  subsample = 0.378
)

control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
xgb.model <- train(
  price ~ .,
  data = data.train,
  tuneGrid = xgb.grid,
  trControl = control,
  method = 'xgbTree'
)

pred.xgb = predict(xgb.model, data.test)
pred.xgb = data.frame(price=pred.xgb)

###### 학습 2
control = trainControl(method='cv', search='grid', number=2,verbose = TRUE)
knn.model <- train(
  price ~ .,
  data = data.train,
  tuneLength = 3,
  trControl = control,
  method = 'knn'
)

pred.knn <- predict(knn.model,data.test)
pred.knn = data.frame(price=pred.knn)

###### 학습 3
control = trainControl(method='cv', search='random', number=2,verbose = TRUE)
gbm.model <- train(
  price ~ .,
  data = data.train,
  tuneLength = 3,
  trControl = control,
  method="gbm")

gbm.model$bestTune

pred.gbm <- predict(gbm.model, data.test)
pred.gbm <- data.frame(price=pred.gbm)




#### lime ####
explainer <- lime(data.train[,-34], xgb.model)
explanation <- explain(data.test[1:5,],           # test data
                        explainer,               # lime 적용 
                        labels = NULL,            # 모델이 분류기일 경우 지정
                        n_labels = NULL,          # 모델이 분류기일 경우 지정
                        n_features = 3,           # 각 설명에 사용할 기능의 수
                        #n_permutations = 2,      # 각 설명에 사용할 순열 수
                        feature_select = 'auto',  # auto, highest_weighs, none,
                        # forward_selection, lasso_path, tree
                        dist_fun = 'gower', 
                        kernel_width = NULL
)
explanation[,1:9]
plot_features(explanation, ncol = 2)



##### stacking#####
## 1번째 방법
final_pred = data.frame(price=((pred.xgb + pred.knn + pred.gbm) / 3))
final_pred = data.frame(price=((0.2*pred.xgb + 0.5*pred.knn + 0.3*pred.gbm) / 3))    # 가중평균도 가능


## 2번째 방법
xgb_ensembel = predict(xgb.model,data.train[-34])
knn_ensembel = predict(knn.model,data.train[-34])
gbm_ensembel = predict(gbm.model,data.train[-34])

ensemble_train <- data.frame(xgb=xgb_ensembel, knn=knn_ensembel, gbm=gbm_ensembel, price=data.train$price)
ensemble_test <- data.frame(pred.xgb,pred.knn,pred.gbm)
colnames(ensemble_test) <- c('xgb','knn','gbm')  # 꼭 train과 test 변수명 같아야함


control = trainControl(method='cv', search='random', number=2,verbose = TRUE)
xgb.model_stack <- train(
  price ~ .,
  data = ensemble_train,
  tuneLength = 5,
  trControl = control,
  method = 'xgbTree'
)

pred.stack = predict(xgb.model_stack, ensemble_test)
pred.stack = data.frame(price=pred.stack)







