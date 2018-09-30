rm(list=ls())
# 데이터
library(MASS)
data(Boston)
str(Boston)

##### 변수설명 ########################################
# crim : per capita crime rate by town.
# zn :proportion of residential land zoned for lots over 25,000 sq.ft.
# indus :proportion of non-retail business acres per town.
# chas : Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# nox : nitrogen oxides concentration (parts per 10 million).
# rm : average number of rooms per dwelling.
# age : proportion of owner-occupied units built prior to 1940.
# dis : weighted mean of distances to five Boston employment centres.
# rad : index of accessibility to radial highways.
# tax : full-value property-tax rate per \$10,000.
# ptratio : pupil-teacher ratio by town.
# black : 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# lstat : lower status of the population (percent).
# medv : median value of owner-occupied homes in \$1000s.
#######################################################

##### 회귀분석하기 #####
### medv(본인 소유의 주택가격(중앙값) 단위: $1,000)을 예측해주세요
attach(Boston)

# 히스토그램으로 구조보기

### 회귀 적합 
fit.full <- lm(medv~., Boston) #모든변수로 Y에 적합
summary(fit.full) 

### 모형진단 
par(mfrow=c(2,2))
plot(fit.full) # 선형성, 등분산성 만족한다고 볼 수 있으나 분석자 마다 다름! 

# 독립성
e <-resid(fit.full) # 잔차
plot(predict(fit.full), e) # 시계열 자료가 아니라, 딱히 패턴이 보이진 않지만 분석자마다 다를 수 있음!

# shapiro.test 정규성검정
shapiro.test(e) # p-value < 2.2e-16 정규성 만족X

# 정규성 가정 만족을 위해 반응변수 변환
library(car)
summary(car::powerTransform(medv)) # medv^(0.2)로 변환
medv2 <-medv^(0.2)

# 다시 회귀적합
Boston2<-cbind(Boston[1:13],medv2)
fit.full2 <- lm(medv2~., Boston2) #모든변수로 Y에 적합
summary(fit.full2) 

# 다시 모형진단
par(mfrow=c(2,2))
plot(fit.full2)
e <-resid(fit.full2)
shapiro.test(e) # p-value = 4.105e-12 조금 나아졌지만 아직 정규성 만족 x

### 자료진단 자유롭게 진행!
# 이상점
outlierTest(fit.full2)
# 영향력 관측값
influencePlot(fit.full2, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook’s distance") #원의 크기가 크면 영향력 관측값 의심

# 의심 되는 이상점 영향력 관측치 빼고 다시 회귀 적합
Boston3 <- Boston2[-c(372,413,373,369,381,419),]
fit.full3 <- lm(medv2~., Boston3) #모든변수로 Y에 적합
summary(fit.full3) 

# 다시 모형진단
par(mfrow=c(2,2))
plot(fit.full3)
e <-resid(fit.full3)
shapiro.test(e) # p-value = 1.861e-09 조금 나아졌지만 여전히 정규성 만족 x 

# 다중 공선성
vif(fit.full3) #변수 rad , tax 의심 

# 설명변수를 넣지않은 모델
fit.con <- lm(medv2 ~ 1, Boston3)
# 단계적회귀방법(stepwise)
fit.both <- step(fit.con, list(lower=fit.con, upper=fit.full3), direction = "both")

### 결론
summary(fit.both) 
# R-squared:  0.815,	Adjusted R-squared:  0.8109

### 회귀분석을 통한 예측
predict(fit.both)