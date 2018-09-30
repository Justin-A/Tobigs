####################################################################
rm(list=ls()) # Initialize
setwd("~/Desktop/Justin/Tobigs/10/1주차/Simple_Regression/") # Set Working Directory
getwd() # Check Working Directory

#####단순선형회귀#####
fundata <- read.csv("fundata.csv") # Read Data
str(fundata) # Check Data Structure

fundata <- fundata[,-1] # Remove Index
# par(mfrow=c(1,1)) #plots를 2X2으로 나누겠다

fit1 <- lm(y1 ~ x1, fundata) ; summary(fit1) # y1 to x1 Simple Regression
plot(fundata$x1, fundata$y1) # y1 ~ x1 Graph
abline(fit1) # With Regression Line

fit2 <- lm(y2 ~ x2, fundata) ; summary(fit2) # y2 to x2 Simple Regression
plot(fundata$x2, fundata$y2) # y2 ~ x2 Graph
abline(fit2) # With Regression Line

fit3 <- lm(y3 ~ x3, fundata) ; summary(fit3) # y3 to x3 Simple Regression
plot(fundata$x3, fundata$y3) # y3 ~ x3 Graph
abline(fit3) # With Regression Line

fit4 <- lm(y4 ~ x4, fundata) ; summary(fit4) # y4 to x4 Simple Regression
plot(fundata$x4, fundata$y4) # y4 ~ x4 Graph
abline(fit4) # With Regression Line

# Multi Linear Regression
data <- read.csv("data.csv") # Read Another Data
str(data) # Check Data Structure
head(data) # Data Sample

####################################################
# Y : 가솔린 연비
# X1: 배기량
# X2: 마력 
# X3: (엔진의)압축비
# X4: 무게
####################################################

### Preprocessing
names(data) <- c("Y","X1","X2","X3","X4") # Change Colnames
names(data) # Check colnames
# Check NA
sum(is.na(data)) # NA = 0개
# If NA != 0 
data <- na.omit(data) # to Remove Row

### Execute
par(mfrow=c(2,3)) # Graph 2 * 3
hist(data$Y) ; summary(data$Y)
hist(data$X1) ; summary(data$X1)
hist(data$X2) ; summary(data$X2)
hist(data$X3) ; summary(data$X3)
hist(data$X4) ; summary(data$X4)
plot(data) # library(psych) ; pairs.panels(data)

### Fit
fit.full <- lm(Y ~ ., data) # 모든변수로 Y에 적합
summary(fit.full) # R-squared: 0.7665, Adjusted R-squared:  0.7292
# Meaningful: X1 (By p-value = 0.0354 < 0.05)

fit1 <- lm(Y~ . -X4, data) # X4 is largest p-value
summary(fit1) # R-squared:  0.7659,   Adjusted R-squared:  0.7389 
# Maeningful: X1 (By p-value = 0.00207 < 0.05) 

fit2 <- lm(Y~ . -X4 -X3, data) # X3 is next
summary(fit2) # R-squared:  0.7649,   Adjusted R-squared:  0.7475
# Meaningful : X1 (By p-value = 0.000625 < 0.05)

fit3 <- lm(Y~ . -X4 -X3 -X2, data) # X4 is smallest
summary(fit3) # R-squared:  0.7601,   Adjusted R-squared:  0.7515 
# Meaningful : X1 (By p-value = 2e-16 < 0.05)

# Feature Selection by AIC
# Empty Model
fit.con <- lm(Y ~ 1, data) ; summary(fit.con)
# Forward Selection
fit.forward <- step(fit.con, list(lower = fit.con, upper = fit.full), direction = "forward")
summary(fit.forward) # R-squared:  0.7601, Adjusted R-squared:  0.7515, Meaningful : X1

# Backward Elimination
fit.backward <- step(fit.full, list(lower = fit.con), direction = "backward")
summary(fit.backward) # R-squared:  0.7601,   Adjusted R-squared:  0.7515, Meaningful : X1

# Stepwise Selection
fit.both <- step(fit.con, list(lower = fit.con, upper = fit.full), direction = "both")
summary(fit.both) # R-squared:  0.7601,   Adjusted R-squared:  0.7515, Meaningful : X1


### Compare
# compare = anova(fit.full, fit.both) ; compare
# 지금은 같은 모델이라 사용하지 않는다.(보통 p-value가 0.05보다 크면 유의미한 차이 없다고 판단)

### Evaluate

## Plot
# Residuals vs Fitted : 선형성, 오차의 등분산성, 독립성 판단에 사용. 
# 만약 잔차와 예측값사이에 어떤 규칙이 발견된다면 그 규칙을 변수로 추가 할 수 있다. 

# Normal Q-Q : 정규성을 볼 수 있는 그림. 정규성을 만족하면 y = x 직선위에 모든점이 올라와야 된다. 

# Scale-Location : 등분산성을 판단하는 plot, 흩어짐의 정도가 일정해야 함 
#                  흩어짐의 정도가 치우쳐있음, 등분산성에 문제가 있다고 판단한다.

# Residual vs Leverage : 
# 개개의 관찰치에 대한 정보를 제공한다.이상치/큰 지레점/영향관측치를 확인할 수 있다
# 이상치(outlier): 회귀모형으로 잘 예측되지 않는 관측치(즉 아주 큰 양수/음수의 residual)
# 지레점(high leverage point): 예측변수측의 이상치
# 영향치(influential observation): 통계 모형 계수 결정에 불균형한 영향을 미치는 관측치

par(mfrow=c(2,2))
plot(fit.both)
# Residuals vs Fitted - 오차의 등분산성, 선형성, 독립성 판단
# normal Q-Q - 정규성 판단

## shapiro.test 정규성검정
e <-resid(fit.both) # 잔차
shapiro.test(e) # p-value = 0.8677 정규성 만족

# 독립성
plot(predict(fit.both), e) # 딱히 패턴이 보이진 않는다.
plot(predict(fit.both), e, type = 'o')

#install.packages("car")
#vif, outlierTest 등
library(car)

# 다중 공선성
vif(fit.both) #에러 난다.
vif(fit.full)

### 자료진단
# 이상점 
outlierTest(fit.both)
# Nullfor the Bonferonniadjusted outlier test is the observation is an outlier.
## Bonferonni p가 < .05이면 이상점으로 판단
## 이상점 없음

# 영향력 관측값 
influence.measures(fit.both) # *표시 : 영향력 의심되는 관측값 

#influencePlot(fit.both, id.method="identify", main="Influence Plot",
#              sub="Circle size is proportional to Cook’s distance") #원의 크기가 크면 영향력 관측값 의심
#influenceIndexPlot(fit.both, id.n=3)
# Cook's distance measures how much an observation influences the overall model or predicted values
#influencePlot(fit.both, id.n=6)
# Creates a bubble-plot combining the display of Studentizedresiduals, hat-values, and Cook's distance (represented in the circles).

### 결론
summary(fit.both) # y=33.487803-0.47056x1
# R-squared:  0.7601,   Adjusted R-squared:  0.7515
### 회귀분석을 통한 예측값
predict(fit.both)


############################################################################################
##### 지시 변수 처리하기 #####
##data 불러오기
data2 <- read.csv("education1960_70.csv",header=T,stringsAsFactors=FALSE) # Load Data
str(data2) # Data Structure
head(data2) # Sample Data

#summary(data2)
#names(data2)

####################################################
# STATE : 주
# Y : 개인당 들어가는 교육 비용
# X1:수입
# X2:18세 이하 거주자 (천명)
# X3:도시 거주 인구(천명)
# Region: Northeast(1),North Central(2),South(3),West(4)
# Year: 1960 or 1970
#######################################################

##data 탐색 및 전처리
#결측치 확인
sum(is.na(data2)) #0
#있다면 data <- na.omit(data2) 행별로 제거

#구조보기
par(mfrow=c(2,3))
hist(data2$Y) ; summary(data2$Y)
hist(data2$X1) ; summary(data2$X1)
hist(data2$X2) ; summary(data2$X2)
hist(data2$X3) ; summary(data2$x3)
hist(data2$Region) ; summary(data2$Region)
hist(data2$Year) ; summary(data2$Year)

# model.eff = lm(Y ~ X1+X2+X3+Region+Year, data = data2, 
#                contrasts = list(Region="contr.sum"))
# model.matrix(model.eff)

#Region 가변수 처리(4개-> 지시변수는 3개)
data2$Z1 <- ifelse(data2$Region == "1", 1, 0)
data2$Z2 <- ifelse(data2$Region == "2", 1, 0) 
data2$Z3 <- ifelse(data2$Region == "3", 1, 0)
names(data2)
unique(data2$STATE)
data2 <- data2[,c(-1,-6)]
names(data2)
####################################################
# Y : 개인당 들어가는 교육 비용
# X1:수입
# X2:18세 이하 거주자 (천명)
# X3:도시 거주 인구(천명)
# Year: 1960 or 1970
# Z1=0 Z2=0 Z3=0 : Region = West
# Z1=1 Z2=0 Z3=0 : Region = Northeast
# Z1=0 Z2=1 Z3=0 : Region = North Central
# Z1=0 Z2=0 Z3=1 : Region = South
#######################################################
model <- lm(Y ~ ., data = data2) ; summary(model)
empty <- lm(Y ~ 1., data = data2)
full <- lm(Y ~ ., data = data2)

forward_model <- step(object = empty, scope = list(upper = full), direction = "forward")
backward_model <- step(object = full, scope = list(lower = empty), direction = "backward")
stepwise_model <- step(object = empty, scope = list(lower = empty, upper = full), direction = "both")
