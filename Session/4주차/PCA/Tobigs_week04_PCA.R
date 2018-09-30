#####################################
# Principal Component Analysis(PCA) #
# 2018-08-08 Tobigs Week 04 Class   #
#####################################

rm(list=ls()) 

### 디렉토리 및 라이브러리 설정 ###
setwd("~/Desktop/Justin/Tobigs/10/4주차/PCA/")
if(!require(rgl)) install.packages("rgl"); library(rgl) # scatter plot 을 3D로 표현하는 library

### 1. Random Data 형성 ###
## 3차원 데이터를 만들어보자 
## (주의) PCA 구현하는 데이터를 형성할 때에는 데이터셋의 평균을 항상 0으로 맞춰줘야 합니다!
set.seed(10) # 10기니까 10으로 난수 형성 
X <- rnorm(n=1000, mean=0, sd=100) 
Y <- rnorm(n=1000, mean=0, sd=10)
Z <- rnorm(n=1000, mean=0, sd=50)
range <- c(min(X), max(X)) # 분산이 가장 큰 축인 x축에 맞춰서 range를 변환
plot3d(X, Y, Z, xlim=range, ylim=range, zlim=range) # rgl패키지로 만든 3D Plot

# data의 point가 x 축을 기준으로 분포하고, y축의 샘플들이 굉장히 좁은 것을 볼 수 있습니다.
# x,z로 data 설명할 수 있어보이니 x축과 z축을 corr이 있도록 sorting을 해보겠습니다.
X <- sort(rnorm(n=1000, mean=0, sd=100))
Y <- rnorm(n=1000, mean=0, sd=10)
Z <- sort(rnorm(n=1000, mean=0, sd=50))
range <- c(min(X), max(X))
plot3d(X, Y, Z, xlim=range, ylim=range, zlim=range) 
### Random Data 형성 완료! ###

### 2. PCA를 간단히 구현해보자 ###
## PDF 26p의 과정과 함께 보세용!
## 2-1. Data Matrix 형성
OriginMatrix <- cbind(X, Y, Z) # 1000*3의 Matrix 형성
head(OriginMatrix)

## 2-2. Covariance Matrix 형성
CovMatrix <- cov(OriginMatrix) # cov(): Covariance Matrix를 형성해주는 함수 
CovMatrix

## 2-3. eigen value, eigen vector
lambdaA <- eigen(CovMatrix) # eigen(): Eigen Value와 Eigen Vector를 형성해주는 함수 
lambdaA # value와 vector를 확인하려면 각각 lambdaA$values, lambdaA$vectors
## Values의 component와 vectors의 열이 일대일 대응된다.

## 2-4. 새로운 data set 생성(OriginMatrix의 선형 결합)
TransformedMatrix <- OriginMatrix %*% lambdaA$vectors
head(TransformedMatrix)

## 2-5. Dataset 선형 변환여부 확인
range2 <- c(min(TransformedMatrix[,1]), max(TransformedMatrix[,1]))
plot3d(TransformedMatrix[,1], TransformedMatrix[,2], TransformedMatrix[,3], xlim=range2, ylim=range2, zlim=range2, col='red')
plot3d(X, Y, Z, xlim=range, ylim=range, zlim=range, add=T)
## 차원축소한 것이 아니므로 새로 생성된 변수 TransformedMatrix는 OriginMatrix의 선형 변환 

## 2-6. 변수의 Component Impact 확인 
sd(TransformedMatrix[,1])/(sd(TransformedMatrix[,1])+sd(TransformedMatrix[,2])+sd(TransformedMatrix[,3]))# 0.9030655
sd(TransformedMatrix[,2])/(sd(TransformedMatrix[,1])+sd(TransformedMatrix[,2])+sd(TransformedMatrix[,3]))# 0.07973736
sd(TransformedMatrix[,3])/(sd(TransformedMatrix[,1])+sd(TransformedMatrix[,2])+sd(TransformedMatrix[,3]))# 0.01719718
## 제3변수 Impact가 작으니 제거하고 ploting 
plot(TransformedMatrix[,1], TransformedMatrix[,2]) # 모든 data가 pc1 pc2 에 분포

### 3. 구현한 PCA와 PCA 함수를 비교해보자 ###
## SVD(Singular Value Decomposition) 이용(prcomp와 princomp)
## prcomp(x, retx = TRUE, center = TRUE, scale. = FALSE, tol = NULL, rank. = NULL, ...)
## x : data, retx : 변수축 회전 여부, center=zero:원점 설정 여부, scale : 표준화여부
## prcomp : sdev, rotation(eigenvetors), center, scale, x(principal component)
pca <- prcomp(OriginMatrix, center=T, scale=T) # center=T(데이터 중앙을 0으로), scale=T(분산을 1로)
## 해석: PC1 =  0.70636575 * X + 0.04628937 * Y + 0.70633188 * Z의 선형식을 가지는 변수
pca$sdev
pca$rotation # rotation(eigenvetors)

### 4. dataset으로 전처리 단계에서 주성분분석 ###

### 데이터 확인 ###
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=",")
colnames(wine) <- c("Cvs","Alcohol","Malic acid","Ash","Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline") # 변수명 설정 
str(wine) # 데이터 구조 확인
head(wine) # 데이터 내용 확인 
summary(wine) 
# 13개 변수로 이루어진 178개의 데이터
# 설명변수는 모두 연속형변수
# 종속변수 Y인 Cvs는 factor로 변환해야(Cvs: The type of wine into one of three classes)
unique(wine$Cvs) 
wine$Cvs <- factor(wine$Cvs) # 종속변수 Y인 Cvs는 factor로 변환
pairs(wine[,-1], col = wine$Cvs, upper.panel = NULL, pch = 16, cex = 0.5) # 변수 간 상관성 확인 # upper.panel = NULL은 위쪽 panel을 안보이겠다는 의미 
legend("topright", bty = "n", legend = c("Cv1","Cv2","Cv3"), pch = 16, col = c("black","red","green"),xpd = T, cex = 2, y.intersp = 0.5) # 범례 

## data partition
set.seed(10)
idx <- sample(1:nrow(wine), round(nrow(wine)*0.7), replace = F)
train <- wine[idx, -1] ; test <- wine[-idx, -1] 
train_label <- wine[idx, 1] ; test_label <- wine[-idx, 1]

dev.off() # plot 갱신 
winePCA <- prcomp(scale(train)) # 종속변수를 제외한 데이터 PCA 
plot(winePCA$x[,1:2], col = wine$Cvs)

## 변수의 개수 결정(elbow point & Cumulative Proportion)
plot(winePCA, type="l") # 3~4번째에서 끊으면 되겠다.
summary(winePCA)# Elbow point인 PC4까지로는 74%정도까지 설명할 수 있다. 
min(which(summary(winePCA)[[6]][3,] >= 0.85)) # 누적비율을 원하는 비율까지 올려서 누적설명력이 85% 이상인 주성분의 index 뽑기
## summary(winePCA)[[6]]  =>  Importance of components 부분
## summary(winePCA)[[6]][3,]  =>  Cumulative Proportion 부분

## 기존 data matrix와 Eigenvetor 내적
## 행렬곱을 통해 새로운 주성분을 만드는 과정
## 새로운 주성분 PC의 성분의 구성 값 계산
## rotation이 계수값
## 주성분 구성할때의 pc1 <- a*x1 + b*x2 +... 에서 a,b
trainPRC <- as.matrix(train) %*% winePCA$rotation
testPRC <- as.matrix(test) %*% winePCA$rotation

trainF <- cbind(as.data.frame(trainPRC), train_label)
testF <- cbind(as.data.frame(testPRC), test_label)
colnames(trainF)[14] <- "label"; colnames(testF)[14] <- "label"
str(trainF)

## 만든 주성분으로 회귀분석
fit <- lm(train_label~., family = "binomial", data = trainF)
summary(fit)

## 예측
fit_pred <- predict(fit, type="response", newdata = testF)
test_pred<- round(fit_pred)

table <- table(test_label, test_pred)
table
sum(diag(table))/sum(table) # 96%

