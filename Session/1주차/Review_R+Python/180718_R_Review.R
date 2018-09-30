# http://rfriend.tistory.com/

rm(list=ls())
x <- 1
is.vector(x)

f <- c(1:10); f
f[-c(1, 2)]; f[3:10]; f[c(2,4,6)]
f[f %% 2 == 1]
index = (f %% 2 == 1)
f[index]
subset(f, f %% 2 == 1)
LETTERS[1:10]

seq(0, 10, by=2.5); seq(0, 10, length=10)
rep(c(1,2,3), 2); rep(c(1,2,3), c(1,2,3))

n <- 10; mu <- 0; sigma <- 1; a <- 1; b <- 10; k <- 5
lambda <- 3; replace <- TRUE; p <- 0.5; prob <- rep(0.1, 10)

rnorm(n, mu, sigma) # 정규분포 
runif(n, a, b) # 균일분포
rexp(n, lambda) # 지수분포
rbinom(n, k, prob) # 이항분포
rpois(n, lambda) # 포아송분포
sample(x = f, n, replace, prob) # 임의표본추출

set.seed(123) # 임의추출에 같은 결과를 재현해줌
sample(x = f, n, replace, prob)

n <- 400; x <- rnorm(n, 50, 10)
hist(x, nclass=20, main="normal(50,10) distribution")


M <- matrix(1:10, nrow=4, ncol=3); M # byrow=F 가 디폴트
M[1:3, 2:3]
M[M[,1] %% 2 == 0, ]
apply(M, 1, sum); apply(M, 2, sum)

n <- 1000; p <- 10
M2 <- matrix(runif(n*p), n, p)
M2_t <- t(apply(M2, 1, sort))
head(M2_t[, 1:5])
hist(M2_t[, 2], xlim=c(0,1), main="Second Smallest")

setwd("~/Desktop/Justin/Tobigs/10/1주차/Review_R+Python/")
exams <- read.csv("exams.csv", header=T, encoding = 'utf-8')
str(exams)
colnames(exams)[1] <- 'course.id'
dim(exams); names(exams)
exams[exams$course.id == 10, "final"]
median(exams$mid); median(exams[,2])
is.na(exams$final); median(exams$final); median(exams$final, na.rm=T)

exams$final[is.na(exams$final)] <- 0
with(exams, plot(final ~ mid, pch=20, xlim=c(0,45), ylim=c(0,45),
                 main="exam scores"))
with(exams, cor(mid, final))

book2 <- read.csv("book2.csv", header=T)
str(book2)
summary(book2[, 2:3])
book2$project[is.na(book2$project)] <- 0

class_record <- merge(exams, book2)
str(class_record)
head(class_record, 5)
index <- order(book2$course.id)
book2_sort <- book2[index,]
class_record1 <- cbind(exams, book2_sort[,-4])
head(class_record1, 5)

set.seed(1)
g <- sample(c("A","B"), 100, replace=T)
x <- rnorm(100, 50, 10)
D <- data.frame(grp=g, score=x)
str(D)
head(D)
D[1, "grp"] <- "C"
levels(D$grp) <- c("A","B","C")
D[1, "grp"] <- "C"
head(D,2)

set.seed(1)
alpha <- sample(c("A", "B", "C"), 25, replace=T); f <- factor(alpha)
str(f)
z <- sample(1:5, 25, replace=T); g <- factor(z)
str(g)
df <- data.frame(f=f, z=z)
table(f)
table(f, g)
addmargins(table(f,g))

set.seed(2)
x = round(rnorm(25, 50, 10))
df2 <- data.frame(x, z)
head(df2)
tapply(x, f, min)
tapply(x, f, function(t) max(t)-min(t))

s <- split(df2, f); s
sapply(s, apply, 2, median)

aggregate(df2$x, list(f), sum)
aggregate(df2$x, list(f, g), sum)

x <- runif(100,0,10)
y <- 5 + 0.5*(x-5) + rnorm(100)
x.cut <- cut(x, 0:10)
class(x.cut); head(cbind(x, x.cut))
y.local <- aggregate(y, list(x.cut), mean); y.local

plot(x, y, ylim=c(0,10), main="x vs. y")
segments(0:9, y.local$x, 1:10, y.local$x, lwd=2)
abline(v=1:9, lty="dotted", col="red")

# stack and unstack
if(!require(datasets)) install.packages("datasets")
library(datasets)
data(PlantGrowth)
head(PlantGrowth, 10)
PG_tab <- unstack(PlantGrowth, weight ~ group); PG_tab
PG1 <- stack(PG_tab); head(PG1, 10)
names(PG1) <- c("weight", "group")
identical(PlantGrowth, PG1)

data(Indometh) # pharmacokinetics of indomethacin
Indometh$BP <- round(rnorm(nrow(Indometh), 100, 15))
head(Indometh, 10)
idm_wide <- reshape(Indometh, idvar="Subject",
                         v.names=c("conc","BP"), timevar="time", sep="_", direction="wide")
idm_wide[, c(1:7)]
# idvar = 개체식별 변수
# v.names = 매 시점에서 측정된 변수(들)
# timevar = 시간 변수
# sep = 구분자 (separator)
# direction = "wide"
idm_wide <- data.frame(idm_wide)
idm_long <- reshape(idm_wide, idvar="Subject",
                    varying=colnames(idm_wide)[-1], sep="_", direction="long")
head(idm_long, 12)


if(!require(data.table)) install.packages("data.table"); library(data.table)
n <- 10^6
digits <- as.factor(0:9)
x1 <- sample(digits, n, replace=T)
x2 <- sample(digits, n, replace=T)
x3 <- sample(digits, n, replace=T)
x4 <- sample(digits, n, replace=T)
x5 <- sample(digits, n, replace=T)
x6 <- sample(digits, n, replace=T)
DF <- data.frame(x1, x2, x3, x4, x5, x6, y=rnorm(n))
DT <- data.table(x1, x2, x3, x4, x5, x6, y=rnorm(n))
head(DT, 10); class(DT)
write.table(DF, file='mydata.txt', sep='|', row.names = F, quote = F)
system.time(read.csv('mydata.txt'))
system.time(fread('mydata.txt'))

setkey(DT, x1, x2, x3, x4, x5, x6)
head(DT, 10)
p.time <- proc.time()
DF[x1=="0" & x2=="1" & x3=="2",]
proc.time() - p.time

p.time <- proc.time()
DT[J("0","1","2")]
proc.time() - p.time