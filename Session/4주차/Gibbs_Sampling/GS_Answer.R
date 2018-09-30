set.seed(323)
n_burnin = 1e+03
n_simul = 1e+03
x1 = numeric(n_simul)
x2 = numeric(n_simul)

# Burn-in period 동안 샘플링하고 버리기
x1[1] = 0.1; x2[1] = 0.1;
for(i in 2:n_burnin){
  x1[i] = rexp(1, rate = x2[i-1] + 1)
  x2[i] = rexp(1, rate = x1[i] + 1)
}

# 버린 지점부터 본격적으로 sampling
x1[1] = x1[n_burnin]; x2[1] = x2[n_burnin];
for (i in 2:n_simul) {
  x1[i] = rexp(1, rate = x2[i-1] + 1)
  x2[i] = rexp(1, rate = x1[i] + 1)
}

# Traceplot
par(mar=c(3.5,3.5,1.5,1), mgp=c(2.4,0.8,0), las=1, mfrow=c(1,2)) # Graphics Margin 설정
plot(x1, pch=20, xlab="Iteration", type="l"); plot(x2, pch=20, xlab="Iteration", type="l")

# ACF : 시계열에서 사용하는 자기상관계수 확인
# 자기상관계수가 안정적으로 낮아짐을 체크
plot(acf(x1, plot=FALSE), main="Series x1"); plot(acf(x2, plot=FALSE), main="Series x2")

# Plot Samples
par(mar=c(3.5,3.5,1,1), mgp=c(2.4,0.8,0), las=1, mfrow=c(1,2)) # Graphics Margin
plot(x1,x2, pch=20, main="Samples from Gibbs")

library(MASS)
contour(kde2d(x1,x2, lims=c(0,4,0,4)), xlab="x1", ylab="x2", main="Estimated Kernel Density")
