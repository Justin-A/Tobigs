# -----2번-----
# 메모장은 인풋 매트릭스와 같은 차원의 행렬이며, 
# 각 행.열에 메모하는 정보는 현재 행.열까지 만들어진 정사각형의 한 변의 길이이다.
# 인풋 매트릭스의 원소가 0일 경우 해당 행.열의 메모장 정보는 즉시 0이 되며,
# 메모장 행.열의 바로 왼쪽, 위, 대각선 왼쪽 위가 모두 정사각형이 형성되어 있으면(1 이상) 
# 현재 행.열은 여지껏 형성된 정사각형들 중 최소 변의 길이를 갖는 정사각형의 변+1이다.

Large <- function(mat){
  memo <- matrix(0, nrow(mat), ncol(mat))
  memo[1, ] <- mat[1, ]
  memo[, 1] <- mat[, 1]
  for (i in 2:nrow(mat)){
    for (j in 2:ncol(mat)){
      if (mat[i, j] == 1){
        memo[i, j] <- min(memo[i-1, j], memo[i, j-1], memo[i-1, j-1])+1
      } else{
        memo[i, j] <- 0
      }
    }
  }
  return((max(memo))^2)
}

Mat1 <- matrix(c(1, 0, 1, 1, 1,
                 0, 0, 0, 1, 1,
                 0, 1, 1, 1, 1,
                 0, 1, 1, 1, 1,
                 0, 1, 1, 1, 1),5, 5, byrow=TRUE)
Mat2 <- matrix(c(1, 0, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1,
                 1, 0, 1, 1, 1, 1, 1,
                 0, 1, 1, 1, 1, 1, 1,
                 0, 0, 1, 1, 1, 1, 1), 5, 7, byrow=TRUE)

Large(Mat1); Large(Mat2)

# -----4번-----

chopchop <- function(x){
  X <- x
  N <- length(x) - 1
  memo <- matrix(0, N, N)
  
  for (s in 2:N){
    for (i in 1:(N-s+1)){ # diagonal sequence length
      j <- i+s-1
      memo[i, j] <- Inf
      for (k in i:(j-1)){
        memo[i, j] <- min(memo[i, j], memo[i, k] + 
                            memo[k+1, j] +
                            X[i]*X[k+1]*X[j+1])
      }
    }
  }
  return(memo[1, N])
}

test1 <- c(10, 20, 5, 30, 15)
test2 <- c(10, 20, 5, 30)
test3 <- c(30, 35, 15, 5, 10, 20, 25)
chopchop(test1); chopchop(test2); chopchop(test3)
