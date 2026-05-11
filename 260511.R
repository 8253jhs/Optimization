i <- 1:30
x <- (i-1)/29
epsilon <- 0.15*sin(10*i^3)
y <- 2*x+1+epsilon
df <- data.frame(x, y)
A <- as.matrix(df$x)
y <- df$y

mse_history <- c()   # mse 기록벡터

MB_reg <- function(A, y, 
                   lr = 0.01,     # 학습률
                   epochs = 100,  # 에폭수
                   bs = 10) {     # 미니배치사이즈
  n <- nrow(A); p <- ncol(A)
  A <- cbind(1, A)
  theta <- rep(0, 1+p) # runif(1+p)
  
  for (epoch in 1:epochs) {
    # 데이터를 무작위로 섞음
    shuffle_indices <- sample(1:n)
    A_shuffled <- A[shuffle_indices, ]
    y_shuffled <- y[shuffle_indices]
    
    # 미니배치 단위로 업데이트 수행
    for (start_idx in seq(1, n, by = bs)) {
      # 배치의 끝 인덱스 계산 (데이터 개수를 초과하지 않도록 min 사용)
      end_idx <- min(start_idx + bs - 1, n)
      
      # 현재 미니배치 데이터 추출
      A_batch <- A_shuffled[start_idx:end_idx, ]
      y_batch <- y_shuffled[start_idx:end_idx]
      m <- nrow(A_batch) # 실제배치사이즈 (마지막 배치는 크기가 작을 수 있음)
      
      y_pred <- as.vector(A_batch %*% theta)  # 예측값 (배치전체 행렬 연산)
      error <- y_pred - y_batch  # 오차
      
      # 그래디언트 계산 (배치 내 오차의 평균)
      # A_batch의 전치행렬과 error 벡터를 내적하여 모든 theta의 기울기를 한 번에 구함
      grad <- as.vector((2 / m) * t(A_batch) %*% error)
      theta <- theta - lr * grad   # 가중치 업데이트
    }
    
    # 한 에포크가 끝난 후 전체 데이터에 대한 mse 계산하여 기록
    y_pred_all <- as.vector(A %*% theta)
    mse <- mean((y_pred_all - y)^2)
    mse_history[epoch] <- mse
    
    if (epoch %% 10 == 0)  # 진행 상황 출력 (10 에포크마다)
      cat(sprintf("Epoch: %d, MSE: %.4f\n", epoch, mse))
  }
  return(theta)
}

MB_reg(A, y)
MB_reg(A, y, lr=0.02, epochs = 10000)

coef(lm(y ~ x, data = df))

#########################################################

getwd()
cal <- read.csv("fetch_california_housing.csv", header=T)
names(cal)
A <- as.matrix(cal[, 1:8])
y <- cal$MedHouseVal
MB_reg(A, y, lr = 0.0000001, epochs = 1000, bs = 32)
