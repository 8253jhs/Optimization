x <- c(0.5, 0.8, 1.1, 2.5)
y <- c(8.5, 7.0, 6.5, 5)
df1 <- data.frame(x, y)
plot(df1, xlab="지하철역까지의 거리",
     ylab="원룸 월 임차료",
     pch=16, cex=2, col="red")
grid()
A <- cbind(1, df1$x)
y <- df1$y
det(t(A) %*% A)
theta_hat <- solve(t(A) %*% A) %*% t(A) %*% y
theta_hat
theta0 <- theta_hat[1]
theta1 <- theta_hat[2]
abline(a=theta0, b=theta1, lwd=2, col="blue")



x <- c(0.5, 0.8, 1.1, 2.5)
y <- c(8.5, 7.0, 6.5, 5)
df1 <- data.frame(x, y)
plot(df1, xlab="지하철역까지의 거리",
     ylab="원룸 월 임차료",
     pch=16, cex=2, col="red")
grid()

reg1 <- lm(y ~ x, data = df1) # (출력변수 ~ 입력변수)
reg1$coefficients
abline(reg1, col="blue")



x <- c(1, 2, 3)
y <- c(1, 1, 3)
df1 <- data.frame(x, y)
df1 <- data.frame(x, y)
plot(df1, pch=16, cex=2, col="red")
grid()

reg1 <- lm(y ~ x+0, data = df1) # (출력변수 ~ 입력변수)
reg1$coefficients
abline(reg1, col="blue")



i <- 1:30
x <- (i-1)/29
epsilon <- 0.15*sin(10*i^3)
y <- 2*x+1+epsilon
df2 <- data.frame(x, y)
plot(df2, pch=16, col="red")
grid()
reg2 <- lm(y ~ x, data = df2)
abline(reg2, col="blue")
predict(reg2, data.frame(x=c(1.1,1.2)))
mean(df2$x)
abline(v = mean(df2$x), col="green")
abline(h = mean(df2$y), col="green")
