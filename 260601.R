f <- function(x, y) 1/2*(x^2+4*y^2)
library(Deriv)
f_prime_x <- Deriv(f, "x")
f_prime_y <- Deriv(f, "y")
grad <- function(x, y)
  c(f_prime_x(x, y),
    f_prime_y(x, y))

# Adagrad
x <- 4; y <- 2
eta <- 1; G <- 0
theta <- c(x, y)
epsilon <- 10^-8
for (i in 1:3) {
  cat(theta, " ")
  g <- grad(theta[1], theta[2])
  G <- G + g*g
  theta <- theta - eta / (sqrt(G) + epsilon) * g
  cat(g, G, theta, "\n")
}

# RMSprop
x <- 4; y <- 2
eta <- 0.1; v <- 0
theta <- c(x, y)
epsilon <- 10^-8
beta <- 0.9
for (i in 1:3000) {
  cat(theta, " ")
  g <- grad(theta[1], theta[2])
  v <- beta * v + (1 - beta)*g*g
  theta <- theta - eta / (sqrt(v) + epsilon) * g
  cat(g, v, theta, "\n")
}

################################################

i <- 1:30
x <- (i-1)/29
e <- 0.15*sin(10*i^3)
y <- 2*x+1+e
df <- data.frame(x, y)
plot(df, pch=16, col="blue", cex=1.2)
OLS <- lm(y ~ x, data=df)
abline(OLS, col="red", lwd=2)

A <- cbind(1, df$x)
y <- as.matrix(df$y)
eta <- 0.1
n <- nrow(A)
iter <- 350

# GD
theta <- c(0, 0)
for (i in 1:iter) {
  g <- 1/n * t(A) %*% (A %*% theta - y)
  theta <- theta - eta * g
}
theta
abline(a=theta[1], b=theta[2], col="darkgreen", lwd=2)

# Adagrad
theta <- c(0, 0)
G <- 0
epsilon <- 10^-8
for (i in 1:iter) {
  g <- 1/n * t(A) %*% (A %*% theta - y)
  G <- G + g*g
  theta <- theta - eta / (sqrt(G)+epsilon) * g
}
theta
abline(a=theta[1], b=theta[2], col="orange", lwd=2)

# RMSprop
theta <- c(0, 0)
v <- 0
epsilon <- 10^-8
beta <- 0.9
for (i in 1:iter) {
  g <- 1/n * t(A) %*% (A %*% theta - y)
  v <- beta * v + (1-beta)*g*g
  theta <- theta - eta / (sqrt(v)+epsilon) * g
}
theta
abline(a=theta[1], b=theta[2], col="purple", lwd=2)
