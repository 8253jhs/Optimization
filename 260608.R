f <- function(x) x^2
library(Deriv)
f_prime_x <- Deriv(f, "x")
grad <- function(x)
  return(f_prime_x(x))

x <- 4
v <- 0
eta <- 0.1
beta <- 0.9

for(i in 1:1000) {
  cat(x, " ")
  g <- grad(x)
  v <- beta*v - eta*g
  x <- x + v
  cat(g, v, x, "\n")
}

############################

f <- function(x, y)
  1/2*(x^2+100*y^2)
f_prime_x <- Deriv(f, "x")
f_prime_y <- Deriv(f, "y")
grad <- function(x, y)
  return(c(f_prime_x(x, y),
           f_prime_y(x, y)))
x <- 4; y <- 1
theta <- c(x, y)
v <- 0
eta <- 0.1
beta <- 0.9
for(i in 1:200) {
  theta_tilde <- theta + beta*v
  g <- grad(theta_tilde[1], theta_tilde[2])
  v <- beta*v - eta*g
  theta <- theta + v
}
round(theta, 3)

########################################

f <- function(x) x^2
f_prime_x <- Deriv(f, "x")
grad <- function(x) return(f_prime_x(x))
x <- 4
eta <- 0.1
beta1 <- 0.9
beta2 <- 0.999
m <- v <- 0
epsilon <- 10^-8
for(k in 1:2000) {
  cat(x, " ")
  g <- grad(x)
  m <- beta1*m + (1-beta1)*g
  v <- beta2*v + (1-beta2)*g*g
  m_hat <- m/(1-beta1^k)
  v_hat <- v/(1-beta2^k)
  x <- x - eta*m_hat/(sqrt(v_hat)+epsilon)
  cat(g, m_hat, sqrt(v_hat), "\n")
}

##########################################

i <- 1:30
x <- (i-1)/29
e <- 0.15*sin(10*i^3)
y <- 2*x+1+e
df <- data.frame(x, y)
plot(df, pch=16, col="blue", cex=1.2)
OLS <- lm(y ~ x, data = df)
coef(OLS)
abline(OLS, lwd=2, col="red")

A <- cbind(1, df$x)
y <- as.matrix(df$y)
n <- nrow(A)
theta <- c(0, 0)
eta <- 0.05
beta1 <- 0.9
beta2 <- 0.999
m <- v <- 0
epsilon <- 10^-8

for(k in 1:1500) {
  g <- t(A) %*% (A %*% theta - y)/n
  m <- beta1*m + (1-beta1)*g
  v <- beta2*v + (1-beta2)*g*g
  m_hat <- m/(1-beta1^k)
  v_hat <- v/(1-beta2^k)
  theta <- theta - eta*m_hat/(sqrt(v_hat)+epsilon)
}
theta
abline(a=theta[1], b=theta[2], lwd=2, col="darkgreen")
