f <- function(x) exp*(2*x-1) + 4*x^2 - x * 100
a <- 0
b <- 4

r <- (-1 + sqrt(5)) / 2
x1 <- b-r*(b-a)
x2 <- a+r*(b-a)
f1 <- f(x1)
f2 <- f(x2)

for(i in 1:4) {
  if(f1 < f2) {
    b <- x2
    x2 <- x1
    f2 <- f1
    x1 <- b-r*(b-a)
    f1 <- f(x1)
  }
  else {
    a <- x1
    x1 <- x2
    f1 <- f2
    x2 <- a-r*(b-a)
    f2 <- f(x2) 
  }
}

############################

f <- function(x1, x2)
  x1^2+x2^2-2*x1+x1*x2+1

library(mosaic)
plotFun(f(x1, x2) ~ x1 & x2,
        x1lab = "x1",
        x2lab = "x2",
        x1.lim = c(-3,5),
        x2.lim = c(-2,2),
        surface = F,
        alpha = 1)
x1 <- x2 <- 5
x <- c(x1, x2)
eta <- 0.1
library(Deriv)
f_pri_x1 <- Deriv(f, "x1")
f_pri_x2 <- Deriv(f, "x2")
grad <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  return(c(f_pri_x1(x1, x2),
           f_pri_x2(x1, x2))
  )
}
for(i in 1:100) {
  x <- x - eta*grad(x)
  print(x)
  Sys.sleep(0.1)
}

############################################

df <- data.frame(x = c(0, 1, 1),
                 y = c(0, 1, 3))
plot(df, pch = 16, cex = 1.5, col="blue")
A <- cbind(1, df$x)
theta <- solve(t(A) %*% A) %*% t(A) %*% df$y
theta0 <- theta[1]
theta1 <- theta[2]
abline(a=theta0, b=theta1, col="red", lwd=2)

Theta <- c(1.5, 3)
eta <- 0.03
for(i in 1:70) {
  abline(a=Theta[1], b=Theta[2], col="green")
  Theta <- Theta - eta*(t(A) %*% (A %*% Theta - df$y))
}
Theta
