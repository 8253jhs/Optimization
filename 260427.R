f <- function(x) x^4 - 2
a <- 1
b <- 2
curve(f, a, 1.6)
abline(h=0, lty=2)
tol <- 10^-10
i <- 1

while(b - a > tol) {
  m <- mean(c(a, b))
  points(m, f(m), col=i, pch=16)
  if(f(a)*f(m) <= 0) {
    b <- m
  } else {
    a <- m
  }
  i <- i + 1
  Sys.sleep(1)
}
points(m, f(m), col="red", pch=16)

(1 + sqrt(5)) / 2 # 황금비

##################################

library(Deriv)
f <- function(x) x^4 - 2
x <- 100 # 초기값
tol <- 10^-10
f_1st <- Deriv(f, "x")

while(abs(f(x)) > tol) {
  x <- x - f(x) / f_1st(x)
  print(x)
}

##################################

install.packages("rootSolve")
f <- function(x) x^4 - 2
curve(f, -2, 2)
abline(h = 0, lty = 2)
uniroot(f, c(-2, 0))$root
uniroot(f, c(0, 2))$root
library(rootSolve)
uniroot.all(f, c(-2, 2), tol=10^-10) #bisection 구현


# secant method