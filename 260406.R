install.packages("pracma")
library(pracma)

f <- function(x) exp(x)
curve(f, -3, 3, lwd = 2)
n <- 1 # 하나씩 증가시켜보기
p <- taylor(f, x0=0, n=n)
p # x의 계수
x <- seq(-3, 3, length.out=1000)
lines(x, polyval(p, x), col=n)

f <- function(x) sin(x)
curve(f, -3, 3, lwd = 2)
n <- 2 # 하나씩 증가시켜보기
p <- taylor(f, x0=0, n=n)
p # x의 계수
x <- seq(-3, 3, length.out=1000)
lines(x, polyval(p, x), col=n)

f <- function(x) (x-1)^2+2
curve(f, -30, 30)
optimize(f, c(-10, 10), maximum = F) # 최대화 문제면 T

f <- function(x) 3*x^4-4*x^3-12*x^2+1
curve(f, -3, 3)
optimize(f, c(-3, 3), maximum = F, tol = 10^-10)

library(Deriv)

f <- function(x1, x2) (x1^2)*x2+x1*(x2^3)-x1*x2
H <- function(x1, x2) {
  h11 <- Deriv(f, "x1", n=2)
  h21 <- h12 <- Deriv(Deriv(f, "x1"), "x2")
  h22<- Deriv(f, "x2", n=2)
 return(matrix(c(h11(x1, x2), h12(x1, x2),
           h21(x1, x2), h22(x1, x2)),
         byrow = T, ncol = 2))
}
H(0, 0)
det(H(0, 0))

f <- function(x, y) 
  x^2-x*y+2*y^2-2*x+exp(x+y)
grad <- function(x, y) {
  dx <- Deriv(f, "x")
  dy <- Deriv(f, "y")
  return(c(dx(x, y), dy(x, y)))
}
grad(0, 0)

install.packages("nloptr")
library(nloptr)
f <- function(x, y) 
  x^2-x*y+2*y^2-2*x+exp(x+y)
Deriv(f, "x")
Deriv(f, "y")

obj <- function(x)
  x[1]^2-x[1]*x[2]+2*x[2]^2-2*x[1]+exp(x[1]+x[2]) # 목적함수

# 그래디언트 벡터
grad <- function(x) c(2 * x[1] + exp(x[1] + x[2]) - (2 + x[2]),
                      4 * x[2] + exp(x[1] + x[2]) - x[1])
x_init <- c(-1, -1)
options <- list("algorithm" = "NLOPT_LD_LBFGS",
                "xtol_rel" = 10^-20,
                "maxeval" = 1000)
nloptr(x0 = x_init,
       eval_f = obj,
       eval_grad_f = grad,
       opts = options)
