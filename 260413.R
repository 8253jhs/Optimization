# 실습 1 - 4
f <- function(x1, x2, x3)
  x1^4+2*x2^2+3*x3^2-4*x1-4*x2*x3
library(nloptr)
library(Deriv)

Deriv(f, "x3")


obj <- function(x)
  x[1]^4+2*x[2]^2+3*x[3]^2-4*x[1]-4*x[2]*x[3]
grad <- function(x)
  c(4 * x[1]^3 -4,
    4 * x[2] - 4 * x[3],
    6 * x[3] - 4 * x[2])
x_init <- c(3, 3, 3)
options <- list("algorithm" = "NLOPT_LD_LBFGS",
                "xtol_rel" = 10^20,
                "maxeval" = 1000)

p1_4 <- nloptr(x0 = x_init,
             eval_f = obj,
             eval_grad_f = grad,
             opts = options)
p1_4$objective # 목적함수값
round(p1_4$solution)  # 최적해

##################################

f <- function(x1, x2, x3)
  x1^2 + x2^2 + x3^2

# 목적함수와 목적함수의 그래디언트 벡터
eval_f <- function(x)
  list("objective" = x[1]^2 + x[2]^2 + x[3]^2,
       "gradient" = c(2*x[1], 2*x[2], 2*x[3]))

#제약식
eval_g_eq <- function(x)
  list("constraints"=c(x[1]+x[2]+x[3]-6,
                       x[1]+2*x[2]+x[3]-10),
       "jacobian" = rbind(c(1, 1, 1),
                          c(1, 2, 1)))

optimizer <- list("algorithm" = "NLOPT_LD_LBFGS",
                  "xtol_rel" = 10^-30)
options <- list("algorithm" = "NLOPT_LD_AUGLAG",
                "xtol_rel" = 10^-30,
                "maxeval" = 5000,
                "local_opts" = optimizer)
x_init <- c(0, 0, 0)
p2_1 <- nloptr(x0 = x_init,
               eval_f = eval_f,
               eval_g_eq = eval_g_eq,
               opts = options)

p2_1$solution
p2_1$objective

############################

f <- function(x) x^2+(4/x)^2
curve(f, -10, 10)

######################################

# 목적함수와 목적함수의 그래디언트 벡터
eval_f <- function(x)
  list("objective" = x[1]^2 + x[2]^2,
       "gradient" = c(2*x[1], 2*x[2]))

#제약식
eval_g_eq <- function(x)
  list("constraints"=c(x[1]*x[2]-4),
       "jacobian" = rbind(c(x[2], x[1])))

optimizer <- list("algorithm" = "NLOPT_LD_LBFGS",
                  "xtol_rel" = 10^-30)
options <- list("algorithm" = "NLOPT_LD_AUGLAG",
                "xtol_rel" = 10^-30,
                "maxeval" = 5000,
                "local_opts" = optimizer)
x_init <- c(1, 1)
p2_3 <- nloptr(x0 = x_init,
               eval_f = eval_f,
               eval_g_eq = eval_g_eq,
               opts = options)

p2_3$solution
p2_3$objective

#######################################

f <- function(x1, x2) -log(x1 + 1) - x2
Deriv(f, "x2")

# 목적함수와 목적함수의 그래디언트 벡터
eval_f <- function(x)
  list("objective" = -log(x[1]+1)-x[2],
       "gradient" = c(-(1/(1 + x[1])), -1))

# 등식제약식
eval_g_eq <- function(x)
  list("constraints"=c(x[1]^2+x[2]^2-9),
       "jacobian" = rbind(c(2*x[1],2*x[2])))

# 부등식제약식
eval_g_ineq <- function(x)
  list("constraints"=c(2*x[1]+x[2]-3),
       "jacobian" = rbind(c(2,1)))

lb <- c(0, 1)
ub <- c(5, 4)

optimizer <- list("algorithm" = "NLOPT_LD_LBFGS",
                  "xtol_rel" = 10^-30)
options <- list("algorithm" = "NLOPT_LD_AUGLAG",
                "xtol_rel" = 10^-30,
                "maxeval" = 5000,
                "local_opts" = optimizer)
x_init <- c(1, 1)
p3_4 <- nloptr(x0 = x_init,
               eval_f = eval_f,
               eval_g_eq = eval_g_eq,
               eval_g_ineq = eval_g_ineq,
               lb = lb,
               ub = ub,
               opts = options)

p3_4$solution
p3_4$objective

#######################

# 실습 3-5
eval_f <- function(x)
  list(
    "objective" = x[1]^2 + x[2]^2,
    "gradient" = c(2*x[1], 2*x[2]))

# equality constraints & their gradient vectors
eval_g_ineq <- function(x)
  list(
    "constraints" = c(1 - x[1] - x[2],
                      1 - x[1]^2 - x[2]^2,
                      9 - 9*x[1]^2 - x[2]^2,
                      x[2] - x[1]^2,
                      x[1] - x[2]^2),
    "jacobian" = rbind(c(-1, -1),
                       c(-2*x[1], -2*x[2]),
                       c(-18*x[1], -2*x[2]),
                       c(-2*x[1], 1),
                       c(1, -2*x[2])))

lb <- c(-50, -50)
ub <- c(50, 50)
x_init <- c(3,3)

optimizer <- list("algorithm" = "NLOPT_LD_LBFGS",
                  "xtol_rel"  = 1.0e-30)

options <- list("algorithm" = "NLOPT_LD_AUGLAG",
                "xtol_rel"  = 1.0e-30,
                "maxeval"   = 5000,
                "local_opts" = optimizer)

# model and solve problem
p3_5 <- nloptr(x0 = x_init,
               eval_f = eval_f,
               eval_g_ineq = eval_g_ineq,
               lb = lb,
               ub = ub,
               opts = options)

p3_5$objective
p3_5$solution