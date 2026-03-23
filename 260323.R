install.packages("lpSolve")
library(lpSolve)
obj <- c(1.5, 1)
const <- matrix(c(1, 0,
                  0, 1,
                  1, 1), byrow = T, ncol = 2)
dir <- c("<=", "<=", "<=")  # rep("<=", 3)
rhs <- c(4, 6, 5)
?lp
lp("max", obj, const, dir, rhs)
lp("max", obj, const, dir, rhs)$solution



obj <- c(8000, 6000)
const <- matrix(c(0.2, 0.4,
                  0.5, 0.25), byrow = T, ncol = 2)
dir <- c("<=", "<=")
rhs <- c(700, 1000)
?lp
lp("max", obj, const, dir, rhs)
lp("max", obj, const, dir, rhs)$solution



