install.packages("mosaic")
install.packages("manipulate")
library(mosaic)
library(manipulate)

f <- function(x, y) x + 1
plotFun(f(x, y) ~ x & y,
        xlab = "x",
        ylab = "y",
        zlab = "f(x, y)",
        xlim = c(-3, 3),
        ylim = c(-3, 3),
        surface = T,
        alpha = 1)


install.packages("Deriv")
library(Deriv)

f <- function(x, y) x*exp(y)
Deriv(f, "x")
Deriv(Deriv(f, "x"), "x")
Deriv(Deriv(f, "x"), "y")
Deriv(f, "y", n = 2) #y에 대해 두번 편미분

f <- function(w0, w1) (1-(w0+w1 * 0 ))^2 + (2 - W0 + w1 * 1))^2+
Deriv(f, "w0")
Deriv(f, "w1")

