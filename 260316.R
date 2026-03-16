1:10
rep(2, 5)
seq(1, 10, 3)
matrix(1:9, nrow=3, ncol=3)
matrix(1:9, nrow=3, ncol=3, byrow=T)
matrix(1:7, nrow=3, ncol=3, byrow=T)
u <- 1:3
v <- 4:6
w <- 7:9
cbind(u, w, v)
rbind(w, v, u)
diag(5)
diag(3, 5)
diag(1:3, 5)
#install.packages(matlab)
library(matlab)
ones(5)
zeros(4)
A <- matrix(1:9, ncol=3)
t(A)


A1 <- matrix(c(1, 1,
               1, -1), nrow=2, byrow=T)
det(A1)
A2 <- cbind(c(1, 2), c(1, 2))
det(A2)
b1 <- c(3, -1)
solve(A1, b1)
b2 <- c(1, 1)
solve(A2, b2)
det(matrix(1:9, ncol=3, byrow=T))
round(det(matrix(1:9, ncol=3, byrow=T)), 3)
det(matrix(c(1, 0, 1,
             1, -1, 1,
             0, 1, 1), nrow=3, byrow=T))

A <- rbind(c(1, 1, 1), 1:3, rep(0, 3))
qr(A)
qr(A)$rank
det(A)

# p=1 (L1 Norm: |x| + |y| = 1) -> 마름모
x1 <- c(1, 0, -1, 0, 1)
y1 <- c(0, 1, 0, -1, 0)

# p=2 (L2 Norm: x^2 + y^2 = 1) -> 원 (삼각함수 이용)
theta <- seq(0, 2 * pi, length.out = 100)
x2 <- cos(theta)
y2 <- sin(theta)

# p=무한대 (L_inf Norm: max(|x|, |y|) = 1) -> 정사각형
x_inf <- c(1, -1, -1, 1, 1)
y_inf <- c(1, 1, -1, -1, 1)

par(mfrow = c(1, 3), pty = "s")

# 플로팅 함수
draw_base_plot <- function(x, y, title, col_border, col_fill) {
  plot(NULL, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5),
       xlab = "x", ylab = "y", main = title)
  abline(h = 0, v = 0, col = "gray50", lty = 1)
  # 다각형 그리기
  polygon(x, y, border = col_border, col = col_fill, lwd = 2)
}

# (1) || v ||_1 = 1 그리기
draw_base_plot(x1, y1, "|| v ||_1 = 1", "red", rgb(1, 0, 0, 0.2))

# (2) || v ||_2 = 1 그리기
draw_base_plot(x2, y2, "|| v ||_2 = 1", "blue", rgb(0, 0, 1, 0.2))

# (3) || v ||_Inf = 1 그리기
draw_base_plot(x_inf, y_inf, "|| v ||_\\infty = 1", "forestgreen", rgb(34/255, 139/255, 34/255, 0.2))


# 크기 비교를 위해 하나의 평면에 겹쳐서 그리기 (참고용)
par(mfrow = c(1, 1), pty = "s")

plot(NULL, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5),
     xlab = "x", ylab = "y", main = "Lp Norm Unit Circles (겹친 형태)")
abline(h = 0, v = 0, col = "gray50")

# 선을 겹쳐서 그리기
lines(x1, y1, col = "red", lwd = 2)
lines(x2, y2, col = "blue", lwd = 2)
lines(x_inf, y_inf, col = "forestgreen", lwd = 2)

# 범례 추가
legend("topright", 
       legend = c("|| v ||_1 = 1", "|| v ||_2 = 1", "|| v ||_\\infty = 1"),
       col = c("red", "blue", "forestgreen"), 
       lwd = 2, bty = "n") # bty="n"은 범례 테두리 제거
