df1 <- data.frame(x=c(1,3),
                  y=as.factor(c(-1,1)))
plot(df1$x, c(0, 0), col=c("blue", "red"), pch=16, cex=2)
abline(h=0)
install.packages("e1071")
library(e1071)
svm1 <- svm(y ~ x, data = df1,
            kernel = "linear",
            scale = F)
svm1
coef(svm1)
predict(svm1, data.frame(x=4))

###############################################

df2 <- data.frame(x1=c(1, 2, 4), x2=c(1, 2, 4),
                  y=as.factor(c(-1, -1, 1)))
plot(df2[,1:2], col=c("blue", "blue", "red"),
     pch=16, cex=3)
svm2 <- svm(y ~ x1 + x2, data = df2,
           kernel="linear", scale=F)
svm2
svm2$SV
coef(svm2)
plot(svm2, df2)

############################################

df3 <- expand.grid(x1=c(-1,1), x2 = c(-1,1))
df3 <- rbind.data.frame(df3, 3*df3)
df3$y <- as.factor(rep(c(-1, 1), each=4))
plot(df3[,-3], pch=16, cex=2,
     col=rep(c("blue", "red"), each=4))

svm_linear <- svm(y ~ x1+x2, data = df3,
                  kernel = "linear", scale = F)
plot(svm_linear, df3)


svm_poly <- svm(y ~ x1+x2, data = df3,
                  kernel = "poly", scale = F)
plot(svm_poly, df3)


svm_sig <- svm(y ~ x1+x2, data = df3,
                  kernel = "sigmoid", scale = F)
plot(svm_sig, df3)


svm_radial <- svm(y ~ x1+x2, data = df3,
                  kernel = "radial", scale = F)
plot(svm_radial, df3)
