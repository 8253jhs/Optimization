set.seed(11)
x <- c(runif(20,0,6), runif(20,5,9))
y <- rep(c(0,1),each=20)
df <- data.frame(x,y)
df <- df[sample(1:40,40),]
row.names(df) <- 1:40

logireg <- glm(y ~ ., data = df, family = "binomial")
df$y_prob <- predict(logireg, df, type = "response")
df$y_hat <- ifelse(df$y_prob > 0.5, 1, 0)


real <- as.factor(df$y)
pred <- as.factor(df$y_hat)
library(caret)
confusionMatrix(pred, real)



mtcars <- mtcars
