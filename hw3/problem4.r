library("MASS")
data    <- read.csv('data/winequality-red.txt', sep=";", header=TRUE)

rr <- lm.ridge(data$quality~., data=data[,1:12], lambda=seq(1, 10000, 100))
matplot(rr$lambda, t(rr$coef/rr$scales), type="l")
