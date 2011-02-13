#library("class")
library("MASS")
library("ggplot2")

data       <- read.csv('data/winequality-red.txt', sep=";", header=TRUE)
data.size  <- nrow(data)
train.size <- round(3/5 * data.size)
test.size  <- data.size - train.size

train.data <- head(data, train.size)
test.data  <- tail(data, test.size)

ef.train <- function (x) {
    1 - sum(train.data$quality == round(x)) / train.size
}

ef.test <- function (x) {
    1 - sum(test.data$quality == round(x)) / test.size
}

model <- lm.ridge(train.data$quality~., data=train.data[,1:11], lambda=exp(seq(-15,6,by=0.01)))

train.pred <- as.matrix(train.data[,1:11]) %*% t(coef(model)[,-1]) +  rep(1,train.size) %o% coef(model)[,1]
train.err  <- data.frame("err"=apply(train.pred, 2, ef.train))
train.err  <- cbind(train.err, "lambda"=as.numeric(row.names(train.err)))

test.pred <- as.matrix(test.data[,1:11]) %*% t(coef(model)[,-1]) +  rep(1,test.size) %o% coef(model)[,1]
test.err  <- data.frame("err"=apply(test.pred, 2, ef.test))
test.err  <- cbind(test.err, "lambda"=as.numeric(row.names(test.err)))


p <- qplot(lambda, err, data=train.err, geom="line", colour=I("red"))
p + geom_line(mapping=aes(x=lambda, y=err), data=test.err, colour=I("blue"))
