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

ef.data <- function (x) {
    1 - sum(data$quality == round(x)) / data.size
}

l      <- exp(seq(-15,6,by=0.01))
output <- data.frame("lambda"=l)
model  <- lm.ridge(train.data$quality~., data=train.data[,1:11], lambda=l)

train.pred <- as.matrix(train.data[,1:11]) %*% t(coef(model)[,-1]) +  rep(1,train.size) %o% coef(model)[,1]
output     <- cbind(output, data.frame("Train"=apply(train.pred, 2, ef.train)))

test.pred  <- as.matrix(test.data[,1:11]) %*% t(coef(model)[,-1]) +  rep(1,test.size) %o% coef(model)[,1]
output     <- cbind(output, data.frame("Test"=apply(test.pred, 2, ef.test)))

data.pred  <- as.matrix(data[,1:11]) %*% t(coef(model)[,-1]) +  rep(1,data.size) %o% coef(model)[,1]
output     <- cbind(output, data.frame("Data"=apply(data.pred, 2, ef.data)))

d <- melt(output, id="lambda")
p <- ggplot(d, aes(lambda, value, colour=variable)) 
p <- p + geom_line()
p <- p + scale_colour_discrete(name = "")
p <- p + scale_x_continuous(expression(lambda))
p <- p + scale_y_continuous("Error Rate")
print(p)
