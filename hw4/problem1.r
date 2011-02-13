library("class")
library("MASS")

data       <- read.csv('data/winequality-red.txt', sep=";", header=TRUE)
data.size  <- nrow(data)
train.size <- round(3/5 * data.size)
test.size  <- data.size - train.size

train.data <- head(data, train.size)
test.data  <- tail(data, test.size)

err <- function (x) {
    1 - sum(train.data$quality == round(x)) / train.size
}

model <- lm.ridge(train.data$quality~., data=train.data[,1:11], lambda=exp(seq(-15,10,by=0.01)))
preds <- as.matrix(train.data[,1:11]) %*% t(coef(model)[,-1]) +  rep(1,train.size) %o% coef(model)[,1]
train.err <- data.frame(apply(preds, 2, err))
plot(as.numeric(row.names(train.err)), train.err[,1], type='l')
