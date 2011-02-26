library("klaR")
library("class")
library("ggplot2")

iris.size <- nrow(iris)
train.size <- round(4/5 * iris.size)
test.size <- round(1/5 * iris.size)

berr <- c()
for (i in 1:50) {
    train.data <- iris[sample(1:iris.size, train.size, replace=TRUE),]
    test.data <- iris[sample(1:iris.size, test.size, replace=TRUE),]

    m <- NaiveBayes(train.data$Species~., data=train.data)
    br <- predict(m, test.data[,1:4])
    berr <- c(berr, 1 - sum(test.data$Species == br$class) / test.size)
}

koutput <- data.frame()
for (k in 2:50) {
    pred <- knn.cv(iris[,1:4], iris$Species, k=k)
    err  <- 1 - sum(iris$Species == pred) / iris.size
    koutput <- rbind(koutput, data.frame(k=k, err=err))
}

kd <- melt(koutput, id="k")
kp <- ggplot(kd, aes(k, value, colour=variable))
kp <- kp + geom_line()
kp <- kp + opts(legend.position="none")
kp <- kp + scale_y_continuous("Error Rate")
kp <- kp + geom_abline(data=data.frame(err=mean(berr), slope=c(0)), aes(intercept=err, slope=slope, colour="Bayes.Error"))

png("problem2.png")
print(kp)
dev.off()
