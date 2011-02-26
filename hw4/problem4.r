library("klaR")
library("class")
library("ggplot2")

train   <- read.csv('data/sonar_train.txt', header=FALSE)
train.d <- train[,1:60]
train.t <- as.factor(train[,61])
train.size <- nrow(train.d)

test       <- read.csv('data/sonar_test.txt', header=FALSE)
test.d     <- test[,1:60]
test.t     <- data.frame(class=as.factor(test[,61]))
test.size  <- nrow(test.d)

comb.d <- rbind(train, test)[,1:60]
comb.t <- data.frame(class=as.factor(rbind(train, test)[,61]))
comb.size <- nrow(comb.d)

m <- NaiveBayes(train.t~., data=train.d)
br <- predict(m, test.d[,1:60])
berr <- 1 - sum(test.t$class == br$class) / test.size

koutput <- data.frame()
for (k in 2:50) {
    pred <- knn.cv(comb.d, comb.t$class, k=k)
    err  <- 1 - sum(comb.t$class == pred) / comb.size
    koutput <- rbind(koutput, data.frame(k=k, err=err))
}

kd <- melt(koutput, id="k")
range <- c(kd$value, berr)
ymin <- min(range)
ymax <- max(range)

kp <- ggplot(kd, aes(k, value, colour=variable, ymin=ymin, ymax=ymax))
kp <- kp + geom_line()
kp <- kp + opts(legend.position="none")
kp <- kp + scale_y_continuous("Error Rate")
kp <- kp + geom_abline(data=data.frame(err=mean(berr), slope=c(0)), aes(intercept=err, slope=slope, colour="Bayes.Error"))

png("problem4.png")
print(kp)
dev.off()
