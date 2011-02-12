data       <- read.csv('data/winequality-red.txt', sep=";", header=TRUE)

data.size  <- nrow(data)
train.size <- round(3/5 * data.size)
test.size  <- data.size - train.size

train.data <- head(data, train.size)
test.data  <- tail(data, test.size)

lmensemble <- function(n) {
    indiv <- data.frame()
    ensemble.train <- data.frame("quality"=train.data$quality)
    ensemble.test <- data.frame("quality"=test.data$quality)

    for (e in 1:10) {
        columns <- sample(1:11, size=n)

        sample.train <- train.data[, columns]
        sample.train["quality"] <- train.data$quality

        sample.test <- test.data[, columns]
        sample.test["quality"] <- test.data$quality

        model <- lm(sample.train$quality~., data=sample.train[,1:n])

        p.train <- predict(model, newdata=sample.train[,1:n])
        p.test  <- predict(model, newdata=sample.test[,1:n])

        err.train <- 1 - sum(sample.train$quality == mapply(round, p.train)) / train.size
        err.test  <- 1 - sum(sample.test$quality == mapply(round, p.test)) / test.size
        err.rms   <- sqrt(sum(model$residuals * model$residuals) / length(model$residuals))

        indiv   <- rbind(indiv, data.frame("rms"=err.rms, "train"=err.train, "test"=err.test))
        ensemble.test  <- cbind(ensemble.test, e=p.test)
        ensemble.train <- cbind(ensemble.train, e=p.train)
    }

    model <- lm(ensemble.train$quality~., data=ensemble.train[,1:10])
    p.train <- predict(model, newdata=ensemble.train[,1:10])
    p.test  <- predict(model, newdata=ensemble.test[,1:10])

    err.rms   <- sqrt(sum(model$residuals * model$residuals) / length(model$residuals))
    err.train <- 1 - sum(ensemble.train$quality == mapply(round, p.train)) / train.size
    err.test  <- 1 - sum(ensemble.test$quality == mapply(round, p.test)) / test.size

    i.mean.rms   <- mean(indiv$"rms")
    i.mean.train <- mean(indiv$"train")
    i.mean.test  <- mean(indiv$"test")

    return(data.frame(err.rms, err.train, err.test, i.mean.rms, i.mean.train, i.mean.test))
}

{
    results <- lmensemble(5)
    str(results)
    print("Problem 3.A:")
    print(results[4:6])

    print("\n\nProblem 3.B:")
    print(results[1:3])
}

{
    print("\n\nProblem 3.C:")
    results <- data.frame()
    for (n in 2:11) {
        results <- rbind(results, data.frame(n, lmensemble(n)))
    }

    print(results)
    str(results)

    png("hw_3_problem_3_c.png")
    plot(results$n, results$err.rms, type="l", xlab="Number of Columns", ylab="Error Rate", col="blue", pch=20)
    lines(results$n, results$err.train, col="red", pch=20)
    lines(results$n, results$err.test, col="green", pch=20)
    legend("topright", legend=c("RMS", "Train", "Test"), col=c("blue", "red", "green"), lwd=c(1,1))
    dev.off()
}
