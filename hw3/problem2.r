data       <- read.csv('data/winequality-red.txt', sep=";", header=TRUE)
data.size  <- nrow(data)

{
    cv.size    <- 1/5
    output <- data.frame()
    for (cv in 1:5) {
          ## choose a random sample of data for test and training
          data.cv <- data[sample(data.size, size=(data.size * cv.size)), ]
          lm.wine <- lm(data.cv$quality~., data=data.cv)

          err.rms <- sqrt(sum(lm.wine$residuals * lm.wine$residuals) / length(lm.wine$residuals))
          lm.fit  <- predict(lm.wine, newdata=data.cv[,1:12])
          err.rate <- 1 - sum(data.cv$quality == mapply(round, lm.fit)) / nrow(data.cv)

          output <- rbind(output, data.frame("cv"=cv, "err.rms"=err.rms, "err.rate"=err.rate))
    }

    cat("problem 2.A RMS ERR: ", mean(output$err.rms), "\n")
    cat("problem 2.A TEST ERR: ", mean(output$err.rate), "\n")
}

{
    output <- data.frame()
    test.size <- 1/5 * data.size
    data.test <- data[sample(data.size, size=test.size), ]

    for (cv in nrow(data):(length(data) + 2)) {
          ## choose cv rows of data
          data.cv <- head(data, cv)
          lm.wine <- lm(data.cv$quality~., data=data.cv)
          err.rms <- sqrt(sum(lm.wine$residuals * lm.wine$residuals) / length(lm.wine$residuals))

          fit.train  <- predict(lm.wine, newdata=data.cv[,1:12])
          fit.test   <- predict(lm.wine, newdata=data.test[,1:12])

          err.train <- 1 - sum(data.cv$quality == mapply(round, fit.train)) / cv
          err.test  <- 1 - sum(data.test$quality == mapply(round, fit.test)) / test.size
          output <- rbind(output, data.frame("cv"=cv, "err.rms"=err.rms, "err.train"=err.train, "err.test"=err.test))
    }

    cat("problem 2.A RMS ERR: ", mean(output$err.rms), "\n")
    cat("problem 2.A TRAIN ERR: ", mean(output$err.train), "\n")
    cat("problem 2.A TEST ERR: ", mean(output$err.test), "\n")

    png("hw_3_problem_2_b.png")
    plot(output$cv, output$err.rms, col='blue', type="l", xlab="Number of Observations", ylab="Error Rate", pch=20)
    lines(output$cv, output$err.test, col='red', pch=20)
    lines(output$cv, output$err.train, col='green', pch=20)
    legend("bottomright", legend=c("RMS ERR","TEST ERR", "TRAIN ERR"), col=c("blue", "red", "green"), lwd=c(1,1))
    dev.off()
}
