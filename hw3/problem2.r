data       <- read.csv('data/winequality-red.txt', sep=";", header=TRUE)
data.size  <- nrow(data)

{
    cv.size    <- 1/5
    output <- data.frame()
    for (cv in 1:5) {
          ## choose a random sample of data for test and training
          data.cv <- data[sample(data.size, size=(data.size * cv.size)), ]
          lm.wine <- lm(data.cv$quality~., data=data.cv)
          lm.fit  <- predict(lm.wine, newdata=data.cv[,1:12])

          summary((data.cv$quality - lm.fit) - lm.wine$residuals)
          err.rms <- sqrt(sum(lm.wine$residuals * lm.wine$residuals) / length(lm.wine$residuals))
          err.rate <- 1 - sum(data.cv$quality == mapply(round, lm.fit)) / nrow(data.cv)
          output <- rbind(output, data.frame("cv"=cv, "err.rms"=err.rms, "err.rate"=err.rate))
    }

    cat("problem 2.A RMS ERR: ", mean(output$err.rms), "\n")
    cat("problem 2.A TEST ERR: ", mean(output$err.rate), "\n")
}

{
    output <- data.frame()
    for (cv in nrow(data):(length(data) + 2)) {
          ## choose cv rows of data
          data.cv <- head(data, cv)
          lm.wine <- lm(data.cv$quality~., data=data.cv)
          lm.fit  <- predict(lm.wine, newdata=data.cv[,1:12])

          summary((data.cv$quality - lm.fit) - lm.wine$residuals)
          err.rms <- sqrt(sum(lm.wine$residuals * lm.wine$residuals) / length(lm.wine$residuals))
          err.rate <- 1 - sum(data.cv$quality == mapply(round, lm.fit)) / nrow(data.cv)
          output <- rbind(output, data.frame("cv"=cv, "err.rms"=err.rms, "err.rate"=err.rate))
    }

    cat("problem 2.A RMS ERR: ", mean(output$err.rms), "\n")
    cat("problem 2.A TEST ERR: ", mean(output$err.rate), "\n")

    plot(output$cv, output$err.rms, col='blue', type="l", xlab="Number of Observations", ylab="Error Rate", pch=20)
    lines(output$cv, output$err.rate, col='red', pch=20)
    legend("bottomright", legend=c("RMS ERR","TEST ERR"), col=c("blue","red"), lwd=c(1,1))
}
