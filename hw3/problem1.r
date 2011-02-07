data       <- read.csv('data/winequality-red.txt', sep=";", header=TRUE)
data.train <- head(data, 1400)
data.test  <- head(data, 199)

{
    print("Partial data set:")
    lm.wine <- lm(data.train$quality~., data=data.train)
    lm.fit  <- predict(lm.wine, newdata=data.test[,1:12])

    err.rms <- sqrt(sum(lm.wine$residuals * lm.wine$residuals) / length(lm.wine$residuals))
    cat("rms err: ", err.rms, "\n")
    cat("err rate: ", 1 - sum(data.test$quality == mapply(round, lm.fit)) / nrow(data.test), "\n")
}

{
    print("Whole data set:")
    lm.wine <- lm(data$quality~., data=data)
    lm.fit  <- predict(lm.wine, newdata=data[,1:12])

    summary((data$quality - lm.fit) - lm.wine$residuals)
    err <- sqrt(sum(lm.wine$residuals * lm.wine$residuals) / length(lm.wine$residuals))
    cat("rms err: ", err, "\n")
    cat("err rate: ", 1 - sum(data$quality == mapply(round, lm.fit)) / nrow(data), "\n")
}
