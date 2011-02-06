require("rpart")

cvtree <- function(data, depth, cv) {
    data.size  <- nrow(data)
    train.size <- 2/3          ## the proportion of data to use for training
    test.size  <- 1/3          ## the proportion of data to use for test
    err        <- data.frame()

    for (i in 1:cv) {
        ## choose a random sample of data for test and training
        train <- data[sample(data.size, size=(data.size * train.size)), ]
        test  <- data[sample(data.size, size=(data.size * test.size)), ]

        ## data
        train.d <- train[,1:11]
        test.d  <- test[,1:11]

        ## target
        train.t <- train$quality
        test.t  <- test$quality

        fit <- rpart(train.t~., train.d, control=rpart.control(maxdepth=depth))
        err.train <- 1 - sum(train.t  == predict(fit, train.d, type="class")) / length(train.t)
        err.test  <- 1 - sum(test.t   == predict(fit, test.d,  type="class")) / length(test.t)
        err <- rbind(err, data.frame("depth"=depth, "cv"=cv, "err.train"=err.train, "err.test"=err.test))
    }
    return(data.frame("depth"=depth, "cv"=cv, "err.train"=mean(err$err.train), "err.test"=mean(err$err.test)))
}

data         <- read.csv('data/winequality-red.txt', sep=";", header=TRUE)
levels       <- c( "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
data$quality <- factor(data$quality, levels=levels)
CV           <- 10

results = data.frame()
for (d in 1:20) {
    results <- rbind(results, cvtree(data, d, CV))
}

print(results)
