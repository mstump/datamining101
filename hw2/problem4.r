require("rpart")

training   <- read.csv('data/sonar_train.txt', header=FALSE)
training.d <- training[,1:60]
training.t <- as.factor(training[,61])

test       <- read.csv('data/sonar_test.txt', header=FALSE)
test.d     <- test[,1:60]
test.t     <- as.factor(test[,61])

tree <- rpart(training.t~.,
              data=training.d,
              control=rpart.control(minsplit=0, minbucket=0, cp=-1,
                maxcompete=0, maxsurrogate=0, usesurrogate=0,
                xval=0, maxdepth=5))

cat("Error Rate: ", 1 - sum(test.t == predict(tree, test.d, type="class")) / length(test.t), "\n")
