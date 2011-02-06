require("rpart")

# train the filter
training_data <- read.csv('data/table4_8pg199.txt', header=TRUE)
training_data[2] <- as.factor(training_data[,2])
training_data[3] <- as.factor(training_data[,3])
target <- as.factor(training_data[,5])

tree <- rpart(target~., data=training_data[2:4], control=rpart.control(minsplit=0, minbucket=0, cp=-1, maxcompete=0, maxsurrogate=0, usesurrogate=0, xval=0, maxdepth=3))

# plot the tree
plot(tree)
text(tree)

predict_data <- read.csv('data/problem2_data.csv', header=TRUE)
predict_data[2] <- as.factor(predict_data[,2])
predict_data[3] <- as.factor(predict_data[,2])

prediction <- predict(tree, predict_data[2:4], type="class")
print(prediction)
