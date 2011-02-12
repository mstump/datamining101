library("MASS")
data    <- read.csv('data/winequality-red.txt', sep=";", header=TRUE)

coef <- data.frame()
for (i in 1:20) {
    exp <- (3 - 4 * (i / 20))
	lam <- 10 ^ exp
    rr <- lm.ridge(data$quality~., data=data[,1:12], lambda=lam)
    coef <- rbind(coef, lm.wine$coef)
    str(lm.wine$coef)
}

str(coef)
print(coef)
