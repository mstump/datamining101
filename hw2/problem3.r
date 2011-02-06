
cerror <- function(df) {
    e1 <- nrow(subset(df, Label == 1)) / nrow(df)
    e2 <- nrow(subset(df, Label == 0)) / nrow(df)
    return(1 - max(e1, e2))
}

centropy <- function(df) {
    e1 <- nrow(subset(df, Label == 1)) / nrow(df)
    e2 <- nrow(subset(df, Label == 0)) / nrow(df)
    t <- ((-1 * e1) * log2(e1) - (e2 * log2(e2)))
    if (is.nan(t))
      return(0)

    return(t)
}

cgini <- function(df) {
    e1 <- nrow(subset(df, Label == 1)) / nrow(df)
    e2 <- nrow(subset(df, Label == 0)) / nrow(df)
    return(1 - e1^2 - e2^2)
}

data <- read.csv('data/problem3_data.csv', header=TRUE)
tcount <- nrow(data)
pentropy <- centropy(data)

error <- data.frame()
entropy <- data.frame()
gini <- data.frame()

for (s in c("A", "B")) {
    ## split on specified column
    l <- subset(data, eval(parse(text=paste(s,"==","1"))))
    r <- subset(data, eval(parse(text=paste(s,"==","0"))))

    lcount <- nrow(l)
    rcount <- nrow(r)

    ## classification error
    lr <- lcount / tcount
    rr <- rcount / tcount

    lerr <- cerror(l)
    rerr <- cerror(r)

    err <- (lr * lerr) + (rr * rerr)
    error <- rbind(error, data.frame("split"=s, "error"=err, "l err"=lerr, "r err"=rerr))

    ## entropy
    lentropy <- centropy(l)
    rentropy <- centropy(r)
    gain     <- pentropy - (((lentropy * lcount) / tcount) + ((rentropy * rcount) / tcount))
    entropy  <- rbind(entropy, data.frame("split"=s, "l"=lentropy, "r"=rentropy, "gain"=gain))

    ## gini
    lgini <- cgini(l)
    rgini <- cgini(r)
    sgini <- ((lcount/tcount) * lgini) + ((rcount/tcount) * rgini)
    gini  <- rbind(gini, data.frame("split"=s, "l"=lgini, "r"=rgini, "gini"=sgini))
}

cat("Data:\n")
print(data)

cat("\nMisclassification error:\n")
print(error)

cat("\nParent entropy:  ", pentropy, "\n")
cat("\nSplit entropy:\n")
print(entropy)

cat("\nGini:\n")
print(gini)
