
findsplits <- function(h, t, r) {
    if (length(t) > 0) {
        n <- t[1]
        if (h == n) {
            u <- unique(t)
            if (length(u) > 1) {
                n <- u[2]
            }
            else
                return(r)
        }
        return(findsplits(t[1], tail(t, -1), c(r, ((n - h) / 2) + h)))
    }
    return(r)
}

cerror <- function(df) {
    e1 <- nrow(subset(df, Target == 1)) / nrow(df)
    e2 <- nrow(subset(df, Target == 0)) / nrow(df)
    return(1 - max(e1, e2))
}

centropy <- function(df) {
    e1 <- nrow(subset(df, Target == 1)) / nrow(df)
    e2 <- nrow(subset(df, Target == 0)) / nrow(df)
    t <- ((-1 * e1) * log2(e1) - (e2 * log2(e2)))
    if (is.nan(t))
      return(0)

    return(t)
}

cgini <- function(df) {
    e1 <- nrow(subset(df, Target == 1)) / nrow(df)
    e2 <- nrow(subset(df, Target == 0)) / nrow(df)
    return(1 - e1^2 - e2^2)
}

d <- read.csv('data/table4_8pg199.txt', header=TRUE)
sorted <- d[order(d[4]), ]
scount <- nrow(sorted)
pentropy <- centropy(sorted)
split <- findsplits(sorted$a3[1], tail(sorted$a3, -1), vector())

error <- data.frame()
gini <- data.frame()
entropy <- data.frame()

for (s in split) {

    ## split the dataframe at the specified pt
    le <- subset(sorted, a3 <= s)
    gt <- subset(sorted, a3 >  s)

    lecount <- nrow(le)
    gtcount <- nrow(gt)

    ## classification error
    lr <- lecount / scount
    gr <- gtcount / scount

    leerr <- cerror(le)
    gterr <- cerror(gt)

    err <- (lr * leerr) + (gr * gterr)
    error <- rbind(error, data.frame("split"=s, "error"=err, "le err"=leerr, "gt err"=gterr))

    ## entropy
    lentropy <- centropy(le)
    gentropy <- centropy(gt)
    gain     <- pentropy - (((lentropy * lecount) / scount) + ((gentropy * gtcount) / scount))
    entropy  <- rbind(entropy, data.frame("split"=s, "le"=lentropy, "gt"=gentropy, "gain"=gain))

    ## gini
    lgini <- cgini(le)
    ggini <- cgini(gt)
    sgini <- ((lecount/scount) * lgini) + ((gtcount/scount) * ggini)
    gini  <- rbind(gini, data.frame("split"=s, "le"=lgini, "gt"=ggini, "gini"=sgini))

}

cat("Data:\n")
print(d)

cat("\nMisclassification error:\n")
print(error)

cat("\nParent entropy:  ", pentropy, "\n")
cat("\nSplit entropy:\n")
print(entropy)

cat("\nGini:\n")
print(gini)
