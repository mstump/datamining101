require("reldist")

d <- read.csv('data/table4_8pg199.txt', header=TRUE)
sorted <- d[order(d[4]), ]
print(sorted)

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

split <- findsplits(sorted$a3[1], tail(sorted$a3, -1), vector())

error <- data.frame()
for (s in split) {
    le <- subset(sorted, a3 <= s)
    gt <- subset(sorted, a3 >  s)

    lr <- nrow(le) / nrow(sorted)
    gr <- nrow(gt) / nrow(sorted)

    leerr <- cerror(le)
    gterr <- cerror(gt)

    err <- (lr * leerr) + (gr * gterr)

    error <- rbind(error, data.frame("split"=s, "error"=err, "le err"=leerr, "gt err"=gterr))
}
print(error)
