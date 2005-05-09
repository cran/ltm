"descript" <-
function(X, n.print=10, ..., print=TRUE){
    X <- data.matrix(X)
    n <- nrow(X)
    p <- ncol(X)
    perc <- round(apply(X, 2, table)/n, 2)
    ind <- subsets(p, 2)
    nind <- nrow(ind)
    pvals <- numeric(nind)
    for(i in 1:nind) pvals[i] <- chisq.test(table(X[, ind[i, 1]], X[, ind[i, 2]]), ...)$p.value
    res <- data.frame(ind, format.pval(pvals, digits = 1))
    names(res) <- c("Item i", "Item j", "p.value")
    if(print){
        cat("\nProportions of positive/negative responses:\n")
        print(perc)
        cat("\nPair-wise Associations:\n")
        res <- res[order(pvals, decreasing = TRUE), ]
        row.names(res) <- 1:nind
        print(res[seq(1, min(n.print, nind)), ])
    } else list(perc=perc, pw.ass=res)
}

