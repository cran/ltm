"descript" <-
function(X, n.print = 10, ...){
    if(!inherits(X, "matrix") && !inherits(X, "data.frame"))
        stop("'X' must be either a data.frame or a matrix")
    nam <- deparse(substitute(X))
    X <- data.matrix(X)
    n <- nrow(X)
    p <- ncol(X)
    perc <- apply(X, 2, table)/n
    ind <- combinations(p, 2)
    nind <- nrow(ind)
    pvals <- numeric(nind)
    for(i in 1:nind)
        pvals[i] <- chisq.test(table(X[, ind[i, 1]], X[, ind[i, 2]]), ...)$p.value
    pw.ass <- data.frame(ind, format.pval(pvals, digits = 1))
    names(pw.ass) <- c("Item i", "Item j", "p.value")
    pw.ass <- pw.ass[order(pvals, decreasing = TRUE), ]
    row.names(pw.ass) <- 1:nind
    itms <- rbind(Freq = table(rowSums(X)))
    out <- list(perc = perc, items = itms, pw.ass = pw.ass, n.print = n.print, name = nam)
    class(out) <- "descript"
    out
}
