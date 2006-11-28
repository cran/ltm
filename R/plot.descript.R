`plot.descript` <-
function (x, xlab, ylab, ...) {
    if (!inherits(x, "descript"))
        stop("Use only with 'descript' objects.\n")
    levs <- apply(data.matrix(x$data)[complete.cases(x$data), ], 2, function(xx) length(unique(xx)))
    if (any(levs > 2))
        stop("the plot method for 'descript' objects currently works for dichotomous responses.\n")
    tot <- as.vector(rowSums(x$data, na.rm = TRUE))
    lis <- split(x$data, tot)
    lis <- lis[-c(1, length(lis))]
    out <- sapply(lis, colMeans, na.rm = TRUE)
    if (missing(xlab))
        xlab <- "Total Score"
    if (missing(ylab))
        ylab <- "Proportion Correct"    
    matplot(cbind(1:ncol(out)), t(out), xlab = xlab, ylab = ylab, ...)
    invisible(out)
}

