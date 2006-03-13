"print.rcor.test" <-
function (x, digits = max(3, getOption("digits") - 4), ...) {
    mat <- x$cor.mat
    mat[lower.tri(mat)] <- x$p.values[, 3]
    mat <- round(mat, digits)
    low.part <- format.pval(mat[lower.tri(mat)], eps = 1/10^digits)
    upp.part <- format(mat[upper.tri(mat)])
    mat[lower.tri(mat)] <- low.part
    mat[upper.tri(mat)] <- upp.part
    diag(mat) <- "****"
    print(noquote(mat))
    cat("\n\n")
    invisible(x)
}

