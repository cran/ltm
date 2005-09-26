"print.fscores" <-
function(x, ...){
    if(!inherits(x, "fscores"))
        stop("Use only with 'fscores' objects.\n")
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")
    methodMI <- x$method == "MI"
    cat("\nScoring Method:", if (methodMI) "Multiple Imputation\n" else if (x$method == "EB") "Empirical Bayes\n" else "Component\n")
    if (methodMI) cat("# Imputations:", x$B, "\n")
    cat("\nFactor-Scores for observed response patterns:\n")
    dat <- x$score.dat
    dat[] <- lapply(dat, function(x.) if (is.numeric(x.)) round(x., 3) else x.)
    print(dat)
    cat("\n")
    invisible(x)
}

