"print.fscores" <-
function (x, ...) 
{
    methodMI <- x$method == "MI"
    cat("\nScoring Method:", if (methodMI) 
        "Multiple Imputation\n"
    else if (x$method == "EB") 
        "Empirical Bayes\n"
    else "Component\n")
    if (methodMI) 
        cat("# Imputations:", x$B, "\n")
    cat("\nFactor-Scores for observed response patterns:\n")
    dat <- x$score.dat
    dat[] <- lapply(dat, function(x.) if (is.numeric(x.)) 
        round(x., 3)
    else x.)
    print(dat)
    invisible(x)
}

