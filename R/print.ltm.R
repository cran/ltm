"print.ltm" <-
function (x, digits = max(3, getOption("digits") - 3), ...) 
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    if (is.matrix(coefs <- coef(x))) {
        cat("Coefficients:\n")
        print(coefs, print.gap = 2, quote = FALSE)
    }
    else cat("No coefficients\n")
    cat("\nLog.Lik:", round(x$log.Lik, 3))
    cat("\n\n")
    invisible(x)
}

