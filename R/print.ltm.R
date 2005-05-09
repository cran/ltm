"print.ltm" <-
function (x, digits = max(3, getOption("digits") - 3), ...){
    if(!inherits(x, "ltm")) stop("Use only with 'ltm' objects.\n")
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    if(is.matrix(coefs <- coefficients(x))) {
        cat("Coefficients:\n")
        print(round(coefs, 3), print.gap = 2, quote = FALSE)
    } else cat("No coefficients\n")
    cat("\nLog.Lik:", round(x$log.Lik, 3))
    cat("\n\n")
    invisible(x)
}

