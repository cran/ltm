"print.rasch" <-
function (x, digits = max(3, getOption("digits") - 3), ...){
    if(!inherits(x, "rasch")) stop("Use only with 'rasch' objects.\n")
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    if(is.matrix(coefs <- coef(x))) {
        cat("Coefficients:\n")
        print(round(coefs, 3), print.gap = 2, quote = FALSE)
    } else cat("No coefficients\n")
    cat("\nLog.Lik:", round(x$log.Lik, 3))
    cat("\n\n")
    invisible(x)
}

