`print.summ.ltm` <-
function (x, digits = max(3, getOption("digits") - 3), ...) {
    if (!inherits(x, "summ.ltm"))
        stop("Use only with 'summ.ltm' objects.\n")
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep="")
    cat("Model Summary:\n")
    model.sum <- data.frame(log.Lik = x$logLik, AIC = x$AIC, BIC = x$BIC, row.names = "")
    print(model.sum)
    cat("\nCoefficients:\n")
    coefs <- x$coef
    print(round(coefs, digits))
    cat("\nIntegration:\n")
    cat("method: Gauss-Hermite\n")
    cat("quadrature points:", x$control$GHk, "\n")
    cat("\nOptimization:\n")
    cat("Convergence:", x$conv, "\n")
    cat("max(|grad|):", format.pval(x$max.sc, digits = 2, eps = 1e-06), "\n")
    cat("quasi-Newton:", x$control$method, "\n")
    cat("\n")
    invisible(x)
}

