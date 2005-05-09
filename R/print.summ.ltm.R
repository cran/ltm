"print.summ.ltm" <-
function (x, digits = max(3, getOption("digits") - 3), ...){
    if(!inherits(x, "summ.ltm")) stop("Use only with 'summ.ltm' objects.\n")
    cat("\nCall:\n", deparse(x$call), "\n\n", sep="")
    cat("Model Summary:\n")
    model.sum <- data.frame(log.Lik = x$logLik, AIC = x$AIC, BIC = x$BIC, row.names = "")
    print(model.sum)
    cat("\nCoefficients:\n")
    coefs <- x$coef
    p <- x$nitems
    se <- round(coefs[, 2], digits)
    if(any(ind <- is.na(se))) se[ind] <- ""
    z.vals <- round(coefs[, 3], digits)
    if (any(ind <- is.na(z.vals))) z.vals[ind] <- ""
    coefs <- data.frame(value = round(coefs[, 1], digits), std.error = se, 
                z.value = z.vals, row.names = paste(rep(c("(Intercept)", x$ltn), each = p), 1:p, sep = "."))
    print(coefs, digits = digits)
    cat("\nIntegration:\n")
    cat("method: Gauss-Hermite\n")
    cat("quadrature points:", x$control$GHk, "\n")
    cat("\nOptimization:\n")
    cat("Convergence:", x$conv, "\n")
    cat("max(|grad|):", format.pval(x$max.sc, digits = 2, eps = 1e-06), "\n")
    cat("quasi-Newton:", x$control$method, "\n")
    invisible(x)
}

