"print.aov.rasch" <-
function(x, digits = 3, ...){
    if(!inherits(x, "aov.rasch"))
        stop("Use only with 'aov.rasch' objects.\n")
    if (!is.null(x$LRT)) {
        p.val <- round(x$p.value, 3)
        p.val <- if(p.val < 0.001) "<0.001" else p.val
        dat <- data.frame(AIC = round(c(x$aic0, x$aic1), 2), BIC = round(c(x$bic0, x$bic1), 2), 
                log.Lik = round(c(x$L0, x$L1), 2), LRT = c(" ", round(x$LRT, 2)), df = c("", x$df), 
                p.value = c("", p.val), row.names = c(x$nam0, x$nam1))
        cat("\n Anova Table\n")
        print(dat)
    }
    else {
        p.val <- round(x$p.value, 3)
        p.val <- if(p.val < 0.001) "<0.001" else p.val
        cat("\nGoodness-of-Fit using Pearson chi-squared\n")
        cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
        cat("Tobs:", round(x$Tobs, 2), "\n")
        cat("# Bootstrap samples:", x$B + 1, "\n")
        cat("p-value:", p.val, "\n")
    }
    cat("\n")
    invisible(x)
}

