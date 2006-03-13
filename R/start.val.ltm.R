"start.val.ltm" <-
function (X, factors, formula) {
    n <- dim(X)[1]
    p <- dim(X)[2]
    Z <- as.data.frame(lapply(1:factors, rnorm, n = n, sd = 1.5))
    names(Z) <- paste("z", 1:factors, sep = "")
    cf <- paste(formula[3])
    form <- paste("y ~ ", paste("z", 1:factors, collapse = " + ", sep = ""), " + ", cf)
    form <- as.formula(form)
    q. <- length(attr(terms(form), "term.labels"))
    coefs <- matrix(0, p, q. + 1)
    for (i in 1:p) {
        Z$y <- X[, i]
        coefs[i, ] <- glm(form, family = binomial, data = Z)$coef
    }
    dimnames(coefs) <- NULL
    coefs
}

