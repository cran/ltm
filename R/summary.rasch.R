"summary.rasch" <-
function (object, robust.se = FALSE, ...) {
    if(!inherits(object, "rasch")) stop("Use only with 'rasch' objects.\n")
    coefs <- object$coef
    coefs <- c(coefs[, 1], coefs[1, 2])
    Var.betas <- if(robust.se) {
        H <- vcov(object)
        S <- ham(object, object$coef, object$X, object$GH)
        H %*% S %*% H
    } else vcov(object)
    se <- rep(NA, length(coefs))
    ind <- if(!is.null(constraint <- object$constraint)) seq(along=se)[-constraint[, 1]] else seq(along=se)
    se[ind] <- sqrt(diag(Var.betas))
    z.vals <- coefs/se
    coef.tab <- cbind(value = coefs, st.err = se, z.vals)
    out <- list(coefficients = coef.tab, Var.betas = Var.betas)
    out$logLik <- object$log.Lik
    out$AIC <- -2 * object$log.Lik + 2 * length(coefs)
    out$BIC <- -2 * object$log.Lik + log(nrow(object$X)) * length(coefs)
    out$max.sc <- object$max.sc
    out$conv <- object$conv
    out$counts <- object$counts
    out$call <- object$call
    out$control <- object$control
    out$nitems <- ncol(object$X)
    class(out) <- "summ.rasch"
    out
}

