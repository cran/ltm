`residuals.grm` <-
function (object, resp.patterns = NULL, order = TRUE, ...) {
    if (!inherits(object, "grm"))
        stop("Use only with 'grm' objects.\n")
    fits <- fitted(object, resp.patterns = resp.patterns)
    X <- fits[, -ncol(fits), drop = FALSE]
    Exp <- fits[, "Exp"]
    betas <- object$coefficients
    vals <- lapply(betas, function(x) seq(1, length(x) - 1)) 
    Obs <- observedFreqs(object, X, vals)
    out <- cbind(X, Obs = Obs, Exp = Exp, Resid = round((Obs - Exp) / sqrt(Exp), 3))
    if (order)
        out <- out[order(out[, "Resid"]), ]
    out
}

