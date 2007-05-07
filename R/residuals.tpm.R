`residuals.tpm` <-
function (object, resp.patterns = NULL, order = TRUE, ...) {
    if (!inherits(object, "tpm"))
        stop("Use only with 'tpm' objects.\n")
    fits <- fitted(object, resp.patterns = resp.patterns)
    X <- fits[, -ncol(fits), drop = FALSE]
    Exp <- fits[, "Exp"]
    vals <- lapply(1:ncol(X), function (i) c(0,1))
    Obs <- observedFreqs(object, X, vals)
    out <- cbind(X, Obs = Obs, Exp = Exp, Resid = round((Obs - Exp) / sqrt(Exp), 3))
    if (order)
        out <- out[order(out[, "Resid"]), ]
    out
}

