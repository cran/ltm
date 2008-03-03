`factor.scores.grm` <-
function (object, resp.patterns = NULL, method = c("EB", "EAP", "MI"), B = 5, ...) {
    if (!inherits(object, "grm"))
        stop("Use only with 'rasch' objects.\n")
    betas <- object$coefficients
    fits <- fitted(object, resp.patterns = resp.patterns)
    X <- fits[, -ncol(fits), drop = FALSE]
    nx <- nrow(X)
    p <- length(betas)
    method <- match.arg(method)
    if (method == "MI" && is.null(object$hessian)) {
        warning("object does not have an estimate of the Hessian; the 'EB' method is used instead.\n")
        method <- "EB"
    }
    vals <- lapply(betas, function(x) seq(1, length(x) - 1))
    res <- if (any(is.na(object$X))) {
        data.frame(X)
    } else {
        Obs <- observedFreqs(object, X, vals)
        data.frame(X, Obs = Obs, Exp = fits[, ncol(fits)])
    }
    names(res)[1:p] <- names(betas)
    environment(fscores.g) <- environment()
    res <- fscores.g(betas, X, method)
    out <- list(score.dat = res, method = method, B = B, call = object$call, resp.pats = !is.null(resp.patterns))
    class(out) <- "fscores"
    out
}

