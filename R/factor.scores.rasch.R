`factor.scores.rasch` <-
function (object, resp.patterns = NULL, method = c("EB", "EAP", "MI"), B = 5, robust.se = FALSE, ...) {
    if (!inherits(object, "rasch"))
        stop("Use only with 'rasch' objects.\n")
    betas <- object$coef
    fits <- fitted(object, resp.patterns = resp.patterns)
    X <- fits[, -ncol(fits), drop = FALSE]
    nx <- nrow(X)
    p <- nrow(betas)
    method <- match.arg(method)
    vals <- lapply(1:ncol(X), function (i) c(0,1))
    Obs <- observedFreqs(object, X, vals)
    res <- data.frame(X, Obs = Obs, Exp = fits[, ncol(fits)])
    names(res)[1:p] <- rownames(betas)
    environment(fscores.r) <- environment()
    res <- fscores.r(betas, X, method)
    out <- list(score.dat = res, method = method, B = B, call = object$call, resp.pats = !is.null(resp.patterns),
                coef = coef(object))
    class(out) <- "fscores"
    out
}

