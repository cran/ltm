`factor.scores.rasch` <-
function (object, resp.patterns = NULL, method = c("EB", "MI"), B = 5, robust.se = FALSE, ...) {
    if (!inherits(object, "rasch"))
        stop("Use only with 'rasch' objects.\n")
    betas <- object$coef
    fits <- fitted(object, resp.patterns = resp.patterns)
    X <- fits[, -ncol(fits), drop = FALSE]
    nx <- nrow(X)
    p <- nrow(betas)
    method <- match.arg(method)
    Obs <- if (!is.null(resp.patterns)) {
        ind <- match(apply(X, 1, paste, collapse = ""), apply(object$patterns$X, 1, paste, collapse = ""))
        obs <- rep(0, nrow(X))
        obs[!is.na(ind)] <- object$patterns$obs[ind[!is.na(ind)]]
        obs
    } else 
        object$patterns$obs
    res <- data.frame(X, Obs = Obs, Exp = fits[, ncol(fits)])
    names(res)[1:p] <- rownames(betas)
    environment(fscores.r) <- environment()
    res <- fscores.r(betas, X, method)
    out <- list(score.dat = res, method = method, B = B, call = object$call, resp.pats = !is.null(resp.patterns))
    class(out) <- "fscores"
    out
}

