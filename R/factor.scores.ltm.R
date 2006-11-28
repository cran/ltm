`factor.scores.ltm` <-
function (object, resp.patterns = NULL, method = c("EB", "MI", "Component"), B = 5, 
                                robust.se = FALSE, ...) {
    if (!inherits(object, "ltm"))
        stop("Use only with 'ltm' objects.\n")
    betas <- object$coef
    fits <- fitted(object, resp.patterns = resp.patterns)
    X <- fits[, -ncol(fits), drop = FALSE]
    nx <- nrow(X)
    factors <- object$ltst$factors
    inter <- object$ltst$inter
    quad.z1 <- object$ltst$quad.z1
    quad.z2 <- object$ltst$quad.z2
    p <- nrow(betas)
    q. <- 1 + factors + sum(inter, quad.z1, quad.z2)
    method <- match.arg(method)
    if (any(inter, quad.z1, quad.z2) && method == "Component") {
        warning("In presence of nonlinear terms, Component Scores give biased factor-scores estimates. The MI method is used instead.\n")
        method <- "MI"
    }
    Obs <- if (!is.null(resp.patterns)) {
        ind <- match(apply(X, 1, paste, collapse = ""), apply(object$patterns$X, 1, paste, collapse = ""))
        obs <- rep(0, nrow(X))
        obs[!is.na(ind)] <- object$patterns$obs[ind[!is.na(ind)]]
        obs
    } else 
        object$patterns$obs
    res <- data.frame(X, Obs = Obs, Exp = fits[, ncol(fits)])
    names(res)[1:p] <- rownames(betas)
    if (method == "Component") {
        res$z1 <- colSums(t(X) * betas[, 2])
        if (factors == 2)
            res$z2 <- colSums(t(X) * betas[, 3])
    }
    if (method == "EB" || method == "MI") {
        environment(fscores.l) <- environment()
        res <- fscores.l(betas, X, method)
    }
    out <- list(score.dat = res, method = method, B = B, call = object$call, resp.pats = !is.null(resp.patterns))
    class(out) <- "fscores"
    out
}

