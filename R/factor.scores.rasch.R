"factor.scores.rasch" <-
function(object, method = c("EB", "MI"), B = 5, robust.se = FALSE, ...){
    if(!inherits(object, "rasch"))
        stop("Use only with 'rasch' objects.\n")
    betas <- object$coef
    X <- object$patterns$X
    nx <- nrow(X)
    p <- nrow(betas)
    method <- match.arg(method)
    fits <- fitted(object)
    res <- data.frame(X, Obs = object$patterns$obs, Exp = fits[, ncol(fits)])
    names(res)[1:p] <- rownames(betas)
    environment(fscores.r) <- environment()
    res <- fscores.r(betas, X, method)
    out <- list(score.dat = res, method = method, B = B, call = object$call)
    class(out) <- "fscores"
    out
}

