"factor.scores.rasch" <-
function (object, method = c("EB", "MI"), B = 5, robust.se = FALSE, 
    ...) 
{
    betas <- object$coef
    X <- object$patterns$mat
    nx <- nrow(X)
    p <- nrow(betas)
    method <- match.arg(method)
    res <- object$patterns$dat
    environment(scores.MLr) <- environment()
    res <- scores.MLr(betas, X, method)
    out <- list(score.dat = res, method = method, B = B)
    class(out) <- "fscores"
    out
}

