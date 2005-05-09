"factor.scores.ltm" <-
function(object, method=c("Component", "EB", "MI"), B=5, robust.se=FALSE, ...){
    if(!inherits(object, "ltm")) stop("Use only with 'ltm' objects.\n")
    betas <- object$coef
    X <- object$patterns$mat
    nx <- nrow(X)
    strct <- ltn.strct(object$ltn.struct)
    factors <- strct$factors
    inter <- strct$inter
    quad.z1 <- strct$quad.z1
    quad.z2 <- strct$quad.z2
    p <- nrow(betas)
    q. <- 1 + factors + sum(inter, quad.z1, quad.z2)
    method <- match.arg(method)
    if (any(unlist(strct[-1])) && method == "Component") {
        warning("In presence of nonlinear terms, Component Scores give biased factor-scores estimates. The MI method is used instead.\n")
        method <- "MI"
    }
    res <- object$patterns$dat
    if(method == "Component") {
        res$z1 <- colSums(t(X) * betas[, 2])
        if(factors == 2) res$z2 <- colSums(t(X) * betas[, 3])
    }
    if(method == "EB" || method == "MI") {
        environment(scores.ML) <- environment()
        res <- scores.ML(betas, X, method)
    }
    out <- list(score.dat=res, method=method, B=B)
    out$call <- object$call
    class(out) <- "fscores"
    out
}

