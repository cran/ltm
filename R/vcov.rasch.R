"vcov.rasch" <-
function(object, ...){
    if(!inherits(object, "rasch")) stop("Use only with 'rasch' objects.\n")
    inv.hes <- solve(object$hessian)
    p <- nrow(object$coef)
    nams <- c(paste("beta.", 1:p, sep=""), "beta")
    if(!is.null(constraint <- object$constraint)) nams <- nams[-constraint[, 1]]
    dimnames(inv.hes) <- list(nams, nams)
    inv.hes
}

