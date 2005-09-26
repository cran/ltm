"fitted.rasch" <-
function(object, resp.patterns = NULL, ...){
    if(!inherits(object, "rasch"))
        stop("Use only with 'rasch' objects.\n")
    X <- if(is.null(resp.patterns)){
        x <- object$patterns$X
        colnames(x) <- colnames(object$X)
        x
    } else
        resp.patterns
    if(ncol(X) != nrow(object$coef))
        stop("the number of items in ", deparse(substitute(object)), " and the columns of ", 
                deparse(substitute(resp.patterns)), " do not much.\n")
    X <- data.matrix(X)
    mX <- 1 - X
    if(any(na.ind <- is.na(X)))
        X[na.ind] <- mX[na.ind] <- 0
    pr <- probs(object$GH$Z %*% t(object$coef))
    p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
    out <- cbind(X, Exp = round(nrow(object$X) * colSums(object$GH$GHw * t(p.xz)), 3))
    rownames(out) <- 1:nrow(out)
    out
}

