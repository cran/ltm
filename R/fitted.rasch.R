`fitted.rasch` <-
function (object, resp.patterns = NULL, ...) {
    if (!inherits(object, "rasch"))
        stop("Use only with 'rasch' objects.\n")
    X <- if (is.null(resp.patterns)) {
        data.matrix(object$patterns$X)
    } else {
        if (!is.matrix(resp.patterns) && !is.data.frame(resp.patterns))
            stop("'resp.patterns' should be a matrix or a data.frame.\n")
        resp.patterns <- data.matrix(resp.patterns)
        p <- ncol(object$X)
        if (ncol(resp.patterns) != p)
            stop("the number of items in ", deparse(substitute(object)), " and the columns of 'resp.patterns' do not much.\n")
        check.items <- vector("logical", p)
        for (i in 1:p)
            check.items[i] <- all(unique(resp.patterns[, i]) %in% unique(object$patterns$X[, i]))
        if (!all(check.items)) {
            its <- paste((1:p)[!check.items], collapse = ", ")
            stop("the number of levels in 'resp.patterns' does not much for item(s): ", its, "\n")
        }
        resp.patterns
    }
    colnames(X) <- colnames(object$X)
    mX <- 1 - X
    if (any(na.ind <- is.na(X)))
        X[na.ind] <- mX[na.ind] <- 0
    pr <- probs(object$GH$Z %*% t(object$coef))
    p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
    out <- cbind(X, Exp = round(nrow(object$X) * colSums(object$GH$GHw * t(p.xz)), 3))
    rownames(out) <- 1:nrow(out)
    out
}

