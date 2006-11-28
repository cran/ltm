`fitted.grm` <-
function (object, resp.patterns = NULL, ...) {
    if (!inherits(object, "grm"))
        stop("Use only with 'grm' objects.\n")
    betas <- object$coef
    p <- length(betas)
    X <- if (is.null(resp.patterns)) {
        object$patterns$X
    } else {
        if (!is.matrix(resp.patterns) && !is.data.frame(resp.patterns))
            stop("'resp.patterns' should be a matrix or a data.frame.\n")
        resp.patterns <- data.matrix(resp.patterns)
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
    colnames(X) <- names(betas)
    X <- data.matrix(X)
    cpr <- cprobs(betas, object$GH$Z)
    cpr <- lapply(cpr, function (x) log(rbind(x[1, ], diff(x))))
    log.p.xz <- matrix(0, nrow(X), object$control$GHk)
    for (j in 1:p) {
        log.pr <- cpr[[j]]
        log.p.xz <- log.p.xz + log.pr[X[, j], ]
    }
    p.xz <- exp(log.p.xz)
    out <- cbind(X, Exp = round(nrow(object$X) * colSums(object$GH$GHw * t(p.xz)), 3))
    rownames(out) <- 1:nrow(out)
    out
}

