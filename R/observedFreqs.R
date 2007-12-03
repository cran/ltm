`observedFreqs` <-
function (object, Y, vals) {
    if (!class(object) %in% c("grm", "ltm", "rasch", "tpm"))
        stop("'object' must inherit from either class 'grm', class 'ltm', class 'rasch' or class 'tpm'.\n")
    ncatg <- sapply(vals, length)
    X <- cbind(object$patterns$X, Obs <- object$patterns$obs)
    outX <- apply(X, 1, function (x) {
        nx <- length(x)
        if (!any(na.ind <- is.na(x[-nx])))
            return(x)
        else {
            xx <- x[-nx]
            res <- matrix(100, prod(ncatg[na.ind]), nx - 1)
            nr <- nrow(res)
            res[rep(!na.ind, each = nr)] <- rep(xx[!na.ind], each = nr)
            res[rep(na.ind, each = nr)] <- as.matrix(expand.grid(vals[na.ind]))
            cbind(res, x[nx])
        }
    })
    outX <- if (is.matrix(outX)) t(outX) else do.call(rbind, outX)
    XX <- apply(outX[, -ncol(X)], 1, paste, collapse = "")
    nrY <- nrow(Y)
    Y <- cbind(Y, Ind = 1:nrY)
    outY <- apply(Y, 1, function (y) {
        ny <- length(y)
        if (!any(na.ind <- is.na(y[-ny])))
            return(y)
        else {
            yy <- y[-ny]
            res <- matrix(100, prod(ncatg[na.ind]), ny - 1)
            nr <- nrow(res)
            res[rep(!na.ind, each = nr)] <- rep(yy[!na.ind], each = nr)
            res[rep(na.ind, each = nr)] <- as.matrix(expand.grid(vals[na.ind]))
            cbind(res, y[ny])
        }
    })
    outY <- if (is.matrix(outY)) t(outY) else do.call(rbind, outY)
    if (is.null(colnames(outY)))
        colnames(outY) <- c(paste("It", seq(1, ncol(outY) - 1)), "Ind")
    Obs. <- outX[, ncol(outX)]
    obs <- numeric(nrY)
    for (i in 1:nrY) {
        YY <- outY[outY[, "Ind"] == i, -ncol(outY), drop = FALSE]
        ind <- match(apply(YY, 1, paste, collapse = ""), XX)
        obs[i] <- sum(Obs.[ind[!is.na(ind)]])
    }
    obs
}

