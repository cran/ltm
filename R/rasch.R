"rasch" <-
function (data, constraint = NULL, IRT.param = TRUE, start.val = NULL, na.action = NULL, control = list()) {
    cl <- match.call()
    if (!is.data.frame(data) && !is.matrix(data))
        stop("'data' must be either a numeric matrix or a data.frame.\n")
    X <- data.matrix(data)
    if (!all(its <- apply(X, 2, function (x) { x <- x[!is.na(x)]; length(unique(x)) } ) == 2))
        stop("'data' contain more that 2 distinct values for item(s): ", paste(which(!its), collapse = ", "))
    X <- apply(X, 2, function (x) if (all(unique(x) %in% c(1, 0, NA))) x else x - 1)
    oX <- X
    colnamsX <- colnames(X)
    dimnames(X) <- NULL
    if (!is.null(na.action))
        X <- na.action(X)
    n <- nrow(X)
    p <- ncol(X)
    con <- list(iter.qN = 150, GHk = 21, method = "BFGS", verbose = FALSE)
    con[names(control)] <- control
    betas <- if (!missing(start.val) && length(start.val) == p + 1) start.val else rnorm(p + 1)
    if (!is.null(constraint)) {
        if (!is.matrix(constraint) || (nrow(constraint) > p + 1 | ncol(constraint) != 2))
            stop("'constraint' should be a 2-column matrix with at most ", p + 1, " rows (read help file).\n")
        if (any(constraint[, 1] < 1 | constraint[, 1] > p + 1))
            stop("the 1st column of 'constraint' should between 1 and ", p + 1, " (read help file).\n")
        constraint <- constraint[order(constraint[, 1]), , drop = FALSE]
        constraint[, 1] <- round(constraint[, 1])
        betas[constraint[, 1]] <- NA
    }
    pats <- apply(X, 1, paste, collapse = "")
    freqs <- table(pats)
    obs <- as.vector(freqs)
    X <- apply(cbind(names(freqs)), 1, function (x) {
                nx <- nchar(x)
                out <- substring(x, 1:nx, 1:nx)
                out <- out[out != "A"]
                out[out == "N"] <- NA
                out
        })
    X <- as.numeric(t(X))
    dim(X) <- c(length(freqs), p)
    mX <- 1 - X
    if (any(na.ind <- is.na(X)))
        X[na.ind] <- mX[na.ind] <- 0
    GH <- GHpoints(data ~ z1, con$GHk)
    Z <- GH$x
    GHw <- GH$w
    logLik.rasch <- function (betas, constraint) {
        betas <- betas.rasch(betas, constraint, p)
        pr <- probs(Z %*% t(betas))
        p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
        p.x <- rep(c(p.xz %*% GHw), obs)
        -sum(log(p.x))
    }
    score.rasch <- function (betas, constraint) {
        betas <- betas.rasch(betas, constraint, p)
        pr <- probs(Z %*% t(betas))
        p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
        p.x <- c(p.xz %*% GHw)
        p.zx <- p.xz / p.x
        Nt <- GHw * colSums(p.zx * obs)
        scores <- matrix(0, p, 2)
        for (i in 1:p) {
            ind <- !na.ind[, i]
            rit <- if (all(ind)) GHw * colSums(p.zx * X[, i] * obs) else GHw * colSums(p.zx[ind, ] * X[ind, i] * obs[ind])
            scores[i, ] <- -c(crossprod(rit - pr[, i] * Nt, Z))
        }    
        if (!is.null(constraint))
            c(scores[, 1], sum(scores[, 2]))[-constraint[, 1]]
        else
            c(scores[, 1], sum(scores[, 2]))
    }
    environment(logLik.rasch) <- environment(score.rasch) <- environment()
    gc()
    res.qN <- optim(betas[!is.na(betas)], fn = logLik.rasch, gr = score.rasch, method = con$method, hessian = TRUE, 
                control = list(maxit = con$iter.qN, trace = as.numeric(con$verbose)), constraint = constraint)
    ev <- eigen(res.qN$hes, TRUE, TRUE)$values
    if (!all(ev >= -1e-06 * abs(ev[1]))) 
        warning("Hessian matrix at convergence is not positive definite, unstable solution; re-fit the model.\n")
    betas <- betas.rasch(res.qN$par, constraint, p)
    rownames(betas) <- if (!is.null(colnamsX)) colnamsX else paste("Item", 1:p)
    colnames(betas) <- c("beta.i", "beta")
    max.sc <- max(abs(score.rasch(res.qN$par, constraint)))
    fit <- list(coefficients = betas, log.Lik = -res.qN$val, convergence = res.qN$conv, hessian = res.qN$hes, 
                counts = res.qN$counts, patterns = list(X = X, obs = obs), GH = list(Z = Z, GHw = GHw), max.sc = max.sc, 
                constraint = constraint)
    fit$IRT.param <- IRT.param
    fit$X <- oX
    fit$control <- con
    fit$na.action <- na.action
    fit$call <- cl
    class(fit) <- "rasch"
    fit
}

