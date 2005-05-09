"rasch" <-
function (data, constraint = NULL, start.val, na.action = NULL, control = list()) {
    cl <- match.call()
    if(!is.data.frame(data) && !is.matrix(data))
        stop("'data' must be either a numeric matrix or a data.frame")
    X <- data.matrix(data)
    colnamsX <- colnames(X)
    dimnames(X) <- NULL
    oX <- X
    if(!is.null(na.action)) X <- na.action(X)
    n <- nrow(X)
    p <- ncol(X)
    con <- list(iter.qN = 150, GHk = 21, method = "BFGS", verbose = FALSE)
    con[names(control)] <- control
    if(!is.null(constraint)){
        if(!is.matrix(constraint) || (nrow(constraint) > p | ncol(constraint)!=2))
            stop("'constraint' should be a 2-column matrix with at most ", p, " rows (read help file).\n")
        if(any(constraint[, 1] < 1 | constraint[, 1] > p+1))
            stop("the 1st column of 'constraint' should contain integer numbers between 1 and ", p+1, " (read help file).\n")
        constraint[, 1] <- round(constraint[, 1])
    }
    betas <- if(!missing(start.val) && length(start.val) == p + 1) start.val else rnorm(p + 1)
    if(!is.null(constraint)) betas[constraint[, 1]] <- NA
    ind.cc <- complete.cases(X)
    pats <- apply(X[ind.cc,], 1, paste, collapse="")
    freqs <- table(pats)
    obs <- c( as.vector(freqs), rep(1, sum(!ind.cc)) )
    X. <- as.numeric(t(apply(cbind(names(freqs)), 1, substring, 1:p, 1:p)))
    dim(X.) <- c(length(freqs), p)
    X <- rbind(X., X[!ind.cc,])
    mX <- 1 - X
    if(any(na.ind <- is.na(X))) X[na.ind] <- mX[na.ind] <- 0
    GH <- GHpoints(con$GHk, 1, FALSE, FALSE, FALSE)
    Z <- cbind(1, GH$x)
    GHw <- GH$w * dnorm(Z[, 2])
    logLik.rasch <- function(betas, constraint){
        betas <- betas.rasch(betas, constraint, p)
        pr <- probs(Z %*% t(betas))
        p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
        p.x <- rep(c(p.xz %*% GHw), obs)
        -sum(log(p.x))
    }
    score.rasch <- function(betas, constraint){
        betas <- betas.rasch(betas, constraint, p)
        pr <- probs(Z %*% t(betas))
        p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
        p.x <- c(p.xz %*% GHw)
        p.zx <- p.xz / p.x
        Nt <- GHw * colSums(p.zx * obs)
        scores <- matrix(0, p, 2)
        for(i in 1:p){
            ind <- !na.ind[, i]
            rit <- if(all(ind)) GHw * colSums(p.zx * X[, i] * obs) else GHw * colSums(p.zx[ind, ] * X[ind, i] * obs[ind])
            scores[i,] <- -c(crossprod(rit - pr[,i] * Nt, Z))
        }    
        res <- if(!is.null(constraint))
            c(scores[, 1], sum(scores[, 2]))[-constraint[, 1]] else c(scores[, 1], sum(scores[, 2]))
        res
    }
    environment(logLik.rasch) <- environment(score.rasch) <- environment(patterns) <- environment()
    gc()
    res.qN <- optim(betas[!is.na(betas)], fn=logLik.rasch, gr=score.rasch, method=con$method, hessian=TRUE, 
                control=list(maxit=con$iter.qN, trace=as.numeric(con$verbose)), constraint=constraint)
    ev <- eigen(res.qN$hes, TRUE, TRUE)$values
    if(!all(ev >= -1e-06 * abs(ev[1]))) 
        warning("approximate Hessian matrix at convergence is not positive definite, unstable solution; re-fit the model.\n")
    betas <- betas.rasch(res.qN$par, constraint, p)
    rownames(betas) <- if(!is.null(colnamsX)) colnamsX else paste("Item", 1:p)
    colnames(betas) <- c("beta.i", "beta")
    pats <- patterns(X., betas)
    max.sc <- max(abs(score.rasch(res.qN$par, constraint)))
    fit <- list(coefficients=betas, log.Lik=-res.qN$val, convergence=res.qN$conv, hessian=res.qN$hes, 
                counts=res.qN$counts, patterns=pats, GH=list(Z=Z, GHw=GHw), max.sc=max.sc, constraint=constraint)
    fit$X <- oX
    fit$control <- con
    fit$na.action <- na.action
    fit$call <- cl
    class(fit) <- "rasch"
    fit
}
