"rasch" <-
function (dat, start.val, na.action = NULL, control = list()) 
{
    cl <- match.call()
    X <- data.matrix(dat)
    dimnames(X) <- NULL
    oX <- X
    if (!is.null(na.action)) 
        X <- na.action(X)
    n <- nrow(X)
    p <- ncol(X)
    con <- list(iter.qN = 150, GHk = 20, method = "BFGS", verbose = FALSE)
    con[names(control)] <- control
    betas <- if (!missing(start.val) && length(start.val) == p + 1) start.val else rnorm(p + 1)
    betas[p + 1] <- abs(betas[p + 1])
    ind.cc <- complete.cases(X)
    pats <- apply(X[ind.cc,], 1, paste, collapse="")
    freqs <- table(pats)
    obs <- c( as.vector(freqs), rep(1, sum(!ind.cc)) )
    X. <- as.numeric(t(apply(cbind(names(freqs)), 1, substring, 1:p, 1:p)))
    dim(X.) <- c(length(freqs), p)
    X <- rbind(X., X[!ind.cc,])
    mX <- 1 - X
    if (any(na.ind <- is.na(X)))
        X[na.ind] <- mX[na.ind] <- 0
    GH <- GHpoints(con$GHk, 1, FALSE, FALSE, FALSE)
    Z <- cbind(1, GH$x)
    GHw <- GH$w * dnorm(Z[, 2])
    logLik.rasch <- function(betas) {
        betas <- cbind(betas[1:p], abs(betas[p + 1]))
        pr <- probs(Z %*% t(betas))
        p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
        p.x <- rep(c(p.xz %*% GHw), obs)
        -sum(log(p.x))
    }
    score.rasch <- function(betas) {
        betas <- cbind(betas[1:p], abs(betas[p + 1]))
        pr <- probs(Z %*% t(betas))
        p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
        p.x <- c(p.xz %*% GHw)
        p.zx <- p.xz/p.x
        Nt <- GHw * colSums(p.zx * obs)
        scores <- matrix(0, p, 2)
        for(i in 1:p){
            ind <- !na.ind[, i]
            rit <- if(all(ind)) GHw * colSums(p.zx * X[, i] * obs) else GHw * colSums(p.zx[ind, ] * X[ind, i] * obs[ind])
            scores[i,] <- -c(crossprod(rit - pr[,i] * Nt, Z))
        }    
        c(scores[, 1], sum(scores[, 2]))
    }
    environment(logLik.rasch) <- environment(score.rasch) <- environment(patterns) <- environment()
    gc()
    res.qN <- optim(betas, fn = logLik.rasch, gr = score.rasch, 
        method = con$method, hessian = TRUE, control = list(maxit = con$iter.qN, 
            trace = as.numeric(con$verbose)))
    if (any(eigen(res.qN$hes, TRUE, TRUE)$val < sqrt(.Machine$double.eps))) 
        warning("Hessian matrix at convergence is not positive definite, unstable solution. Re-fit the model.\n")
    betas <- cbind(res.qN$par[1:p], abs(res.qN$par[p + 1]))
    pats <- patterns(X., betas)
    max.sc <- max(abs(score.rasch(res.qN$par)))
    fit <- list(coefficients = betas, log.Lik = -res.qN$val, 
        convergence = res.qN$conv, hessian = res.qN$hes, counts = res.qN$counts, 
        patterns = pats, GH = list(Z = Z, GHw = GHw), max.sc = max.sc)
    fit$X <- oX
    fit$control <- con
    fit$na.action <- na.action
    fit$call <- cl
    class(fit) <- "rasch"
    fit
}

