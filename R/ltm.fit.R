"ltm.fit" <-
function (X, betas, constraint, factors, inter, quad.z1, quad.z2, 
    control) 
{
    n <- nrow(X)
    p <- ncol(X)
    ind.cc <- complete.cases(X)
    pats <- apply(X[ind.cc, ], 1, paste, collapse="")
    freqs <- table(pats)
    obs <- c( as.vector(freqs), rep(1, sum(!ind.cc)) )
    X. <- as.numeric(t(apply(cbind(names(freqs)), 1, substring, 1:p, 1:p)))
    dim(X.) <- c(length(freqs), p)
    X <- rbind(X., X[!ind.cc, ])
    mX <- 1 - X
    if(any(na.ind <- is.na(X)))
        X[na.ind] <- mX[na.ind] <- 0
    q. <- 1 + factors + sum(inter, quad.z1, quad.z2)
    GH <- GHpoints(control$GHk, factors, inter, quad.z1, quad.z2)
    Z <- cbind(1, GH$x)
    GHw <- GH$w * exp(rowSums(dnorm(Z[, seq(2, factors + 1), 
        drop = FALSE], log = TRUE)))
    environment(EM) <- environment(logLik.ltm) <- environment(score.ltm) <- environment(patterns) <- environment()
    gc()
    res.EM <- EM(betas, constraint, control$iter.em, control$verbose)
    gc()
    if (!is.null(constraint)) 
        res.EM[constraint[1], 1 + constraint[2]] <- NA
    res.qN <- optim(c(res.EM[!is.na(res.EM)]), fn = logLik.ltm, 
        gr = score.ltm, method = control$method, hessian = TRUE, 
        control = list(maxit = control$iter.qN, trace = as.numeric(control$verbose)), 
        constraint = constraint)
    if (any(eigen(res.qN$hes, TRUE, TRUE)$val < 1e-05)) 
        warning("Hessian matrix at convergence is not positive definite, unstable solution. Re-fit the model.\n")
    if (!is.null(constraint)) {
        ind <- constraint[1] + constraint[2] * p
        betas. <- numeric(p * q.)
        betas.[-ind] <- res.qN$par
        betas.[ind] <- constraint[3]
        betas <- matrix(betas., p, q.)
        hes <- matrix(0, p * q., p * q.)
        hes[-ind, -ind] <- res.qN$hes
        hes[ind, ] <- hes[, ind] <- 0
        hes[ind, ind] <- 1e+20
    }
    else {
        betas <- matrix(res.qN$par, p, q.)
        hes <- res.qN$hes
    }
    pats <- patterns(X., betas)
    max.sc <- max(abs(score.ltm(res.qN$par, constraint)))
    list(coefficients = betas, log.Lik = -res.qN$val, convergence = res.qN$conv, 
        hessian = hes, counts = res.qN$counts, patterns = pats, 
        GH = list(Z = Z, GHw = GHw), max.sc = max.sc)
}

