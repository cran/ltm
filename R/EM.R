"EM" <-
function(betas, constraint, iter, verbose=FALSE){
    lgLik <- numeric(iter)
    for(it in 1:iter) {
        pr <- probs(Z %*% t(betas))
        dvar <- pr * (1 - pr)
        p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
        p.x <- c(p.xz %*% GHw)
        lgLik[it] <- sum(log(rep(p.x, obs)))
        if(verbose)
            cat("EM iteration:", it, "  -logLik:", -lgLik[it], "\n")
        p.zx <- p.xz / p.x
        Nt <- GHw * colSums(p.zx * obs)
        nb <- matrix(0, p, q.)
        for(i in 1:p) {
            ind <- !na.ind[, i]
            rit <- if(all(ind)) GHw * colSums(p.zx * X[, i] * obs) else GHw * colSums(p.zx[ind, ] * X[ind, i] * obs[ind])
            hes <- crossprod(Z, (dvar[, i] * Nt) * Z)
            nb[i, ] <- betas[i, ] + solve(hes, c(crossprod(rit - pr[, i] * Nt, Z)))
        }
        betas <- nb
        if(!is.null(constraint))
            betas[constraint[, 1:2]] <- constraint[, 3]
    }
    betas
}

