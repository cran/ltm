"score.ltm" <-
function (betas, constraint) 
{
    if (!is.null(constraint)) {
        betas. <- numeric(p * q.)
        ind <- constraint[1] + constraint[2] * p
        betas.[-ind] <- betas
        betas.[ind] <- constraint[3]
        betas <- betas.
    }
    dim(betas) <- c(p, q.)
    pr <- probs(Z %*% t(betas))
    p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
    p.x <- c(p.xz %*% GHw)
    p.zx <- p.xz / p.x
    Nt <- GHw * colSums(p.zx * obs)
    scores <- matrix(0, p, q.)
    for (i in 1:p) {
        ind <- !na.ind[, i]
        rit <- if (all(ind)) GHw * colSums(p.zx * X[, i] * obs) else GHw * colSums(p.zx[ind, ] * X[ind, i] * obs[ind])
        scores[i, ] <- -c(crossprod(rit - pr[, i] * Nt, Z))
    }
    res <- if (!is.null(constraint)) 
        c(scores)[-(constraint[1] + constraint[2] * p)]
    else c(scores)
    res
}

