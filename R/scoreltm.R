"scoreltm" <-
function(betas, constraint){
    betas <- betas.ltm(betas, constraint, p, q.)
    pr <- probs(Z %*% t(betas))
    p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
    p.x <- c(p.xz %*% GHw)
    p.zx <- p.xz / p.x
    Nt <- GHw * colSums(p.zx * obs)
    scores <- matrix(0, p, q.)
    for (i in 1:p) {
        ind. <- !na.ind[, i]
        rit <- if(all(ind.)) GHw * colSums(p.zx * X[, i] * obs) else GHw * colSums(p.zx[ind., ] * X[ind., i] * obs[ind.])
        scores[i, ] <- -c(crossprod(rit - pr[, i] * Nt, Z))
    }
    if(!is.null(constraint))
        scores[-((constraint[, 2] - 1) * p + constraint[, 1])]
    else
        c(scores)
}

