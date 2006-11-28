`loglikgrm` <-
function (thetas, constrained) {
    betas <- betas.grm(thetas, constrained, ind1, ind2, p)
    k <- length(Z)
    cpr <- cprobs(betas, Z)
    cpr <- lapply(cpr, function (x) log(rbind(x[1, ], diff(x))))
    log.p.xz <- matrix(0, nfreqs, k)
    for (j in 1:p) {
        log.pr <- cpr[[j]]
        log.p.xz <- log.p.xz + log.pr[X[, j], ]
    }
    p.x <- rep(exp(log.p.xz) %*% GHw, obs)
    - sum(log(p.x))
}

