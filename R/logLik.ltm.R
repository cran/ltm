"logLik.ltm" <-
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
    pr <- plogis(Z %*% t(betas))
    if (any(ind <- pr == 1)) 
        pr[ind] <- 0.9999999
    if (any(ind <- pr == 0)) 
        pr[ind] <- 1e-07
    p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
    p.x <- rep(c(p.xz %*% GHw), obs)
    -sum(log(p.x))
}

