"logLik.ltm" <-
function(betas, constraint){
    if(!is.null(constraint)){
        betas. <- numeric(p * q.)
        ind <- constraint[1] + constraint[2] * p
        betas.[-ind] <- betas
        betas.[ind] <- constraint[3]
        betas <- betas.
    }
    dim(betas) <- c(p, q.)
    pr <- probs(Z %*% t(betas))
    p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1-pr)))
    p.x <- rep(c(p.xz %*% GHw), obs)
    -sum(log(p.x))
}

