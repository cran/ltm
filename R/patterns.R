"patterns" <-
function (X, betas) 
{
    stopifnot(is.matrix(X), is.matrix(betas), nrow(betas)==ncol(X))
    pr <- plogis(Z %*% t(betas))
    if (any(ind <- pr == 1)) 
        pr[ind] <- 0.9999999
    if (any(ind <- pr == 0)) 
        pr[ind] <- 1e-07
    p.xz <- exp(X %*% t(log(pr)) + (1 - X) %*% t(log(1 - pr)))
    ex <- n * colSums(GHw * t(p.xz))
    list(mat = X, dat = data.frame(Pattern = names(freqs), Obs = obs[1:nrow(X)], 
        Exp = round(ex, 2)))
}

