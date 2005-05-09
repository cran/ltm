"patterns" <-
function(X, betas){
    stopifnot(is.matrix(X), is.matrix(betas), nrow(betas)==ncol(X))
    pr <- probs(Z %*% t(betas))
    p.xz <- exp(X %*% t(log(pr)) + (1 - X) %*% t(log(1 - pr)))
    ex <- n * colSums(GHw * t(p.xz))
    list(mat=X, dat=data.frame(Pattern=names(freqs), Obs=obs[1:nrow(X)], Exp=round(ex, 2)))
}

