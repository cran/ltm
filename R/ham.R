"ham" <-
function(obj, betas, X, GH) {
    scores.fun <- function(betas, X) {
        p <- nrow(betas)
        q. <- ncol(betas)
        pr <- probs(GH$Z %*% t(betas))
        p.xz <- exp(X %*% t(log(pr)) + (1-X) %*% t(log(1-pr)))
        p.x <- c(p.xz %*% GH$GHw)
        p.zx <- p.xz / p.x
        Nt <- GH$GHw * colSums(p.zx)
        scores <- matrix(0, p, q.)
        for(i in 1:p){
            rit <- GH$GHw * colSums(p.zx * X[, i])
            scores[i, ] <- c(crossprod(rit - pr[, i] * Nt, GH$Z))
        }
        if(inherits(obj, "ltm")) return(c(scores)) else return(c(scores[, 1], sum(scores[, 2])))
    }
    n <- nrow(X)
    nb <- if(inherits(obj, "ltm")) length(c(betas)) else nrow(betas) + 1
    lis <- lapply(1:n, array, data=0, dim=c(nb, nb))
    for(m in 1:n){
        sc <- scores.fun(betas, X[m, , drop=FALSE])
        lis[[m]] <- outer(sc, sc)
    }
    out <- matSums(lis)
    if(inherits(obj, "rasch") && !is.null(constr <- obj$constraint)) out <- out[-constr[, 1], -constr[, 1]]
    out
}

