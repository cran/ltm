"ham" <-
function (obj, betas, X, GH) 
{
    scores.fun <- function(betas, X) {
        p <- nrow(betas)
        q. <- ncol(betas)
        pr <- plogis(GH$Z %*% t(betas))
        if (any(ind <- pr == 1)) 
            pr[ind] <- 0.9999999
        if (any(ind <- pr == 0)) 
            pr[ind] <- 1e-07
        p.xz <- exp(X %*% t(log(pr)) + (1 - X) %*% t(log(1 - 
            pr)))
        p.x <- c(p.xz %*% GH$GHw)
        p.zx <- p.xz/p.x
        Nt <- GH$GHw * colSums(p.zx)
        scores <- matrix(0, p, q.)
        for (i in 1:p) {
            rit <- GH$GHw * colSums(p.zx * X[, i])
            scores[i, ] <- c(crossprod(rit - pr[, i] * Nt, GH$Z))
        }
        if (inherits(obj, "ltm")) 
            return(c(scores))
        else return(c(scores[, 1], sum(scores[, 2])))
    }
    n <- nrow(X)
    nb <- if (inherits(obj, "ltm")) 
        length(c(betas))
    else length(unique(c(betas)))
    lis <- lapply(1:n, array, data = 0, dim = c(nb, nb))
    for (m in 1:n) {
        sc <- scores.fun(betas, X[m, , drop = FALSE])
        lis[[m]] <- outer(sc, sc)
    }
    matSums(lis)
}

