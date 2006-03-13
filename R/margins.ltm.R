"margins.ltm" <-
function (object, type = c("two-way", "three-way"), rule = 3.5, nprint = 3, ...) {
    if (!inherits(object, "ltm") && !inherits(object, "rasch"))
        stop("Use only with 'ltm' or 'rasch' objects.\n")
    type <- match.arg(type)
    n <- nrow(object$X)
    betas <- object$coef
    p <- nrow(betas)
    q. <- ncol(betas)
    pr <- plogis(object$GH$Z %*% t(betas))
    GHw <- object$GH$GHw
    X <- object$patterns$X
    Obs <- object$patterns$obs
    if (type == "two-way") {
        index <- combinations(p, 2)
        nindex <- nrow(index)
        combs <- as.matrix(expand.grid(lapply(1:2, function(x) 0:1)))
        dimnames(combs) <- NULL
        ncombs <- nrow(combs)
        margins <- array(0, dim = c(nindex, 5, ncombs))
        for (i in 1:nindex) {
            item1 <- index[i, 1]; item2 <- index[i, 2]
            p1 <- pr[, item1]; mp1 <- 1 - p1
            p2 <- pr[, item2]; mp2 <- 1 - p2
            for (j in 1:ncombs) {
                ind <- X[, item1] == combs[j, 1] & X[, item2] == combs[j, 2]
                pp <- p1^combs[j, 1] * mp1^(1 - combs[j, 1]) * p2^combs[j, 2] * mp2^(1 - combs[j, 2])
                obs <- sum(Obs[ind])
                exp. <- n * sum(GHw * pp)
                margins[i, , j] <- c(item1, item2, obs, exp., (obs - exp.)^2/exp.)
            }
        }
    }
    if (type == "three-way") {
        index <- combinations(p, 3)
        nindex <- nrow(index)
        combs <- as.matrix(expand.grid(lapply(1:3, function(x) 0:1)))
        dimnames(combs) <- NULL
        ncombs <- nrow(combs)
        margins <- array(0, dim = c(nindex, 6, ncombs))
        for (i in 1:nindex) {
            item1 <- index[i, 1]; item2 <- index[i, 2]; item3 <- index[i, 3]
            p1 <- pr[, item1]; mp1 <- 1 - p1
            p2 <- pr[, item2]; mp2 <- 1 - p2
            p3 <- pr[, item3]; mp3 <- 1 - p3
            for (j in 1:ncombs) {
                ind <- X[, item1] == combs[j, 1] & X[, item2] == combs[j, 2] & X[, item3] == combs[j, 3]
                pp <- p1^combs[j, 1] * mp1^(1 - combs[j, 1]) * p2^combs[j, 2] * mp2^(1 - combs[j, 2]) * p3^combs[j, 3] * mp3^(1 - combs[j, 3])
                obs <- sum(Obs[ind])
                exp. <- n * sum(GHw * pp)
                margins[i, , j] <- c(item1, item2, item3, obs, exp., (obs - exp.)^2/exp.)
            }
        }
    }
    if (nprint > nindex) {
        warning("not acceptable value for 'nprint' argument, the maximum acceptable value is ", nindex, 
                    "; 'nprint' is set to its default value.\n")
        nprint <- 3
    }
    out <- list(margins = margins, type = type, nprint = nprint, combs = combs, rule = rule, call = object$call)
    class(out) <- "margins.ltm"
    out
}

