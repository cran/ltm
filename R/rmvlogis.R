"rmvlogis" <-
function (n, thetas, guessing = FALSE, link = c("logit", "probit"), distr = c("normal", "logistic")) {
    if (!is.matrix(thetas))
        stop("'thetas' must be a matrix with rows representing the items.\n")
    link <- match.arg(link)
    distr <- match.arg(distr)
    z <- if (distr == "normal") cbind(1, rnorm(n)) else cbind(1, rlogis(n))
    p <- nrow(thetas)
    pr <- if (guessing) {
        if (!ncol(thetas) == 3)
            stop("'thetas' must be a 3-column matrix with the 3rd column representing the guessing parameters.\n")
        betas <- thetas[, 1:2]
        cs <- thetas[, 3]
        cs.mat <- matrix(cs, n, p, TRUE)
        cs.mat + (1 - cs.mat) * if (link == "logit") plogis(z %*% t(betas)) else pnorm(z %*% t(betas))
    } else {
        if (link == "logit") plogis(z %*% t(thetas)) else pnorm(z %*% t(thetas))
    }
    X <- matrix(0, n, p)
    for (i in 1:p)
        X[, i] <- rbinom(n, 1, pr[, i])
    X
}

