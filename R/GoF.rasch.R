"GoF.rasch" <-
function (object, B = 49, ...) {
    if (!inherits(object, "rasch"))
        stop("Use only with 'rasch' objects.\n")
    if (any(is.na(object$X)) && is.null(object$na.action))
        stop("\nIn the presense of missing values the parametric Bootstrap test is not currently implemented.\n")
    pearson.chi <- function (object) {
        Obs <- object$patterns$obs
        fits <- fitted(object)
        Exp <- fits[, ncol(fits)]
        if (any(ind <- Exp == 0))
            Exp[ind] <- 0.001
        sum((Obs - Exp)^2/Exp) + n - sum(Exp)
    }
    rmvlogis <- function (betas) {
        z <- rnorm(n)
        pr <- plogis(cbind(1, z) %*% t(betas))
        X <- matrix(0, n, p)
        for (i in 1:p)
            X[, i] <- ifelse(runif(n) < pr[, i], 1, 0)
        X
    }
    old <- options(warn = (-1))
    on.exit(options(old))
    constraint <- object$constraint
    betas <- object$coef
    betas <- c(betas[, -2], betas[1, 2])
    if (!is.null(constraint))
        betas <- betas[-constraint[, 1]]
    Var.betas <- vcov(object)
    n <- nrow(object$X)
    p <- ncol(object$X)
    Tobs <- pearson.chi(object)
    Ts <- numeric(B)
    for (i in 1:B) {
        betas. <- mvrnorm(1, betas, Var.betas)
        betas. <- betas.rasch(betas., constraint, p)
        X <- rmvlogis(betas.)
        Ts[i] <- pearson.chi(rasch(X, constraint = constraint, start.val = c(betas.[, 1], betas.[1, 2]), 
                                    control = object$control))
    }
    p.val <- (1 + sum(Ts >= Tobs)) / (B + 1)
    out <- list(Tobs = Tobs, p.value = p.val, B = B, call = object$call)
    class(out) <- "GoF.rasch"
    out
}

