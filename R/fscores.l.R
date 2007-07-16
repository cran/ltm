`fscores.l` <-
function (betas, X, method) {
    logf.z <- function (z, y, betas) {
        Z <- c(1, z)
        names(Z) <- if (length(z) == 1) c("(Intercept)", "z1") else c("(Intercept)", "z1", "z2")
        if (inter)
            Z <- c(Z, "z1:z2" = z[1] * z[2])
        if (quad.z1)
            Z <- c(Z, "I(z1^2)" = z[1] * z[1])
        if (quad.z2)
            Z <- c(Z, "I(z2^2)" = z[2] * z[2])            
        Z <- Z[match(colnames(betas), names(Z))]
        pr <- probs(c(betas %*% Z))
        -sum(dbinom(y, 1, pr, log = TRUE), na.rm = TRUE) - sum(dnorm(z, log = TRUE))
    }
    fscore <- function (logf.z, y, betas) {
        opt <- optim(rep(0, factors), fn = logf.z, method = "BFGS", hessian = TRUE, y = y, betas = betas)
        hc <- if (factors == 1) c(1/opt$hes) else solve(opt$hessian)
        list(mu = opt$par, hes = hc)
    }
    if (method == "EB") {
        scores.ML <- matrix(0, nx, factors)
        hes.ML <- array(data = 0, dim = c(factors, factors, nx))
        for (i in 1:nx) {
            out <- fscore(logf.z = logf.z, y = X[i, ], betas = betas)
            scores.ML[i, ] <- out$mu
            hes.ML[, , i] <- out$hes
        }
        se.ML <- t(apply(hes.ML, 3, function(x) sqrt(diag(x))))
        res$z1 <- if (factors == 2) scores.ML[, 1] else c(scores.ML)
        res$se.z1 <- if (factors == 2) se.ML[, 1] else c(se.ML)
        if (factors == 2) {
            res$z2 <- scores.ML[, 2]
            res$se.z2 <- se.ML[, 2]
        }
    }
    if (method == "EAP") {
        Z <- object$GH$Z
        GHw <- object$GH$GHw
        pr <- probs(Z %*% t(betas))
        if (any(na.ind <- is.na(X)))
            X[na.ind] <- 0        
        p.xz <- exp(X %*% t(log(pr)) + (1 - X) %*% t(log(1 - pr)))
        p.x <- c(p.xz %*% GHw)
        p.zx <- p.xz / p.x
        res$z1 <- c(p.zx %*% (Z[, 2] * GHw))
        res$se.z1 <- sqrt(c(p.zx %*% (Z[, 2] * Z[, 2] * GHw)) - res$z1^2)
        if (object$ltst$factors > 1){
            res$z2 <- c(p.zx %*% (Z[, 3] * GHw))
            res$se.z2 <- sqrt(c(p.zx %*% (Z[, 3] * Z[, 3] * GHw)) - res$z2^2)
        }   
    }
    if (method == "MI") {
        constraint <- object$constraint
        if (!is.null(constraint))
            betas <- betas[-((constraint[, 2] - 1) * p + constraint[, 1])]
        var.b <- vcov(object, robust.se)
        scores.B <- lapply(1:B, array, data = 0, dim = c(nx, factors))
        hes.B <- lapply(1:B, array, data = 0, dim = c(factors, factors, nx))
        for (b in 1:B) {
            betas. <- mvrnorm(1, c(betas), var.b)
            betas. <- betas.ltm(betas., constraint, p, q.)
            colnames(betas.) <- colnames(betas)
            for (i in 1:nx) {
                out <- fscore(logf.z = logf.z, y = X[i, ], betas = betas.)
                scores.B[[b]][i, ] <- out$mu
                hes.B[[b]][, , i] <- out$hes
            }
        }
        scores.av <- matMeans(scores.B)
        hes.av <- matArrays(hes.B)
        SV <- lapply(1:B, array, data = 0, dim = c(factors, factors, nx))
        for (b in 1:B) {
            for (i in 1:nx) {
                sc.dif <- scores.B[[b]][i, ] - scores.av[i, ]
                SV[[b]][, , i] <- outer(sc.dif, sc.dif)
            }
        }
        SV <- (B * matArrays(SV))/(B - 1)
        hes.av <- hes.av + (1 + 1/B) * SV
        se.av <- t(apply(hes.av, 3, function(x) sqrt(diag(x))))
        if (factors == 1) {
            res$z1 <- c(scores.av)
            res$se.z1 <- c(se.av)
        }
        if (factors == 2) {
            res$z1 <- scores.av[, 1]
            res$se.z1 <- se.av[, 1]
            res$z2 <- scores.av[, 2]
            res$se.z2 <- se.av[, 2]
        }
    }
    res
}

