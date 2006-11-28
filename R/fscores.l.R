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
        -(sum(y * log(pr) + (1 - y) * log(1-pr)) + sum(dnorm(z, log = TRUE)))
    }
    logg.z <- function (z, y, betas) {
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
        fits <- y - pr
        out <- if (factors == 1){
            if (quad.z1)
                z[1] - sum(fits * (betas[, 2] + 2 * z[1] * betas[, 3]))
            else
                z[1] - sum(fits * betas[, 2])
        } else if (factors == 2) {
                fac1 <- betas[, 2]
                fac2 <- betas[, 3]
                if (inter) {
                    fac1 <- fac1 + z[2] * betas[, 4]
                    fac2 <- fac2 + z[1] * betas[, 4]
                }
                if (quad.z1) {
                    fac1 <- if (inter) fac1 + 2 * z[1] * betas[, 5] else fac1 + 2 * z[1] * betas[, 4]
                }
                if (quad.z2) {
                    if (inter || quad.z1) 
                        fac2 <- fac2 + 2 * z[2] * betas[, 5]
                    if (inter && quad.z1) 
                        fac2 <- fac2 + 2 * z[2] * betas[, 6]
                }
            c(z[1] - sum(fits * fac1), z[2] - sum(fits * fac2))
        }
        out
    }
    fscore <- function (logf.z, logg.z, y, betas) {
        if (factors == 1) {
            opt <- optim(0, fn = logf.z, gr = logg.z, method = "BFGS", hessian = TRUE, y = y, betas = betas)
            hc <- c(1/opt$hes)
        }
        if (factors == 2) {
            opt <- optim(c(0, 0), fn = logf.z, gr = logg.z, method = "BFGS", hessian = TRUE, y = y, betas = betas)
            hc <- solve(opt$hessian)
        }
        list(mu = opt$par, hes = hc)
    }
    if (method == "EB") {
        scores.ML <- matrix(0, nx, factors)
        hes.ML <- array(data = 0, dim = c(factors, factors, nx))
        for (i in 1:nx) {
            out <- fscore(logf.z = logf.z, logg.z = logg.z, y = X[i, ], betas = betas)
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
                out <- fscore(logf.z = logf.z, logg.z = logg.z, y = X[i, ], betas = betas.)
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

