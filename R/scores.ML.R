"scores.ML" <-
function (betas, X, method) 
{
    logLik.z <- function(z, y, betas) {
        if (!is.matrix(z)) 
            z <- t(z)
        z <- Z.fun(z, inter, quad.z1, quad.z2)
        pr <- probs(betas %*% t(cbind(1, z)))
        exp(colSums(y * log(pr) + (1 - y) * log(1 - pr))) * 
            exp(rowSums(dnorm(z[, seq(1, factors), drop = FALSE], 
                log = TRUE)))
    }
    sc.z <- function(z, y, betas) {
        z <- Z.fun(z, inter, quad.z1, quad.z2)
        pr <- probs(c(betas %*% c(1, z)))
        fits <- y - pr
        if (factors == 1) {
            out <- if (quad.z1) 
                z[1] - sum(fits * (betas[, 2] + 2 * z[1] * betas[, 
                  3]))
            else z[1] - sum(fits * betas[, 2])
        }
        if (factors == 2) {
            fac1 <- betas[, 2]
            fac2 <- betas[, 3]
            if (inter) {
                fac1 <- fac1 + z[2] * betas[, 4]
                fac2 <- fac2 + z[1] * betas[, 4]
            }
            if (quad.z1) {
                fac1 <- if (inter) 
                  fac1 + 2 * z[1] * betas[, 5]
                else fac1 + 2 * z[1] * betas[, 4]
            }
            if (quad.z2) {
                if (inter || quad.z1) 
                  fac2 <- fac2 + 2 * z[2] * betas[, 5]
                if (inter && quad.z1) 
                  fac2 <- fac2 + 2 * z[2] * betas[, 6]
            }
            out <- c(z[1] - sum(fits * fac1), z[2] - sum(fits * 
                fac2))
        }
        return(out)
    }
    score <- function(f, gr, ...) {
        g <- function(z, ...) -log(logLik.z(z, ...))
        if (factors == 1) {
            opt <- optim(0, fn = g, gr = gr, method = "BFGS", 
                hessian = TRUE, ...)
            hc <- c(1/opt$hes)
        }
        if (factors == 2) {
            opt <- optim(c(0, 0), fn = g, gr = gr, method = "BFGS", 
                hessian = TRUE, ...)
            hc <- solve(opt$hessian)
        }
        list(mu = opt$par, hes = hc)
    }
    if (method == "EB") {
        scores.ML <- matrix(0, nx, factors)
        hes.ML <- array(data = 0, dim = c(factors, factors, nx))
        for (i in 1:nx) {
            out <- score(f = logLik.z, gr = sc.z, y = X[i, ], 
                betas = betas)
            scores.ML[i, ] <- out$mu
            hes.ML[, , i] <- out$hes
        }
        se.ML <- t(apply(hes.ML, 3, function(x) sqrt(diag(x))))
        res$z1 <- if (factors == 2) 
            scores.ML[, 1]
        else c(scores.ML)
        res$se.z1 <- if (factors == 2) 
            se.ML[, 1]
        else c(se.ML)
        if (factors == 2) {
            res$z2 <- scores.ML[, 2]
            res$se.z2 <- se.ML[, 2]
        }
    }
    if (method == "MI") {
        var.b <- summary(object, robust.se)$Var.betas
        scores.B <- lapply(1:B, array, data = 0, dim = c(nx, 
            factors))
        hes.B <- lapply(1:B, array, data = 0, dim = c(factors, 
            factors, nx))
        for (b in 1:B) {
            betas. <- mvrnorm(1, c(betas), var.b)
            dim(betas.) <- c(p, q.)
            for (i in 1:nx) {
                out <- score(f = logLik.z, gr = sc.z, y = X[i, 
                  ], betas = betas.)
                scores.B[[b]][i, ] <- out$mu
                hes.B[[b]][, , i] <- out$hes
            }
        }
        scores.av <- matMeans(scores.B)
        hes.av <- matArrays(hes.B)
        SV <- lapply(1:B, array, data = 0, dim = c(factors, factors, 
            nx))
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

