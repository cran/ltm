"fscores.r" <-
function (betas, X, method) {
    logf.z <- function(z, y, betas) {
        pr <- probs(c(betas %*% c(1, z)))
        -(sum(y * log(pr) + (1 - y) * log(1 - pr)) + dnorm(z, log = TRUE))
    }
    logg.z <- function(z, y, betas) {
        pr <- probs(c(betas %*% c(1, z)))
        fits <- y - pr
        z - sum(fits * betas[, 2])
    }
    fscore <- function(logf.z, logg.z, y, betas) {
        opt <- optim(0, fn = logf.z, gr = logg.z, method = "BFGS", hessian = TRUE, y = y, betas = betas)
        hc <- c(1/opt$hes)
        list(mu = opt$par, hes = hc)
    }
    if (method == "EB") {
        scores.ML <- hes.ML <- numeric(nx)
        for (i in 1:nx) {
            out <- fscore(logf.z = logf.z, logg.z = logg.z, y = X[i, ], betas = betas)
            scores.ML[i] <- out$mu
            hes.ML[i] <- out$hes
        }
        res$z1 <- scores.ML
        res$se.z1 <- sqrt(hes.ML)
    }
    if (method == "MI") {
        constraint <- object$constraint
        betas <- c(betas[, 1], betas[1, 2])
        if(!is.null(constraint))
            betas <- betas[-constraint[, 1]]
        Var.betas <- vcov(object, robust.se)
        scores.B <- hes.B <- array(0, dim = c(nx, B))
        for (b in 1:B) {
            betas. <- mvrnorm(1, betas, Var.betas)
            betas. <- betas.rasch(betas., constraint, p)
            for (i in 1:nx) {
                out <- fscore(logf.z = logf.z, logg.z = logg.z, y = X[i, ], betas = betas.)
                scores.B[i, b] <- out$mu
                hes.B[i, b] <- out$hes
            }
        }
        scores.av <- rowMeans(scores.B)
        hes.av <- rowMeans(hes.B)
        SV <- array(0, dim = c(nx, B))
        for (b in 1:B) {
            for (i in 1:nx) {
                sc.dif <- scores.B[i, b] - scores.av[i]
                SV[i, b] <- outer(sc.dif, sc.dif)
            }
        }
        SV <- rowSums(SV)/(B - 1)
        hes.av <- hes.av + (1 + 1/B) * SV
        res$z1 <- scores.av
        res$se.z1 <- sqrt(hes.av)
    }
    res
}

