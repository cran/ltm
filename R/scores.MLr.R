"scores.MLr" <-
function (betas, X, method) 
{
    logLik.z <- function(z, y, betas) {
        if (!is.matrix(z)) 
            z <- t(z)
        probs <- plogis(betas %*% t(cbind(1, z)))
        if (any(ind <- probs == 1)) 
            probs[ind] <- 0.9999999
        if (any(ind <- probs == 0)) 
            probs[ind] <- 1e-07
        exp(colSums(y * log(probs) + (1 - y) * log(1 - probs))) * 
            dnorm(z[, 1])
    }
    sc.z <- function(z, y, betas) {
        probs <- plogis(c(betas %*% c(1, z)))
        if (any(ind <- probs == 1)) 
            probs[ind] <- 0.9999999
        if (any(ind <- probs == 0)) 
            probs[ind] <- 1e-07
        fits <- y - probs
        z[1] - sum(fits * betas[, 2])
    }
    score <- function(f, gr, ...) {
        g <- function(z, ...) -log(logLik.z(z, ...))
        opt <- optim(0, fn = g, gr = gr, method = "BFGS", hessian = TRUE, 
            ...)
        hc <- c(1/opt$hes)
        list(mu = opt$par, hes = hc)
    }
    if (method == "EB") {
        scores.ML <- hes.ML <- numeric(nx)
        for (i in 1:nx) {
            out <- score(f = logLik.z, gr = sc.z, y = X[i, ], 
                betas = betas)
            scores.ML[i] <- out$mu
            hes.ML[i] <- out$hes
        }
        res$z1 <- scores.ML
        res$se.z1 <- sqrt(hes.ML)
    }
    if (method == "MI") {
        var.b <- summary(object, robust.se)$Var.betas
        scores.B <- hes.B <- array(0, dim = c(nx, B))
        for (b in 1:B) {
            betas. <- mvrnorm(1, c(betas[1:p], betas[p + 1]), 
                var.b)
            betas. <- cbind(betas.[1:p], betas.[p + 1])
            for (i in 1:nx) {
                out <- score(f = logLik.z, gr = sc.z, y = X[i, 
                  ], betas = betas.)
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

