"rmvordlogis" <-
function (n, betas) {
    p <- length(betas)
    ncatg <- sapply(betas, length)
    z <- rnorm(n)
    gammas <- lapply(betas, function (x) {
        nx <- length(x)
        cbind(plogis(matrix(x[-nx], n, nx - 1, TRUE) - x[nx] * z), 1)
    })
    prs <- lapply(gammas, function (x) {
        nc <- ncol(x)
        cbind(x[, 1], x[, 2:nc] - x[, 1:(nc - 1)])
    })
    out <- matrix(0, n, p)
    for (j in 1:p) {
        for (i in 1:n) {
            out[i, j] <- sample(ncatg[j], 1, prob = prs[[j]][i, ])
        }
    }
    out
}

