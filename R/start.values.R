"start.values" <-
function (X, factors, inter, quad.z1, quad.z2) 
{
    n <- dim(X)[1]
    p <- dim(X)[2]
    Z <- rnorm(factors * n, sd = 1.5)
    dim(Z) <- c(n, factors)
    if (inter) 
        Z <- cbind(Z, Z[, 1] * Z[, 2])
    if (quad.z1) 
        Z <- cbind(Z, Z[, 1] * Z[, 1])
    if (quad.z2) 
        Z <- cbind(Z, Z[, 2] * Z[, 2])
    coefs <- matrix(0, p, ncol(Z) + 1)
    for (i in seq(1, p)) coefs[i, ] <- glm(X[, i] ~ Z, family = binomial)$coef
    dimnames(coefs) <- NULL
    return(coefs)
}

