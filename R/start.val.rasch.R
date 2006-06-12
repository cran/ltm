"start.val.rasch" <-
function (start.val, data) {
    data <- na.exclude(data)
    attr(data, "na.action") <- NULL
    n <- nrow(data)
    p <- ncol(data)
    cmptStrVal <- is.null(start.val) || (start.val == "random" || (all(is.numeric(start.val)) && length(start.val) != p+1))
    randStrVal <- length(start.val) == 1 && start.val == "random"
    if (cmptStrVal) {
        rs <- as.vector(rowSums(data))
        len.uni <- length(unique(rs))
        rs <- factor(rs, labels = 1:len.uni)
        rs <- as.numeric(levels(rs))[as.integer(rs)]
        z <- cbind(1, seq(-3, 3, len = len.uni)[rs])
        if (randStrVal)
            z[, 2] <- rnorm(n)
        coefs <- matrix(0, p, 2)
        for (i in 1:p) {
            fm <- try(glm.fit(z, data[, i], family = binomial()), silent = TRUE)
            coefs[i, ] <- if (!inherits(fm, "try-error")) {
                fm$coef
            } else {
                glm.fit(cbind(1, rnorm(n)), data[, i], family = binomial())$coef
            }
        }
        c(coefs[, 1], mean(coefs[, 2]))
    } else 
        start.val
}

