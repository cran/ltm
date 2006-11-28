`start.val.tpm` <-
function (start.val, data, type, constraint) {
    data <- na.exclude(data)
    attr(data, "na.action") <- NULL
    n <- nrow(data)
    p <- ncol(data)
    cmptStrVal <- is.null(start.val) || (start.val == "random" || (is.matrix(start.val) && length(start.val) != 3*p))
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
        coefs <- cbind(qlogis(seq(0.05, 0.15, length = p))[order(order(coefs[, 1], decreasing = TRUE))], coefs)
        coefs <- if (type == "rasch") c(coefs[, 1:2], abs(mean(coefs[, 3]))) else as.vector(coefs)
    } else {
        coefs[, 1] <- qlogis(coefs[, 1])
        coefs <- unique(c(start.val))
    }
    if (!is.null(constraint)) {
        if (type == "rasch" && any(ind <- constraint[, 2] == 3))
            coefs[-c((constraint[!ind, 2] - 1) * p + constraint[!ind, 1], length(coefs))]
        else
            coefs[-((constraint[, 2] - 1) * p + constraint[, 1])]
    } else
        coefs
}

