"plot.rasch" <-
function (x, type = c("ICC", "IIC"), items = NULL, legend = FALSE, cx = "topleft", cy = NULL, ncol = 1, 
                        col = palette(), lty = 1, ...) {
    if (!inherits(x, "rasch"))
        stop("Use only with 'rasch' objects.\n")
    type <- match.arg(type)
    betas <- x$coefficients
    p <- nrow(betas)
    itms <- if (!is.null(items)) {
        if (!is.numeric(items) || length(items) > p)
            stop("'items' must be a numeric vector of length at most ", p)
        if (type == "ICC" && any(items < 1 | items > p))
            stop("'items' must contain numbers between 1 and ", p, " denoting the items.\n")
        if (type == "IIC" && any(items < 0 | items > p))
            stop("'items' must contain numbers between 0 and ", p)
        items
    } else
        1:p
    z1 <- seq(-3.8, 3.8, length = 100)
    Z <- cbind(1, z1)
    pr <- if (type == "ICC") plogis(Z %*% t(betas)) else betas[1, 2]^2 * plogis(Z %*% t(betas)) * (1 - plogis(Z %*% t(betas)))
    plot.items <- type == "ICC" || (type == "IIC" & (is.null(items) || all(items > 0)))
    plot.info <- !plot.items
    col <- if (plot.items) rep(col, length.out = length(itms)) else col[1]
    lty <- if (plot.items) rep(lty, length.out = length(itms)) else lty[1]
    main <- if (type == "ICC") "Item Characteristic Curves" else { 
        if (plot.items) "Item Information Curves" else "Test Information Function"
    }
    ylab <- if (type == "ICC") "Probability" else "Information"
    r <- if (type == "ICC") c(0, 1) else { if (plot.info) range(rowSums(pr)) else range(pr[, itms]) }
    plot(c(-3.8, 3.8), r, type = "n", xlab = "Ability", ylab = ylab, main = main, ...)
    if (legend) {
        legnd <- if (plot.info) "Information" else rownames(betas)[itms]
        legend(cx, cy, legend = legnd, col = col, lty = lty, bty = "n", ncol = ncol, ...) 
    } else {
        pos <- round(seq(10, 90, length = length(itms)))
        nams <- if (rownames(betas)[1] == "Item 1") 1:p else rownames(betas)
    }
    if (plot.items) {
        for (it in seq(along = itms)) {
            lines(z1, pr[, itms[it]], lty = lty[it], col = col[it], ...)
            if (!legend)
                text(z1[pos[it]], pr[pos[it], itms[it]], labels = nams[itms[it]], adj = c(0, 0), col = col[it], ...)
        }
    }
    if (plot.info)
        lines(z1, rowSums(pr), lty = lty, col = col, ...)
    invisible()
}

