"plot.ltm" <-
function (x, type = c("ICC", "IIC"), items = NULL, legend = FALSE, cx = "topleft", cy = NULL, ncol = 1, 
                    col = palette(), lty = 1, ...) {
    if (!inherits(x, "ltm"))
        stop("Use only with 'ltm' objects.\n")
    type <- match.arg(type)
    if (type == "IIC" && any(x$ltst$factors == 2, x$ltst$inter, x$ltst$quad.z1, x$ltst$quad.z2))
        stop("Item Information Curves are currently plotted only for the one-factor model.\n")
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
    np <- 100
    if (x$ltst$factors == 1){
        z1 <- seq(-3.8, 3.8, length = np)
        Z <- if (x$ltst$quad.z1) cbind(1, z1, z1 * z1) else cbind(1, z1)
        pr <- if (type == "ICC") plogis(Z %*% t(betas)) else {
            pr <- plogis(Z %*% t(betas))
            pqr <- pr * (1 - pr)
            t(t(pqr) * betas[, 2]^2)
        }
        plot.items <- type == "ICC" || (type == "IIC" & (is.null(items) || all(items > 0)))
        plot.info <- !plot.items
        col <- if (plot.items) rep(col, length.out = length(itms)) else col[1]
        lty <- if (plot.items) rep(lty, length.out = length(itms)) else lty[1]
        main <- if (type == "ICC") "Item Characteristic Curves" else { 
            if (plot.items) "Item Information Curves" else "Test Information Function"
        }
        ylab <- if (type == "ICC") "Probability" else "Information"
        r <- if (type == "ICC") c(0, 1) else { if (plot.info) range(rowSums(pr)) else range(pr[, itms]) }
        plot(c(-3.8, 3.8), r, type = "n", xlab = "Ability", ylab = ylab, 
                main = main, ...)
        if (legend) {
            legnd <- if (plot.info) "Information" else rownames(betas)[itms]
            legend(cx, cy, legend = legnd, col = col, lty = lty, bty = "n", ncol = ncol, ...) 
        }
        else {
            pos <- round(seq(10, 90, length = length(itms)))
            nams <- if(rownames(x$coef)[1] == "Item 1") 1:p else rownames(x$coef)
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
    }
    if (x$ltst$factors == 2) {
        nams <- rownames(x$coef)
        if (!any(unlist(x$ltst[2:4]))) {
            cof <- coef(x, TRUE)
            z1 <- cof[itms, 3]
            z2 <- cof[itms, 5]
            plot(z1, z2, type = "n", xlab = "Factor 1", ylab = "Factor 2", main = "Standardized Loadings", 
                    xlim = c(min(z1, -0.1), max(z1, 0.1)), ylim = c(min(z2, -0.1), max(z2, 0.1)))
            abline(h = 0, v = 0, lty = 2)
            text(z1, z2, labels = nams[itms])
        }
        z1 <- seq(-3.8, 3.8, length = np)
        z2 <- seq(-3.8, 3.8, length = np)
        f <- function (z, betas, strct) {
            Z <- cbind(1, z[1], z[2])
            colnames(Z) <- c("(Intercept)", "z1", "z2")
            if (strct$inter)
                Z <- cbind(Z, "z1:z2" = z[1] * z[2])
            if (strct$quad.z1)
                Z <- cbind(Z, "I(z1^2)" = z[1] * z[1])
            if (strct$quad.z2)
                Z <- cbind(Z, "I(z2^2)" = z[2] * z[2])
            Z <- Z[, match(names(betas), colnames(Z)), drop = FALSE]
            pr <- plogis(Z %*% betas)
        }
        grid. <- as.matrix(expand.grid(z1, z2))
        dimnames(grid.) <- NULL
        old.par <- par(ask = TRUE)
        on.exit(par(old.par))
        for (it in itms) {
            z <- apply(grid., 1, f, betas = betas[it, ], strct = x$ltst)
            dim(z) <- c(np, np)
            persp(z1, z2, z, ticktype = "detailed", theta = 30, phi = 30, expand = 0.5, d = 2, cex = 0.7, 
                    xlab = "Factor 1", ylab = "Factor 2", 
                    zlab = "Probability", main = list("Item Characteristic Surfaces", cex = 1.5), 
                    sub = list(nams[it], cex = 1.4), ...)
        }
    }
    invisible()
}

