"plot.ltm" <-
function (x, cx=NULL, cy=NULL, ...){
    if(!inherits(x, "ltm")) stop("Use only with 'ltm' objects.\n")
    betas <- x$coef
    strct <- ltn.strct(x$ltn.struct)
    np <- 100
    if(strct$factors == 1){
        p <- nrow(betas)
        z1 <- seq(-3.4, 3.4, length = np)
        Z <- if (strct$quad.z1) cbind(1, z1, z1 * z1)
        else cbind(1, z1)
        pr <- plogis(Z %*% t(betas))
        plot(c(-3.4, 3.4), c(0, 1), type = "n", xlab = "z1", ylab = "probability", main = "Item Characteristic Curves")
        for (it in 1:p) lines(z1, pr[, it], col = it, ...)
        x <- if (is.null(cx)) 2.25 else cx
        y <- if (is.null(cy)) 0.5 else cy
        legend(x = x, y = y, legend = paste("Item", 1:p), col = 1:p, lty = 1)
    }
    if(strct$factors == 2) {
        old <- options(graphics.record = TRUE)
        if(!any(unlist(strct[-1]))) {
            cof <- coef(x, TRUE)
            z1 <- cof[, 3]
            z2 <- cof[, 5]
            plot(z1, z2, type = "n", xlab = "Factor 1", ylab = "Factor 2", main = "Standardized loadings", 
                    xlim = c(min(z1, -0.1), max(z1, 0.1)), ylim = c(min(z2, -0.1), max(z2, 0.1)))
            abline(h = 0, v = 0, lty = 2)
            text(z1, z2)
        }
        z1 <- seq(-3.4, 3.4, length = np)
        z2 <- seq(-3.4, 3.4, length = np)
        f <- function(z1, z2, betas, strct) {
            Z <- cbind(1, z1, z2)
            if(strct$inter) Z <- cbind(Z, z1 * z2)
            if(strct$quad.z1) Z <- cbind(Z, z1 * z1)
            if(strct$quad.z2) Z <- cbind(Z, z2 * z2)
            pr <- plogis(Z %*% betas)
        }
        grid. <- as.matrix(expand.grid(z1, z2))
        dimnames(grid.) <- NULL
        z <- apply(grid., 1, function(x, betas, strct) f(x[1], x[2], betas, strct), betas = betas[1, ], strct)
        dim(z) <- c(np, np)
        par(ask = TRUE)
        for (it in 1:nrow(betas)) {
            z <- apply(grid., 1, function(x, betas, strct) f(x[1], x[2], betas, strct), betas = betas[it, ], strct)
            dim(z) <- c(np, np)
            persp(z1, z2, z, ticktype = "detailed", theta = 30, phi = 30, expand = 0.5, d = 2, cex = 0.7, 
                    zlab = "probability", main = "Item Characteristic Surfaces", sub = paste("Item", it), ...)
        }
        on.exit({
            options(graphics.record = FALSE)
            par(ask = FALSE)
        })
    }
    invisible()
}

