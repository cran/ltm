"plot.ltm" <-
function(x, items = NULL, legend = FALSE, cx = -3.4, cy = 0.9, ...){
    if(!inherits(x, "ltm"))
        stop("Use only with 'ltm' objects.\n")
    betas <- x$coef
    p <- nrow(betas)
    itms <- if(!is.null(items)){
                if(!is.numeric(items) || length(items) > p)
                    stop("'items' must be a numeric vector of length at most ", p)
                if(any(items < 1 | items > p))
                    stop("'items' must contain numbers between 1 and ", p, " denoting the items")
                items
            } else
                1:p
    np <- 100
    if(x$ltst$factors == 1){
        z1 <- seq(-3.4, 3.4, length = np)
        Z <- if(x$ltst$quad.z1) cbind(1, z1, z1 * z1) else cbind(1, z1)
        pr <- plogis(Z %*% t(betas))
        plot(c(-3.4, 3.4), c(0, 1), type = "n", xlab = "Ability", ylab = "Probability", 
                main = "Item Characteristic Curves", ...)
        if(legend)
            legend(x = cx, y = cy, legend = rownames(x$coef)[itms], col = (1:p)[itms], lty = 1, bty = "n", ...) 
        else{
            z.pos <- seq(-3.2, 3.2, length = p)
            prob.pos <- seq(1, nrow(pr), length = p)
            nams <- if(rownames(x$coef)[1] == "Item 1") 1:p else rownames(x$coef)
        }
        for(it in itms){
            lines(z1, pr[, it], col = it, ...)
            if(!legend)
                text(z.pos[it], pr[prob.pos[it], it], labels = nams[it], pos = 4, col = it, ...)            
        }
    }
    if(x$ltst$factors == 2) {
        old <- options(graphics.record = TRUE)
        nams <- rownames(x$coef)
        if(!any(unlist(x$ltst[2:4]))) {
            cof <- coef(x, TRUE)
            z1 <- cof[itms, 3]
            z2 <- cof[itms, 5]
            plot(z1, z2, type = "n", xlab = "Factor 1", ylab = "Factor 2", main = "Standardized Loadings", 
                    xlim = c(min(z1, -0.1), max(z1, 0.1)), ylim = c(min(z2, -0.1), max(z2, 0.1)))
            abline(h = 0, v = 0, lty = 2)
            text(z1, z2, labels = nams[itms])
        }
        z1 <- seq(-3.4, 3.4, length = np)
        z2 <- seq(-3.4, 3.4, length = np)
        f <- function(z, betas, strct) {
            Z <- cbind(1, z[1], z[2])
            colnames(Z) <- c("(Intercept)", "z1", "z2")
            if(strct$inter)
                Z <- cbind(Z, "z1:z2" = z[1] * z[2])
            if(strct$quad.z1)
                Z <- cbind(Z, "I(z1^2)" = z[1] * z[1])
            if(strct$quad.z2)
                Z <- cbind(Z, "I(z2^2)" = z[2] * z[2])
            Z <- Z[, match(names(betas), colnames(Z)), drop = FALSE]
            pr <- plogis(Z %*% betas)
        }
        grid. <- as.matrix(expand.grid(z1, z2))
        dimnames(grid.) <- NULL
        par(ask = TRUE)
        for (it in itms) {
            z <- apply(grid., 1, f, betas = betas[it, ], strct = x$ltst)
            dim(z) <- c(np, np)
            persp(z1, z2, z, ticktype = "detailed", theta = 30, phi = 30, expand = 0.5, d = 2, cex = 0.7, 
                    xlab = "Factor 1", ylab = "Factor 2", 
                    zlab = "Probability", main = list("Item Characteristic Surfaces", cex = 1.5), 
                    sub = list(nams[it], cex = 1.4), ...)
        }
        on.exit({
            options(graphics.record = FALSE)
            par(ask = FALSE)
        })
    }
    invisible()
}

