"plot.rasch" <-
function(x, items = NULL, legend = FALSE, cx = -3.4, cy = 0.9, ...){
    if(!inherits(x, "rasch"))
        stop("Use only with 'rasch' objects.\n")
    betas <- coef(x)
    p <- nrow(betas)
    itms <- if(!is.null(items)){
                if(!is.numeric(items) || length(items) > p)
                    stop("'items' must be a numeric vector of length at most ", p)
                if(any(items < 1 | items > p))
                    stop("'items' must contain numbers between 1 and ", p, " denoting the items")
                items
            } else
                1:p
    z1 <- seq(-3.4, 3.4, length = 100)
    Z <- cbind(1, z1)
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
    invisible()
}

