"plot.rasch" <-
function (x, cx = NULL, cy = NULL, ...) 
{
    betas <- coef(x)
    p <- nrow(betas)
    z1 <- seq(-3.4, 3.4, length = 100)
    Z <- cbind(1, z1)
    pr <- plogis(Z %*% t(betas))
    plot(c(-3.4, 3.4), c(0, 1), type = "n", xlab = "Factor", ylab = "probability", main = "Item Characteristic Curves")
    for (it in 1:p) lines(z1, pr[, it], col = it, ...)
    x <- if (is.null(cx)) 2.25 else cx
    y <- if (is.null(cy)) 0.5 else cy
    legend(x = x, y = y, legend = paste("Item", 1:p), col = 1:p, lty = 1)
    invisible()
}

