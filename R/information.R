"information" <-
function (object, range, items = NULL, ...) {
    if (!class(object) %in% c("grm", "ltm", "rasch"))
        stop("'object' must inherit from either class 'grm', class 'ltm' or class 'rasch'.\n")
    p <- ncol(object$X)
    items <- if (!is.null(items)) {
        if (!is.numeric(items) && length(items) > p)
            stop("'items' should be a numeric vector of maximum length ", p, ".\n")
        if (any(!items %in% 1:p))
            stop("'items' should contain numbers in: ", paste(1:p, collapse = ", "), " indicating the items.\n")
        items
    } else 
        1:p
    f <- function (z) {
        switch(class(object),
            "grm" = rowSums(infoprobs(object$coefficients, z)[, items, drop = FALSE]),
            "ltm" = { betas <- object$coefficients; Z <- cbind(1, z);
                mat <- t(t(plogis(Z %*% t(betas)) * (1 - plogis(Z %*% t(betas)))) * betas[, 2]^2);
                rowSums(mat[, items, drop = FALSE])
                },
            "rasch" = { betas <- object$coefficients; Z <- cbind(1, z);
                mat <- betas[1, 2]^2 * plogis(Z %*% t(betas)) * (1 - plogis(Z %*% t(betas)));
                rowSums(mat[, items, drop = FALSE])
                })
    }
    I0 <- integrate(f, -10, 10, ...)$value
    I1 <- integrate(f, lower = range[1], upper = range[2], ...)$value
    list("Info in range" = I1, "Total Info" = I0, "Proportion in range" = I1/I0)
}

