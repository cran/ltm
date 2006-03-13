"information" <-
function (object, range, ...) {
    if (!class(object) %in% c("grm", "ltm", "rasch"))
        stop("'object' must inherit from either class 'grm', 'ltm' or 'rasch'.\n")
    f <- function (z) {
        switch(class(object),
            "grm" = rowSums(infoprobs(object$coefficients, z)),
            "ltm" = { betas <- object$coefficients; Z <- cbind(1, z);
                rowSums(t(t(plogis(Z %*% t(betas)) * (1 - plogis(Z %*% t(betas)))) * betas[, 2]^2)) 
                },
            "rasch" = { betas <- object$coefficients; Z <- cbind(1, z); 
                rowSums(betas[1, 2]^2 * plogis(Z %*% t(betas)) * (1 - plogis(Z %*% t(betas))))
                })
    }
    I0 <- integrate(f, -10, 10, ...)$value
    I1 <- integrate(f, lower = range[1], upper = range[2], ...)$value
    list("Info in range" = I1, "Total Info" = I0, "Proportion in range" = I1/I0)
}

