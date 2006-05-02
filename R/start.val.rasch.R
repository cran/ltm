"start.val.rasch" <-
function (start.val, data) {
    n <- nrow(data)
    p <- ncol(data)
    computeStartVals <- function (start.val) {
        ind <- if (!is.null(start.val)) {
            if (length(start.val) == 1 && start.val == "random")
                return(c(compute = TRUE, random = TRUE))
            if (length(start.val) != p + 1) {
                warning("'start.val' not of proper dimensions.\n")
                return(c(compute = TRUE, random = FALSE))
            } else
                FALSE      
        } else 
            TRUE
        c(compute = ind, random = FALSE)
    }
    comp <- computeStartVals(start.val)
    if (comp["compute"]) {
        z <- if (comp["random"]) {
            cbind(1, rnorm(n))
        } else {
            rs <- rowSums(data)
            cbind(1, seq(-3, 3, len = length(unique(rs)))[rs + 1])
        }
        coefs <- matrix(0, p, 2)
        for (i in 1:p) {
            coefs[i, ] <- glm.fit(z, data[, i], family = binomial())$coef
        }
        c(coefs[, 1], mean(coefs[, 2]))
    } else 
        start.val    
}

