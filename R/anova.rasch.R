"anova.rasch" <-
function (object, object2 = NULL, B = 49, ...) {
    if (!inherits(object, "rasch"))
        stop("Use only with 'rasch' objects.\n")
    out <- if (!is.null(object2)){
        if (!class(object2) %in% c("ltm", "rasch", "tpm"))
            stop("'object2' must inherit from either class 'ltm', 'rasch', or 'tpm'.\n")
        if (inherits(object2, "rasch") && is.null(object$constraint))
            stop("'object1' should be a constrained Rasch model.\n")
        if (!isTRUE(all.equal(object$X, object2$X)))
            warning("it seems that the two objects represent models fitted in different data sets.")
        L0 <- logLik(object)
        L1 <- logLik(object2)
        nb0 <- attr(L0, "df")
        nb1 <- attr(L1, "df")
        df. <- nb1 - nb0
        if (df. < 0)
            stop("'object' is not nested in 'object2'.\n")
        LRT <- - 2 * (L0 - L1)
        attributes(LRT) <- NULL
        if (LRT < 0)
            warning("either the two models are not nested or the model represented by 'object2' fell on a local maxima.\n")
        p.value <- pchisq(LRT, df., lower.tail = FALSE)
        list(nam0 = deparse(substitute(object)), L0 = L0, aic0 = AIC(object), bic0 = AIC(object, k = log(attr(L0, "n"))), 
             nam1 = deparse(substitute(object2)), L1 = L1, aic1 = AIC(object2), 
             bic1 = AIC(object2, k = log(attr(L1, "n"))), LRT = LRT, df = df., p.value = p.value, call = object$call)
    } else {
        if (any(is.na(object$X)) && is.null(object$na.action))
            stop("In the presense of missing values the parametric Bootstrap test is not currently implemented.\n")
        pearson.chi <- function (object) {
            Obs <- object$patterns$obs
            fits <- fitted(object)
            Exp <- fits[, ncol(fits)]
            if(any(ind <- Exp == 0))
                Exp[ind] <- 0.001
            sum((Obs - Exp)^2/Exp) + n - sum(Exp)
        }
        rmvlogis <- function (betas) {
            z <- rnorm(n)
            pr <- plogis(cbind(1, z) %*% t(betas))
            X <- matrix(0, n, p)
            for (i in 1:p)
                X[, i] <- ifelse(runif(n) < pr[, i], 1, 0)
            X
        }
        old <- options(warn = (-1))
        on.exit(options(old))
        constraint <- object$constraint
        betas <- object$coef
        betas <- c(betas[, -2], betas[1, 2])
        if (!is.null(constraint))
            betas <- betas[-constraint[, 1]]
        Var.betas <- vcov(object)
        n <- nrow(object$X)
        p <- ncol(object$X)
        Tobs <- pearson.chi(object)
        Ts <- numeric(B)
        for (i in 1:B) {
            betas. <- mvrnorm(1, betas, Var.betas)
            betas. <- betas.rasch(betas., constraint, p)
            X <- rmvlogis(betas.)
            Ts[i] <- pearson.chi(rasch(X, constraint = constraint, start.val = c(betas.[, 1], betas.[1, 2]), 
                                    control = object$control))
        }
        p.val <- (1 + sum(Ts >= Tobs)) / (B + 1)
        list(Tobs = Tobs, p.value = p.val, B = B, call = object$call)
    }
    class(out) <- "aov.rasch"
    out
}

