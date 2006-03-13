"anova.rasch" <-
function (object, object2 = NULL, B = 49, ...) {
    if (!inherits(object, "rasch"))
        stop("Use only with 'rasch' objects.\n")
    if (!is.null(object2)){
        ind.rasch.obj2 <- inherits(object2, "rasch")
        ind.ltm.obj2 <- inherits(object2, "ltm")
        if (!ind.rasch.obj2 && !ind.ltm.obj2)
            stop("\n", deparse(substitute(object2)), " must inherit from either class 'rasch' or class 'ltm'.")
        if (ind.rasch.obj2 && is.null(object$constraint))
            stop("\n", deparse(substitute(object)), " must be a constrained Rasch model in order to be nested in ", 
                        deparse(substitute(object2)))
        if (!isTRUE(all.equal(object$X, object2$X)))
            warning("it seems that the two objects represent models fitted in different data sets.")
        L0 <- object$log.Lik
        L1 <- object2$log.Lik
        nb0 <- nrow(object$coef) + 1
        nb1 <- if (ind.rasch.obj2) nrow(object2$coef) + 1 else length(object2$coef)
        if (!is.null(constr <- object$constraint))
            nb0 <- nb0 - nrow(constr)
        if (!is.null(constr <- object2$constraint))
            nb1 <- nb1 - nrow(constr)
        df. <- nb1 - nb0
        if (df. < 0)
            stop(deparse(substitute(object)), " is not nested in ", deparse(substitute(object2)))
        LRT <- -2 * (L0 - L1)
        if (LRT < 0)
            warning("it seems that the two models are not nested.")
        p.value <- pchisq(LRT, df., lower.tail = FALSE)
        out <- list(nam0 = deparse(substitute(object)), L0 = L0, aic0 = -2 * L0 + 2 * nb0, 
                bic0 = -2 * L0 + log(nrow(object$X)) * nb0, nam1 = deparse(substitute(object2)), L1 = L1, 
                aic1 = -2 * L1 + 2 * nb1, bic1 = -2 * L1 + log(nrow(object2$X)) * nb1, LRT = LRT, df = df., 
                p.value = p.value, call = object$call)
    } else {
        if (any(is.na(object$X)) && is.null(object$na.action))
            stop("In presense of missing values the parametric Bootstrap test is not currently implemented.\n")
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
            Ts[i] <- pearson.chi(rasch(X, constraint = constraint, control = object$control))
        }
        p.val <- (1 + sum(Ts >= Tobs)) / (B + 1)
        out <- list(Tobs = Tobs, p.value = p.val, B = B, call = object$call)
    }
    class(out) <- "aov.rasch"
    out
}

