"anova.rasch" <-
function (object, ltm.obj = NULL, B = 49, ...) 
{
    if (!is.null(ltm.obj)) {
        if (!inherits(ltm.obj, "ltm")) 
            stop("\n", deparse(substitute(ltm.obj)), " must inherit from class `ltm'.")
        if (any(object$X != ltm.obj$X)) 
            stop("\nObjects are fitted in different data sets.")
        L0 <- object$log.Lik
        L1 <- ltm.obj$log.Lik
        nb0 <- length(unique(c(object$coef)))
        nb1 <- length(ltm.obj$coef)
        df. <- nb1 - nb0
        LRT <- -2 * (L0 - L1)
        p.value <- 1 - pchisq(LRT, df.)
        out <- list(nam0 = deparse(substitute(object)), L0 = L0, 
            aic0 = -2 * L0 + 2 * nb0, bic0 = -2 * L0 + log(nrow(object$X)) * 
                nb0, nam1 = deparse(substitute(ltm.obj)), L1 = L1, 
            aic1 = -2 * L1 + 2 * nb1, bic1 = -2 * L1 + log(nrow(ltm.obj$X)) * 
                nb1, LRT = LRT, df = df., p.value = p.value, 
            call = object$call)
    }
    else {
        pearson.chi <- function(object) {
            pats <- object$patterns$dat
            Obs <- pats$Obs
            Exp <- pats$Exp
            sum((Obs - Exp)^2/Exp) + n - sum(Exp)
        }
        rmvlogis <- function(betas, n, p) {
            z <- rnorm(n)
            pr <- plogis(cbind(1, z) %*% t(betas))
            X <- matrix(0, n, p)
            for (i in 1:p) X[, i] <- ifelse(runif(n) < pr[, i], 1, 0)
            X
        }
        old <- options(warn = (-1))
        on.exit(options(old))
        n <- nrow(object$X)
        betas <- object$coef
        Var.betas <- summary(object)$Var.betas
        p <- nrow(betas)
        Tobs <- pearson.chi(object)
        Ts <- numeric(B)
        for (i in 1:B) {
            betas. <- mvrnorm(1, unique(c(betas)), Var.betas)
            X <- rmvlogis(betas, n, p)
            Ts[i] <- pearson.chi(rasch(X, control = object$control))
        }
        p.val <- sum(c(Tobs, Ts) >= Tobs) / (B + 1)
        out <- list(Tobs = Tobs, p.value = p.val, B = B, call = object$call)
    }
    class(out) <- "aov.rasch"
    out
}

