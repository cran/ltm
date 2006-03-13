"anova.ltm" <-
function (object, object2, ...) {
    if (!inherits(object, "ltm"))
        stop("Use only with 'ltm' objects.\n")
    if (missing(object2))
        stop("anova.ltm() computes LRTs between two fitted latent trait models.")
    if (!inherits(object2, "ltm"))
        stop(deparse(substitute(object2)), " must inherit from class `ltm'.")
    if (!isTRUE(all.equal(object$X, object2$X)))
        warning("it seems that the two models are fitted in different data sets.")
    L0 <- object$log.Lik
    L1 <- object2$log.Lik
    nb0 <- if (!is.null(constr <- object$constraint)) length(object$coef) - nrow(constr) else length(object$coef)
    nb1 <- if (!is.null(constr <- object2$constraint)) length(object2$coef) - nrow(constr) else length(object2$coef)
    df. <- nb1 - nb0
    if (df. < 0)
        stop(deparse(substitute(object)), " is not nested in ", deparse(substitute(object2)))
    LRT <- -2 * (L0 - L1)
    if (LRT < 0)
        warning("either ", deparse(substitute(object)), " is not nested in ", deparse(substitute(object2)), " or ",  
                    deparse(substitute(object2)), " fell on a local maxima.\n")
    old <- options(warn = (-1))
    on.exit(options(old))
    p.value <- pchisq(LRT, df., lower.tail = FALSE)
    out <- list(nam0 = deparse(substitute(object)), L0 = L0, aic0 = -2 * L0 + 2 * nb0, 
            bic0 = -2 * L0 + log(nrow(object$X)) * nb0, nam1 = deparse(substitute(object2)), L1 = L1, 
            aic1 = -2 * L1 + 2 * nb1, bic1 = -2 * L1 + log(nrow(object2$X)) * nb1, LRT = LRT, df = df., 
            p.value = p.value)
    class(out) <- "aov.ltm"
    out
}

