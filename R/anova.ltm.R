"anova.ltm" <-
function (object, obj2, ...){
    if(!inherits(object, "ltm")) stop("Use only with 'ltm' objects.\n")
    if(!inherits(obj2, "ltm")) stop(deparse(substitute(obj2)), " must inherit from class `ltm'.")
    if(any(object$X != obj2$X)) warning("it seems that the two models are fitted in different data sets.")
    L0 <- object$log.Lik
    L1 <- obj2$log.Lik
    nb0 <- length(object$coef)
    nb1 <- length(obj2$coef)
    df. <- nb1 - nb0
    LRT <- -2 * (L0 - L1)
    if(LRT < 0) warning("either ", deparse(substitute(object)), " is not nested in ", deparse(substitute(obj2)), " or ",  deparse(substitute(obj2)), " fell on a local maxima.\n")
    old <- options(warn = (-1))
    on.exit(options(old))
    p.value <- 1 - pchisq(LRT, df.)
    out <- list(nam0=deparse(substitute(object)), L0=L0, aic0=-2*L0+2*nb0, bic0=-2*L0+log(nrow(object$X))*nb0, 
            nam1=deparse(substitute(obj2)), L1=L1, aic1=-2*L1+2*nb1, bic1=-2*L1+log(nrow(obj2$X))*nb1, 
            LRT = LRT, df = df., p.value = p.value)
    class(out) <- "aov.ltm"
    out
}

