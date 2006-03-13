"coef.grm" <-
function (object, ...) {
    if (!inherits(object, "grm"))
        stop("Use only with 'grm' objects.\n")
    coefs <- if (object$IRT.param) IRT.parm(object, digits.abbrv = object$control$digits.abbrv)$parms else object$coef
    if (all(sapply(coefs, length) == length(coefs[[1]])))
        coefs <- do.call(rbind, coefs)
    print(round(coefs, 3), print.gap = 2, quote = FALSE)
}

