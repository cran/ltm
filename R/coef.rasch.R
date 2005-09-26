"coef.rasch" <-
function(object, prob = FALSE, ...) {
    if(!inherits(object, "rasch"))
        stop("Use only with 'rasch' objects.\n")
    cof <- object$coef
    if(prob)
        cof <- cbind(cof, "P(x=1|z=0)" = plogis(cof[, 1]))
    round(cof, 3)
}

