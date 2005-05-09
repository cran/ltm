"coef.rasch" <-
function(object, ...) {
    if(!inherits(object, "rasch")) stop("Use only with 'rasch' objects.\n")
    if(is.null(cof <- object$coef)) cat("\n No Coefficients.\n") else round(cof, 2)
}

