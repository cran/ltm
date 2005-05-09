"coef.ltm" <-
function (object, standardized=FALSE, ...){
    if(!inherits(object, "ltm")) stop("Use only with 'ltm' objects.\n")
    cof <- object$coef
    if(standardized){
        strct <- ltn.strct(object$ltn.struct)
        factors <- strct$factors
        if(any(strct$inter, strct$quad.z1, strct$quad.z2)){
            warning("standardized loadings are returned for the simple one- and two-factor models.\n")
            dimnames(cof) <- list(paste("Item", 1:nrow(cof)), c("(Intercept)", object$ltn.struct))
            return(round(cof, 2))
        }
        std.z <- cof[, -1] / sqrt(rowSums(cof[, -1, drop=FALSE] * cof[, -1, drop=FALSE]) + factors)
        if (factors == 1){
            cof <- cbind(cof, std.z)
            dimnames(cof) <- list(paste("Item", 1:nrow(cof)), c("(Intercept)", "z1", "std.z1"))
            return(round(cof, 2))
        }
        else{
            cof <- cbind(cof[, 1:2], std.z[, 1], cof[, 3], std.z[, 2])
            dimnames(cof) <- list(paste("Item", 1:nrow(cof)), c("(Intercept)", "z1", "std.z1", "z2", "std.z2"))
            return(round(cof, 2))
        }
    }
    dimnames(cof) <- list(paste("Item", 1:nrow(cof)), c("(Intercept)", object$ltn.struct))
    round(cof, 2)
}

