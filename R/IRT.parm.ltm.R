"IRT.parm.ltm" <-
function(object, standard.errors = FALSE, robust = FALSE, ...){
    if(!inherits(object, "ltm"))
        stop("Use only with 'ltm' objects.\n")
    if(object$ltst$factors > 1 || object$ltst$quad.z1)
        return(list(parms = object$coef, se = if(standard.errors) sqrt(diag(vcov(object, robust = robust))) else NULL))
    thetas <- object$coef
    parms <- cbind(-thetas[, 1] / thetas[, 2], thetas[, 2])
    dimnames(parms) <- list(rownames(thetas), abbreviate(c("Difficulty", "Discrimination"), 6))
    out <- list(parms = parms)
    out$se <- if(standard.errors){
        p <- nrow(thetas)
        constraint <- object$constraint
        if(!is.null(constraint) && any(constraint[, 2] == 2))
            warning("standard errors, under the IRT parameterization, cannot be computed for some of the difficulty \n\tparameters because the corresponding discrimination parameters are fixed.")
        ind <- (constraint[, 2] - 1) * p + constraint[, 1]
        Var <- if(!is.null(constraint)){
            V <- matrix(NA, 2 * p, 2 * p)
            V[seq(1, 2 * p)[-ind], seq(1, 2 * p)[-ind]] <- vcov(object, robust = robust)
            V
        } else
            vcov(object, robust = robust)
        ses <- rep(NA, p)
        for(i in seq(along = ses))
            ses[i] <- deltamethod(~ -x1 / x2, c(thetas[i, 1], thetas[i, 2]), Var[c(i, p + i), c(i, p + i)])
        ses <- c(ses, sqrt(diag(Var[seq(p + 1, 2 * p), seq(p + 1, 2 * p)])))
        if(!is.null(constraint))
            ses[-ind]
        else
            ses
    } else
        NULL
    out
}

