"betas.rasch" <-
function(betas, constraint, p){
    if(!is.null(constraint)){
        betas. <- numeric(p+1)
        betas.[constraint[, 1]] <- constraint[, 2]
        betas.[-constraint[, 1]] <- betas
        return(cbind(betas.[1:p], abs(betas.[p + 1])))
    } else{
        return(cbind(betas[1:p], abs(betas[p + 1])))
    }
}

