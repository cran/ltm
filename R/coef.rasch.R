"coef.rasch" <-
function (object, ...) 
{
    cof <- object$coef
    dimnames(cof) <- list(paste("Item", 1:nrow(cof)), c("Difficulty", "Discrimination"))
    round(cof, 2)
}

