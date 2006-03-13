"start.val.rasch" <-
function (start.val, data) {
    n <- nrow(data)
    p <- ncol(data)
    computeStartVals <- function (start.val) {
        ind <- if (!is.null(start.val)) {
            if (length(start.val) == 1 && start.val == "random")
                return(list(compute = TRUE, random = TRUE))
            if (length(start.val) != p + 1) {
                warning("'start.val' not of proper dimensions; random starting values are used instead.\n")
                TRUE
            } else
                FALSE      
        } else 
            TRUE
        list(compute = ind, random = FALSE)
    }
    comp <- computeStartVals(start.val)
    if (comp$compute) {
        if (comp$random)
            rnorm(p + 1)
        else
            NA # still under consideration
    } else 
        start.val    
}

