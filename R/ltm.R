"ltm" <-
function (formula, start.val, constraint = NULL, na.action = NULL, 
    control = list()) 
{
    cl <- match.call()
    av <- all.vars(formula)
    X <- get(av[1], env = parent.frame())
    X <- data.matrix(X)
    if (!is.null(na.action)) 
        X <- na.action(X)
    dimnames(X) <- NULL
    if (any(!av[-1] %in% c("z1", "z2"))) 
        stop("\nYou have to use `z1' and/or `z2' to denote the factors.")
    factors <- length(av[-1])
    if (factors > 2) 
        stop("\nMaximum number of factors to include is 2.")
    tm <- attr(terms(formula), "term.labels")
    inter <- "z1:z2" %in% tm
    quad.z1 <- "I(z1^2)" %in% tm
    quad.z2 <- "I(z2^2)" %in% tm
    betas <- if (missing(start.val)) 
        start.values(X, factors, inter, quad.z1, quad.z2)
    else start.val
    if (!is.null(constraint)) {
        if (length(constraint) != 3) {
            constraint <- NULL
            warning("constraint must be a vector of length 3, see ?ltm for more info. `constraint' is set to NULL\n")
        }
        if (constraint[1] > ncol(X) || constraint[2] > 2) {
            constraint <- NULL
            warning("not acceptable values for the constraint, see `?ltm' for more info. `constraint' is set to NULL\n")
        }
    }
    con <- list(iter.em = 40, iter.qN = 150, GHk = 15, method = "BFGS", 
        verbose = FALSE)
    con[names(control)] <- control
    fit <- ltm.fit(X, betas, constraint, factors, inter, quad.z1, 
        quad.z2, con)
    fit$X <- X
    fit$ltn.struct <- c("z1", "z2", "z1:z2", "z1^2", "z2^2")[c("z1" %in% 
        av, "z2" %in% av, inter, quad.z1, quad.z2)]
    fit$control <- con
    fit$call <- cl
    class(fit) <- "ltm"
    fit
}

