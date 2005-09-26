"ltm" <-
function (formula, constraint = NULL, IRT.param = FALSE, start.val, na.action = NULL, control = list()) {
    cl <- match.call()
    av <- all.vars(formula)
    X <- get(av[1], env = parent.frame())
    X <- data.matrix(X)
    if(!is.null(na.action))
        X <- na.action(X)
    factors <- length(av <- av[-1])
    if(factors > 2)
        stop("\nMaximum number of factors to include is 2.")
    if( (factors == 1 & av != "z1") || (factors == 2 & any(!av[-1] %in% c("z1", "z2"))) )  
        stop("\nyou have to use 'z1' for the first factor and 'z2' for the second one.")
    tm <- attr(terms(formula), "term.labels")
    ltst <- list(factors = factors, inter = "z1:z2" %in% tm, quad.z1 = "I(z1^2)" %in% tm, quad.z2 = "I(z2^2)" %in% tm)
    if(IRT.param && (factors > 1 | ltst$quad.z1))
        warning("conversion to the IRT parameterization works only for the two-parameter logistic model.\n")
    p <- ncol(X)
    q. <- 1 + factors + sum(unlist(ltst[-1]))
    betas <- if(!missing(start.val)){
        if(all(is.numeric(start.val), is.matrix(start.val), nrow(start.val) == p, ncol(start.val) == q.))
            start.val else  {
                warning("'start.val' must be a ", ncol(X), " by ", q., 
                                " numeric matrix; random starting values are used instead.\n")
                start.values(X, factors, formula)
            } 
    } else
        start.values(X, factors, formula)
    if(!is.null(constraint)){
        if((!is.numeric(constraint) | !is.matrix(constraint)) || (nrow(constraint) > p * q. - 1 | ncol(constraint) != 3))
            stop("'constraint' should be a 3-column numeric matrix with at most ", p * q. - 1, " rows (read help file).\n")
        if(any(constraint[, 1] < 1 | constraint[, 1] > p))
            stop("the 1st column of 'constraint' denotes the items and it should between 1 and ", p, " (read help file).\n")
        if(any(constraint[, 2] < 1 | constraint[, 2] > q.))
            stop("the 2nd column of 'constraint' denotes either the intercept or the factor loadings and it should between 1 and ", 
                    q., " (read help file).\n")
        constraint <- constraint[order(constraint[, 1]), , drop = FALSE]
        constraint[, 1:2] <- round(constraint[, 1:2])
        betas[constraint[, 1:2]] <- constraint[, 3]
    }
    con <- list(iter.em = 40, iter.qN = 150, GHk = 15, method = "BFGS", verbose = getOption("verbose"))
    con[names(control)] <- control
    fit <- ltm.fit(X, betas, constraint, formula, con)
    ltst$nams <- colnames(fit$coefficients)
    fit$ltst <- ltst
    fit$X <- X
    fit$control <- con
    fit$IRT.param <- IRT.param
    fit$constraint <- constraint
    fit$call <- cl
    class(fit) <- "ltm"
    fit
}

