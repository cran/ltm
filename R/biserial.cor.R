"biserial.cor" <-
function (x, y, use = c("all.obs", "complete.obs", "pairwise.complete.obs")) {
    if(class(x) != "numeric")
        stop("'x' must be a numeric variable")
    y <- as.factor(y)
    if(length(levs <- levels(y)) > 2)
        stop("\ny must be a dichotomous variable")
    if((nx <- length(x)) != (ny <- length(y)))
        stop("'x' and 'y' do not have the same length")
    bcor <- function (x, y, na.rm) {
        diff.mu <- as.vector(diff(tapply(x, y, mean, na.rm = na.rm)))
        prob <- mean(y == levs[1], na.rm = na.rm)
        diff.mu * sqrt(prob * (1 - prob)) / sd(x, na.rm = na.rm)
    }
    use <- match.arg(use)
    out <- if(use == "all.obs")
                bcor(x, y, na.rm = FALSE)
           else if(use == "complete.obs")
                bcor(x, y, na.rm = TRUE)
           else if(use == "pairwise.complete.obs"){
                dat <- data.frame(x, y)
                ind <- complete.cases(dat)
                x <- dat[ind, "x"]
                y <- dat[ind, "y"]
                bcor(x, y, na.rm = FALSE)
           }
    out
}

