"matSums" <-
function (lis, na.rm = FALSE) 
{
    if (!is.list(lis) || !all(sapply(lis, is.matrix))) 
        stop("'lis' must be a list containing 2-dimensional arrays")
    dims <- sapply(lis, dim)
    n <- dims[1, 1]
    p <- dims[2, 1]
    if (!all(n == dims[1, ]) || !all(p == dims[2, ])) 
        stop("the matrices must have the same dimensions")
    mat <- matrix(unlist(lis), n * p, length(lis))
    matrix(rowSums(mat, na.rm = na.rm), n, p)
}

