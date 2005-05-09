"GHpoints" <-
function(k, factors, inter=FALSE, quad.z1=FALSE, quad.z2=FALSE){
    GH <- gauher(k)
    grid.t <- as.matrix(expand.grid(lapply(1:factors, function(k, u) u$x, u = GH)))
    grid.w <- as.matrix(expand.grid(lapply(1:factors, function(k, u) u$w, u = GH)))
    dimnames(grid.t) <- dimnames(grid.w) <- NULL
    grid.w <- apply(grid.w, 1, prod) * exp(rowSums(grid.t^2))
    if(inter) grid.t <- cbind(grid.t, grid.t[, 1] * grid.t[, 2])
    if(quad.z1) grid.t <- cbind(grid.t, grid.t[, 1] * grid.t[, 1])
    if(quad.z2) grid.t <- cbind(grid.t, grid.t[, 2] * grid.t[, 2])
    list(x=grid.t, w=grid.w)
}

