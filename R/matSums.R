"matSums" <-
function (lis) 
{
    res <- array(data=0.0, dim=dim(lis[[1]]))
    for(i in seq(along=lis)) res <- res + lis[[i]]
    res
}

