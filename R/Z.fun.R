"Z.fun" <-
function (z, inter, quad.z1, quad.z2) 
{
    if (is.matrix(z)) {
        if (inter) 
            z <- cbind(z, z[, 1] * z[, 2])
        if (quad.z1) 
            z <- cbind(z, z[, 1] * z[, 1])
        if (quad.z2) 
            z <- cbind(z, z[, 2] * z[, 2])
    }
    else {
        if (inter) 
            z <- c(z, z[1] * z[2])
        if (quad.z1) 
            z <- c(z, z[1] * z[1])
        if (quad.z2) 
            z <- c(z, z[2] * z[2])
    }
    return(z)
}

