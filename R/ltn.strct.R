"ltn.strct" <-
function (x) 
{
    factors <- if ("z1" %in% x && "z2" %in% x) 
        2
    else 1
    inter <- "z1:z2" %in% x
    quad.z1 <- "z1^2" %in% x
    quad.z2 <- "z2^2" %in% x
    list(factors = factors, inter = inter, quad.z1 = quad.z1, 
        quad.z2 = quad.z2)
}

