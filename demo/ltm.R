
## This demo presents some feature of the ltm() function


## Fit the Rasch model using ltm()
ltm(Lsat ~ z1, constr = cbind(1:length(Lsat), 2, 1))
rasch(Lsat, constr = cbind(length(Lsat) + 1, 1))


## ltm() fits latent trait models up to two latent variables
fit <- ltm(Wirs ~ z1 + z2)

## use the following to produce the plot of standardized loadings,
plot(fit, type = "loadings")

## and use the following to produce the Item Characteristic Surfaces
plot(fit, ticktype = "detailed", theta = 30, phi = 30, expand = 0.5, d = 2, cex = 0.7)


## ltm() can also be used to include nonlinear latent terms
set.seed(123321) # because of the random starting values used
fit <- ltm(Mobility ~ z1 * z2)

plot(fit, ticktype = "detailed", theta = 30, phi = 30, expand = 0.5, d = 2, cex = 0.7)
