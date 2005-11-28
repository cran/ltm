
## Here is a sample analysis of the Lsat data using the Rasch model


## First some descriptives for Lsat

descript(Lsat)


## First we fit the original form of the Rasch model assuming
## fixed discrimination parameter equal to 1; results are reported
## under the usual IRT parameterization; in order to fix the
## discrimination parameter the 'constraint' argument is used

m1 <- rasch(Lsat, IRT = TRUE, constr = cbind(length(Lsat) + 1, 1))

summary(m1)


## In order to check the fit of the model the anova() function 
## is used; This computes a Bootstrap p-value for the Pearson's 
## Chi-squared statistic

anova(m1, B = 100) # B specifies the number of Bootstrap samples


## Alternatively, we could check also the fit on the margins

margins(m1)

margins(m1, "three-way")


## The Item Characterstic Curves are produced by the plot() function

par(mfrow = c(2, 2))
plot(m1, lwd = 3, cex = 1.2)
# or
plot(m1, legend = TRUE, lwd = 3, cx = 1, cy = 0.7) # 'cx' and 'cy' define the coordinates of the legend




## We repeat the analysis without constaining discrimination parameter

m2 <- rasch(Lsat, IRT = TRUE)

summary(m2)


## The Goodness-of-Fit is checked again

anova(m2, B = 100) # B specifies the number of Bootstrap samples


## The fit on the margins

margins(m2)

margins(m2, "three-way")


## The Likelihood Ratio Test of the two models is computed again with
## the anova() function; remember to put first the model under the null
## hypothesis -- in this case the constrained Rasch model m1

anova(m1, m2)


## The Item Characterstic Curves for the unconstrained model

plot(m2, lwd = 3, cex = 1.2)
# or
plot(m2, legend = TRUE, lwd = 3, cx = 1, cy = 0.7)


## Finally, the ability estimates can be obtained using the factor.scores()
## function

factor.scores(m2)
# or
factor.scores(m2, method = "MI", B = 20) # using Multiple Imputation
