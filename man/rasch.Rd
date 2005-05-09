\name{rasch}
\alias{rasch}
\title{ Rasch Model }
\description{
  Fit the Rasch model under the Item Response Theory approach.
}
\usage{
rasch(data, constraint = NULL, start.val, na.action = NULL, control = list())
}
\arguments{
  \item{data}{a data.frame (that will be converted to a numeric matrix using 
        \code{data.matrix()}) or a numeric matrix of manifest variables. The binary responses
        must be in 0/1 format.}
  \item{constraint}{a two-column numeric matrix with at most \eqn{p} rows (where \eqn{p} is the number of items), 
        specifying fixed-value constraints. The first column represents the item (i.e., \eqn{1} denotes the first item,
        \eqn{2} the second, etc., and \eqn{p+1} the discrimination parameter) and the second column the 
        value at which the corresponding parameter should be fixed. See \bold{Examples} for more info.}
  \item{start.val}{ a numeric vector of \eqn{p+1} starting values for the algorithm. The first \eqn{p}
        values correspond to the easiness parameters while the last value corresponds to the discrimination 
        parameter. If it is not supplied randomly chosen starting values are used instead.}
  \item{na.action}{ the \code{na.action} to be used on \code{dat}. In case of missing data, if 
        \code{na.action=NULL} the model uses the available cases, i.e., it takes into account the observed 
        part of sample units with missing values (valid under MAR mechanisms if the model is correctly specified). 
        If you want to apply a complete case analysis then use \code{na.action=na.exclude}.}
  \item{control}{a list of control values,
    \describe{
        \item{iter.qN}{ the number of quasi-Newton iterations. Default 150.}
        \item{GHk}{ the number of Gauss-Hermite quadrature points. Default 21.}
        \item{method}{ the optimization method to be used in \code{optim}. Default "BFGS".}
        \item{verbose}{ logical; if \code{TRUE} info about the optimization procedure are printed.}
    }
  }
}
\details{ 
    The Rasch model is special case of the unidimensional latent trait model when all the discrimination 
    parameters are equal. This model was first discussed by Rasch (1960) and it is used mainly in educational 
    testing where the aim is to study the abilities of a particular set of individuals.

    The model is defined as follows \deqn{\log\left(\frac{\pi_i}{1-\pi_i}\right) = \beta_{i} + \beta z,}{ logit
    (\pi_i) = beta_{i} + beta z, } where \eqn{\pi_i} denotes the probability of responding correctly to the \eqn{i}th 
    item, \eqn{\beta_{i}}{beta_{i}} is the easiness parameter for the \eqn{i}th item, \eqn{\beta} is the 
    discrimination parameter (the same for all the items) and \eqn{z} denotes the latent ability.

    The fit of the model is based on approximate marginal Maximum Likelihood, using the Gauss-Hermite quadrature rule 
    for the approximation of the required integrals. The optimization algorithm works under the constraint that 
    the discrimination parameter is always positive.
}
\value{ An object of class \code{rasch} with components,
    \item{coefficients}{the parameter values at convergence.}
    \item{log.Lik}{the log-likelihood value at convergence.}
    \item{convergence}{the convergence identifier returned by \code{optim}.}
    \item{hessian}{the approximate Hessian matrix at convergence returned by \code{optim}.}
    \item{patterns}{a list with two components: (i) \code{mat} a numeric matrix 
    that contains the observed response patterns. (ii) \code{dat} a data.frame that contains the observed and expected 
    frequencies for each observed response pattern.}
    \item{GH}{a list with two components used in the Gauss-Hermite rule: (i) \code{Z} a numeric matrix that contains 
    the quadrature points. (ii) \code{GHw} a numeric vector that contains the corresponding  weights.} 
    \item{max.sc}{the maximum absolute value of the score vector at convergence.}
    \item{X}{the responses data matrix.}
    \item{control}{the values used in the \code{control} argument.}
    \item{call}{the matched call.}
}
\section{Warning}{ 
In case the Hessian matrix at convergence is not positive definite, try
to re-fit the model. \code{rasch} will use new random starting values.
}
\references{
Baker, F. and Kim, S-H. (2004) \emph{Item Response Theory}, 2nd ed. 
New York: Marcel Dekker.

Rasch, G. (1960) \emph{Probabilistic Models for Some 
Intelligence and  Attainment Tests}. Copenhagen: Paedagogiske 
Institute.
}
\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}
\seealso{
  \code{\link{coef.rasch}},
  \code{\link{summary.rasch}},
  \code{\link{anova.rasch}},
  \code{\link{plot.rasch}},
  \code{\link{vcov.rasch}},
  \code{\link{margins}},
  \code{\link{factor.scores}}
}
\examples{

## The Rasch model for the Wirs data:
rasch(Wirs)

## The Rasch model for the Wirs data, under the 
## constraint that: (i) the easiness parameter 
## for the first item equals 0.5, (ii) the easiness 
## parameter for the second item equals 1.2, and 
## (iii) the discrimination parameter equals 1
(constr <- rbind(c(1, 0.5), c(2, 1.2), c(7, 1)))
rasch(Wirs, constraint = constr)


## The Rasch model for the Lsat data:
rasch(Lsat)

## The Rasch model for the Lsat data, under the 
## constraint that the discrimination parameter
## equals 1
rasch(Lsat, constraint = rbind(c(6,1)))


## The Rasch model for the Abortion data:
rasch(Abortion)

}
\keyword{multivariate}
\keyword{regression}
