\name{summary.rasch}
\alias{summary.rasch}

\title{ Summary method for rasch objects }

\description{
    Summarizes the fit of a \code{rasch} object.
}

\usage{
    \method{summary}{rasch}(object, robust.se = FALSE, \dots)
}

\arguments{
  \item{object}{ an object inheriting from class  \code{rasch}. }
  \item{robust.se}{logical; if \code{TRUE} robust estimation of standard errors is used, based on the sandwich estimator.}
  \item{\dots}{ additional argument; currently none is used. }
}

\note{
  For the parameters that have been constrained, the standard errors and \eqn{z}-values are printed as \code{NA}.
  
  When the coefficients' estimates are reported under the usual IRT parameterization (i.e., \code{IRT.param = TRUE}
  in the call of \code{rasch}), their standard errors are calculated using the Delta method.
}

\value{
  An object of class \code{summ.rasch} with components,
  \item{coefficients}{the estimated coefficients' table.}
  \item{Var.betas}{the approximate covariance matrix for the estimated parameters.}
  \item{logLik}{the log-likelihood of \code{object}.}
  \item{AIC}{the AIC for \code{object}.}
  \item{BIC}{the BIC for \code{object}.}
  \item{max.sc}{the maximum absolute value of the score vector at convergence.}
  \item{conv}{the convergence identifier returned by \code{optim}.}
  \item{counts}{the \code{counts} argument returned by \code{optim}.}
  \item{call}{the matched call of \code{object}.}
  \item{control}{the values used in the \code{control} argument in the fit of \code{obj}.}
  \item{nitems}{the number of items in the data set.}
}

\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}

\seealso{
    \code{\link{rasch}}
}

\examples{

## The Rasch model for the Wirs data,
## results are reported under the IRT
## parameterization: 
m <- rasch(Wirs, IRT = TRUE)
summary(m)


## The Rasch model for the Lsat data,
## with the constraint that the discrimination
## parameter equals 1:
m <- rasch(Lsat, constraint = cbind(6, 1))
summary(m)


## The Rasch model for the Abortion data,
## use the sandwich estimator for the standard
## errors: 
m <- rasch(Abortion)
summary(m, TRUE)

}

\keyword{methods}
