\name{summary.rasch}
\alias{summary.rasch}
\title{ Summary method for rasch objects }
\description{
  Summarizes the fit of a \code{rasch} object.
}
\usage{
summary.rasch(object, robust.se = FALSE, ...)
}
\arguments{
  \item{object}{ an object inheriting from class  \code{rasch}. }
  \item{robust.se}{logical; if \code{TRUE} robust estimation of standard errors is used, based on the sandwich estimator.}
  \item{\dots}{ additional argument; currently none is used. }
}
\value{
  An object of class \code{summ.rasch} with components,
  \item{coefficients}{the estimated coefficients' table.}
  \item{Var.betas}{the covariance matrix for the estimated parameters.}
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
\seealso{ \code{\link{rasch}}}
\examples{

## The Rasch model for the Wirs data:
m <- rasch(Wirs)
summary(m)

## The Rasch model for the Lsat data:
m <- rasch(Lsat)
summary(m)

}
\keyword{multivariate}
