\name{summary.ltm}
\alias{summary.ltm}
\title{ Summary method for ltm objects }
\description{
  Summarizes the fit of an \code{ltm} object.
}
\usage{
\method{summary}{ltm}(object, robust.se = FALSE, \dots)
}
\arguments{
  \item{object}{ an object inheriting from class  \code{ltm}. }
  \item{robust.se}{logical; if \code{TRUE} robust estimation of standard errors is used, based on the sandwich estimator.}
  \item{\dots}{ additional argument; currently none is used. }
}

\note{
    For the parameters that have been constrained, the standard errors and \eqn{z}-values are printed as \code{NA}.

    When the coefficients' estimates are reported under the usual IRT parameterization (i.e., \code{IRT.param = TRUE}
    in the call of \code{ltm}), their standard errors are calculated using the Delta method.
}

\value{
  An object of class \code{summ.ltm} with components,
  \item{coefficients}{the estimated coefficients' table.}
  \item{Var.betas}{the approximate covariance matrix for the estimated parameters.}
  \item{logLik}{the log-likelihood of \code{object}.}
  \item{AIC}{the AIC for \code{object}.}
  \item{BIC}{the BIC for \code{object}.}
  \item{max.sc}{the maximum absolute value of the score vector at convergence.}
  \item{conv}{the convergence identifier returned by \code{optim}.}
  \item{counts}{the \code{counts} argument returned by \code{optim}.}
  \item{call}{the matched call of \code{object}.}
  \item{ltn.struct}{a character vector describing the latent structure used in \code{object}.}
  \item{control}{the values used in the \code{control} argument in the fit of \code{object}.}
  \item{nitems}{the number of items in the data set.}
}

\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}

\seealso{
    \code{\link{ltm}}
}

\examples{

## One factor model using the WIRS data;
## results are reported under the IRT
## parameterization
m <- ltm(Wirs ~ z1, IRT = TRUE)
summary(m)
summary(m, TRUE)

}
\keyword{methods}
