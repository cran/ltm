\name{vcov.ltm}
\alias{vcov.ltm}
\title{ vcov method for ltm objects }
\description{
 Extracts the asymptotic variance-covariance matrix of the MLEs from a \code{ltm} object.
}

\usage{
\method{vcov}{ltm}(object, robust = FALSE, \dots)
}
\arguments{
  \item{object}{ an object inheriting from class \code{ltm}. }
  \item{robust}{ logical; if \code{TRUE} the sandwich estimator is used. }
  \item{\dots}{ additional arguments; currently none is used. }
}

\value{
    a numeric matrix representing the estimated covariance matrix of the maximum likelihood estimates.
}

\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}

\seealso{
    \code{\link{ltm}}
}

\examples{

m <- ltm(Lsat ~ z1)
vcov(m)
sqrt(diag(vcov(m))) # standard errors

}
\keyword{methods}
