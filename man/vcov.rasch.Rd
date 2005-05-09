\name{vcov.rasch}
\alias{vcov.rasch}
\title{ vcov method for rasch objects }
\description{
  Extracts the asymptotic variance-covariance matrix of the MLEs from a \code{rasch} object.
}
\usage{
\method{vcov}{rasch}(object, \dots)
}
\arguments{
  \item{object}{ an object inheriting from class \code{rasch}. }
  \item{\dots}{ additional arguments; currently none is used. }
}
\value{
a numeric matrix representing the estimated covariance matrix of the maximum likelihood estimates.
}
\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}
\examples{

m <- rasch(Wirs)
vcov(m)
sqrt(diag(vcov(m))) # standard errors
}
\keyword{methods}
\keyword{multivariate}
