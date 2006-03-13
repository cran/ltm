\name{vcov}
\alias{vcov.grm}
\alias{vcov.ltm}
\alias{vcov.rasch}
\title{ vcov method for fitted IRT models }
\description{
 Extracts the asymptotic variance-covariance matrix of the MLEs from either \code{grm},
 \code{ltm} or \code{rasch} objects.
}

\usage{

\method{vcov}{grm}(object, \dots)

\method{vcov}{ltm}(object, robust = FALSE, \dots)

\method{vcov}{rasch}(object, robust = FALSE, \dots)

}

\arguments{
  \item{object}{ an object inheriting from either class \code{grm}, class \code{ltm} or class \code{rasch}. }
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
    \code{\link{grm}},
    \code{\link{ltm}},
    \code{\link{rasch}}
}

\examples{

m <- grm(Science[c(1,3,4,7)], Hessian = TRUE)
vcov(m)

m <- rasch(Wirs)
vcov(m)
sqrt(diag(vcov(m))) # standard errors

}
\keyword{methods}
