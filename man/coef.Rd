\name{coef}
\alias{coef.grm}
\alias{coef.ltm}
\alias{coef.rasch}
\title{ Extract Estimated Loadings }
\description{
  Extracts the estimated parameters from either \code{grm}, \code{ltm} or \code{rasch} objects.
}
\usage{

\method{coef}{grm}(object, \dots)

\method{coef}{ltm}(object, standardized = FALSE, prob = FALSE, \dots)

\method{coef}{rasch}(object, prob = FALSE, \dots)
}

\arguments{
  \item{object}{ an object inheriting from either class \code{grm}, class \code{ltm} or class \code{rasch}. }
  \item{standardized}{ logical; if \code{TRUE} the standardized loadings are also returned. See  \bold{Details} 
        for more info.}
  \item{prob}{ logical; if \code{TRUE} the probability of a positive response for the median individual
        (i.e., \eqn{Pr(x_i = 1 | z = 0)}, with \eqn{i = 1, \ldots, p}{i = 1, ..., p} denoting the items) 
        is also returned.}
  \item{\dots}{ additional arguments; currently none is used. }
}

\value{A list or a matrix of the estimated parameters for the fitted model.}

\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}

\seealso{
    \code{\link{grm}},
    \code{\link{ltm}},
    \code{\link{rasch}}
}

\examples{

m <- grm(Science[c(1,3,4,7)])
coef(m)

m <- ltm(Lsat ~ z1)
coef(m, TRUE, TRUE)

m <- rasch(Lsat)
coef(m, TRUE)

}

\keyword{methods}
