\name{coef.rasch}
\alias{coef.rasch}
\title{ Loadings for rasch objects }
\description{
  Extracts the estimated parameters from an \code{rasch} object.
}
\usage{
\method{coef}{rasch}(object, prob = FALSE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ an object inheriting from class \code{rasch}. }
  \item{prob}{ logical; if \code{TRUE} the probability of a positive response for the median individual
        (i.e., \eqn{Pr(x_i = 1 | z = 0)}, with \eqn{i = 1, \ldots, p}{i = 1, ..., p} denoting the items) 
        is also returned.}
  \item{\dots}{ additional arguments; currently none is used. }
}
\value{The matrix of the estimated parameters for the fitted model.}
\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}
\seealso{ \code{\link{rasch}} }
\examples{

m1 <- rasch(Lsat)
coef(m1, TRUE)

m2 <- rasch(Abortion)
coef(m2)

}
\keyword{methods}
