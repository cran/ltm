\name{coef.rasch}
\alias{coef.rasch}
\title{ Loadings for rasch objects }
\description{
  Extracts the estimated parameters from an \code{rasch} object.
}
\usage{
coef.rasch(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ an object inheriting from class \code{rasch}. }
  \item{\dots}{ additional arguments; currently none is used. }
}
\value{The matrix of the estimated parameters for the fitted model.}
\seealso{ \code{\link{rasch}} }
\examples{

m1 <- rasch(Lsat)
coef(m1)

m2 <- rasch(Abortion)
coef(m2)

}
\keyword{ multivariate }
