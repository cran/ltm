\name{coef.ltm}
\alias{coef.ltm}
\title{ Loadings for ltm objects }
\description{
  Extracts the estimated parameters from an \code{ltm} object.
}
\usage{
coef.ltm(object, standardized = FALSE, ...)
}
\arguments{
  \item{object}{ an object inheriting from class \code{ltm}. }
  \item{standardized}{ logical; if \code{TRUE} the standardized loadings are also returned. See  Details for more info.}
  \item{\dots}{ additional arguments; currently none is used.}
}
\details{
  The standardization of the factor loadings is useful in order to form a link to the 
  Underlying Variable approach. In particular, the standardized form of the factor loadings
  represents the correlation coefficient between the latent variables and the underlying continuous variables
  based on which the dichotomous outcomes arise (see Bartholomew and Knott, 1999, p.87-88 or Bartholomew 
  \emph{et al.}, 2002, p.191).
  
  The standardized factor loadings are computed only for the linear one- and two-factor models.
}
\value{The matrix of the estimated parameters for the fitted model.}
\references{
Bartholomew, D. and Knott, M. (1999) \emph{Latent Variable Models
and Factor Analysis}, 2nd ed. London: Arnold.

Bartholomew, D., Steel, F., Moustaki, I. and Galbraith, J. (2002)
\emph{The Analysis and Interpretation of Multivariate Data for
Social Scientists}. London: Chapman and Hall.
}
\seealso{ \code{\link{ltm}}}
\examples{

m1 <- ltm(Lsat~z1)
coef(m1)
coef(m1, TRUE)

m2 <- ltm(Wirs~z1+z2)
coef(m2)
coef(m2, TRUE)

}
\keyword{ multivariate }
