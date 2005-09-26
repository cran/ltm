\name{coef.ltm}
\alias{coef.ltm}
\title{ Loadings for ltm objects }
\description{
  Extracts the estimated parameters from an \code{ltm} object.
}
\usage{
\method{coef}{ltm}(object, standardized = FALSE, prob = FALSE, \dots)
}
\arguments{
  \item{object}{ an object inheriting from class \code{ltm}. }
  \item{standardized}{ logical; if \code{TRUE} the standardized loadings are also returned. See  \bold{Details} 
        for more info.}
  \item{prob}{ logical; if \code{TRUE} the probability of a positive response for the median individual
        (i.e., \eqn{Pr(x_i = 1 | z = 0)}, with \eqn{i = 1, \ldots, p}{i = 1, ..., p} denoting the items) 
        is also returned.}
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
\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}
\seealso{ \code{\link{ltm}}}
\examples{

m <- ltm(Lsat ~ z1)
coef(m, TRUE, TRUE)

}
\keyword{methods}
