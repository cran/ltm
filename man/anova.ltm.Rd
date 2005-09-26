\name{anova.ltm}
\alias{anova.ltm}
\title{ Anova method for ltm objects }
\description{
  Performs a Likelihood Ratio Test between two nested \code{ltm} objects. 
}
\usage{
\method{anova}{ltm}(object, object2, \dots)
}
\arguments{
  \item{object}{ an object inheriting from class \code{ltm} representing the model
                under the null hypothesis.}
  \item{object2}{ an object inheriting from class \code{ltm} representing the model
                under the alternative hypothesis.}
  \item{\dots}{ additional arguments; currently none is used.}
}
\value{
An object of class \code{aov.ltm} with components,
    \item{nam0}{the name of \code{object}.}
    \item{L0}{the log-likelihood under the null hypothesis (\code{object}).}
    \item{aic0}{the AIC value for the model given by \code{object}.}
    \item{bic0}{the BIC value for the model given by \code{object}.}
    \item{nam1}{the name of \code{object2}.}
    \item{L1}{the log-likelihood under the alternative hypothesis (\code{object2}).}
    \item{aic1}{the AIC value for the model given by \code{object2}.}
    \item{bic1}{the BIC value for the model given by \code{object2}.}
    \item{LRT}{the value of the Likelihood Ratio Test statistic.}
    \item{df}{the degrees of freedom for the test (i.e., the difference in the number of parameters).}
    \item{p.value}{the \eqn{p}-value of the test.}
}
\section{Warning}{
    The code does not check if the models are nested! The user is responsible to supply nested models in
    order the LRT to be valid.
}
\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}
\seealso{ \code{\link{ltm}} }
\examples{

## LRT between the one- and two-factor models 
## for the Wirs data:
anova(ltm(Wirs ~ z1), ltm(Wirs ~ z1 + z2))


## LRT between the two-factor and the interaction 
## models for the Mobility data:
anova(ltm(Mobility ~ z1 + z2), ltm(Mobility ~ z1 * z2))

}
\keyword{methods}
