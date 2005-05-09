\name{anova.ltm}
\alias{anova.ltm}
\title{ Anova method for ltm objects }
\description{
  Performs a Likelihood Ratio Test between two nested \code{ltm} objects. 
}
\usage{
\method{anova}{ltm}(object, obj2, \dots)
}
\arguments{
  \item{object}{ an object inheriting from class \code{ltm} representing the model
                under the null hypothesis.}
  \item{obj2}{ an object inheriting from class \code{ltm} representing the model
                under the alternative hypothesis.}
  \item{\dots}{ additional arguments; currently none is used.}
}
\value{
An object of class \code{aov.ltm} with components,
    \item{nam0}{the name of \code{object}.}
    \item{L0}{the log-likelihood under the null hypothesis (\code{object}).}
    \item{aic0}{the AIC value for the model given by \code{object}.}
    \item{bic0}{the BIC value for the model given by \code{object}.}
    \item{nam1}{the name of \code{obj2}.}
    \item{L1}{the log-likelihood under the alternative hypothesis (\code{obj2}).}
    \item{aic1}{the AIC value for the model given by \code{obj2}.}
    \item{bic1}{the BIC value for the model given by \code{obj2}.}
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

## LRT between the one- and two-factor model 
## for the Wirs data:
m1 <- ltm(Wirs~z1)
m2 <- ltm(Wirs~z1+z2)
anova(m1, m2)

## LRT between the two-factor and the interaction 
## model for the Mobility data:
m1 <- ltm(Mobility~z1+z2)
m2 <- ltm(Mobility~z1*z2)
anova(m1, m2)

}
\keyword{methods}
\keyword{ multivariate }
