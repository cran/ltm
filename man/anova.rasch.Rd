\name{anova.rasch}
\alias{anova.rasch}
\title{ Anova method for rasch objects }
\description{
  Performs either a parametric Bootsrap goodness-of-fit test based on the
  Pearson chi-squared or a Likelihood Ratio Test between two nested models.
}
\usage{
\method{anova}{rasch}(object, object2 = NULL, B = 49, \dots)
}
\arguments{
  \item{object}{ an object inheriting from class \code{rasch}. }
  \item{object2}{ an object inheriting from either class \code{rasch} or class \code{ltm}. }
  \item{B}{ the number of Bootstrap samples. See \bold{Details} section for more info.}
  \item{\dots}{ additional arguments; currently none is used.}
}
\details{
  If only \code{object} is supplied \code{anova.rasch} performs a parametric 
  Bootstrap test based on Pearson's chi-squared statistic. The Bootstrap 
  approximation to the reference distribution is preferable compared with 
  the ordinary chi-squared approximation since the latter is not valid especially
  for large number of items (=> many response patterns with expected frequencies
  smaller than 1).
  
  In particular, the Bootstrap test is implemented as follows:
  \describe{
    \item{Step 0:}{Based on \code{object} compute the observed value of the statistic \eqn{T_{obs}}.}
    \item{Step 1:}{Simulate new parameter values, say \eqn{\theta^*}, from \eqn{N(\hat{\theta}, C(\hat{\theta}))},
    where \eqn{\hat{\theta}} are the MLEs and \eqn{C(\hat{\theta})} their large sample covariance 
    matrix.}
    \item{Step 2:}{Using \eqn{\theta^*} simulate new data (with the same dimensions as the 
    observed ones), fit the Rasch model and based on this fit calculate the value of the statistic \eqn{T_i}.}
    \item{Step 3:}{Repeat steps 1-2 \code{B} times and estimate the \eqn{p}-value using 
    \eqn{\sum\limits_{i=1}^B I(T_i  > T_{obs}) / (B+1).}{\{\# T_i  > T_{obs}\}.}}
  }
  
  If both \code{object} and \code{object2} are supplied, then a likelihood ratio test between the two fitted
  models is performed.  
}
\value{
An object of class \code{aov.rasch} with components,
    \item{Tobs}{the value of the Pearson's chi-squared statistic for the observed data. Returned if only \code{object} is supplied.}
    \item{B}{the \code{B} argument specifying the number of Bootstrap samples used. Returned if only \code{object} is supplied.}
    \item{call}{the matched call of \code{object}. Returned if only \code{object} is supplied.}
    \item{nam0}{the name of \code{object}. Returned if \code{object2} is also supplied.}
    \item{L0}{the log-likelihood under the null hypothesis (\code{object}). Returned if \code{object2} is also supplied.}
    \item{aic0}{the AIC value for the model given by \code{object}. Returned if \code{object2} is also supplied.}
    \item{bic0}{the BIC value for the model given by \code{object}. Returned if \code{object2} is also supplied.}
    \item{nam1}{the name of \code{object2}. Returned if \code{object2} is also supplied.}
    \item{L1}{the log-likelihood under the alternative hypothesis (\code{object2}). Returned if \code{object2} is also supplied.}
    \item{aic1}{the AIC value for the model given by \code{object2}. Returned if \code{object2} is also supplied.}
    \item{bic1}{the BIC value for the model given by \code{object2}. Returned if \code{object2} is also supplied.}
    \item{LRT}{the value of the Likelihood Ratio Test statistic. Returned if \code{object2} is also supplied.}
    \item{df}{the degrees of freedom for the test (i.e., the difference in the number of parameters). Returned if \code{object2} is also supplied.}
    \item{p.value}{the \eqn{p}-value of the test.}
}
\section{Warning}{
    The code does not check if the models are nested! The user is responsible to supply nested models in
    order the LRT to be valid.

    When the manifest (response) variables matrix contains missing values the Pearson chi-squared is not computed.
}
\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}
\seealso{ \code{\link{rasch}}}
\examples{

\dontrun{
## GoF for the Rasch model for the Wirs data:
## using 99+1 bootstrap samples
m <- rasch(Wirs)
anova(m, B = 99)
}

## GoF for the Rasch model for the Lsat data:
m <- rasch(Lsat)
anova(m)

## An LRT between the constrained (discrimination 
## parameter equals 1) and the unconstrained Rasch
## model for the Wirs data: 
m0 <- rasch(Wirs, constraint = rbind(c(7, 1)))
m1 <- rasch(Wirs)
anova(m0, m1)


## An LRT between the Rasch and the one-factor model
## for the Wirs data: 
m0 <- rasch(Wirs)
m1 <- ltm(Wirs~z1)
anova(m0, m1)

## An LRT between constrained (discrimination 
## parameter equals 1) and the unconstrained Rasch
## model for the Lsat data: 
m0 <- rasch(Lsat, constraint = rbind(c(6, 1)))
m1 <- rasch(Lsat)
anova(m0, m1)

}
\keyword{methods}
\keyword{ multivariate }
