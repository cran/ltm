\name{anova.rasch}
\alias{anova.rasch}
\title{ Anova method for rasch objects }
\description{
  Performs either a parametric Bootsrap goodness-of-fit test based on the
  Pearson chi-squared or a Likelihood Ratio Test between a \code{rasch} model and
  an \code{ltm} model.
}
\usage{
anova.rasch(object, ltm.obj = NULL, B = 49, ...)
}
\arguments{
  \item{object}{ an object inheriting from class  \code{rasch}. }
  \item{ltm.obj}{ an object inheriting from class  \code{ltm}. }
  \item{B}{ the number of Bootstrap samples. See Details section for more info.}
  \item{\dots}{ additional arguments; currently none is used.}
}
\details{
  If only \code{obj} is supplied \code{anova.rasch} performs a parametric 
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
    \eqn{(\# T_i  > T_{obs}) / (B+1) }.}
  }  
}
\value{
An object of class \code{aov.rasch} with components,
    \item{Tobs}{the value of the Pearson's chi-squared statistic for the observed data. Returned if only \code{object} is supplied.}
    \item{p.value}{the estimated \eqn{p}-value for the test. Returned if only \code{object} is supplied.}
    \item{B}{the \code{B} argument specifying the number of Bootstrap samples used. Returned if only \code{object} is supplied.}
    \item{call}{the matched call of \code{object}. Returned if only \code{object} is supplied.}
    \item{nam0}{the name of \code{object}. Returned if \code{ltm.obj} is also supplied.}
    \item{L0}{the log-likelihood under the null hypothesis (\code{object}). Returned if \code{ltm.obj} is also supplied.}
    \item{aic0}{the AIC value for the model given by \code{object}. Returned if \code{ltm.obj} is also supplied.}
    \item{bic0}{the BIC value for the model given by \code{object}. Returned if \code{ltm.obj} is also supplied.}
    \item{nam1}{the name of \code{ltm.obj}. Returned if \code{ltm.obj} is also supplied.}
    \item{L1}{the log-likelihood under the null hypothesis (\code{ltm.obj}). Returned if \code{ltm.obj} is also supplied.}
    \item{aic1}{the AIC value for the model given by \code{ltm.obj}. Returned if \code{ltm.obj} is also supplied.}
    \item{bic1}{the BIC value for the model given by \code{ltm.obj}. Returned if \code{ltm.obj} is also supplied.}
    \item{LRT}{the value of the Likelihood Ratio Test statistic. Returned if \code{ltm.obj} is also supplied.}
    \item{df}{the degrees of freedom for the test (i.e., the difference in the number of parameters). Returned if \code{ltm.obj} is also supplied.}
    \item{p.value}{the \eqn{p}-value of the test.}
}
\seealso{ \code{\link{rasch}}}
\examples{

## GoF for the Rasch model for the Wirs data:
## using 99 bootstrap samples
m <- rasch(Wirs)
anova(m, B=99)

## GoF for the Rasch model for the Lsat data:
m <- rasch(Lsat)
anova(m)


## An LRT between the Rasch and the one-factor model
## for the Wirs data: 
m0 <- rasch(Wirs)
m1 <- ltm(Wirs~z1)
anova(m0, m1)

## An LRT between the Rasch and the one-factor model
## for the Lsat data: 
m0 <- rasch(Lsat)
m1 <- ltm(Lsat~z1)
anova(m0, m1)

}
\keyword{ multivariate }
