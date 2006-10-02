\name{GoF.rasch}
\alias{GoF.rasch}

\title{ Goodness of Fit for Rasch Models}

\description{
  Performs a parametric Bootstrap test for Rasch models. 
}

\usage{

GoF.rasch(object, B = 49, \dots)

}
\arguments{
  \item{object}{ an object inheriting from class \code{rasch}.}
  \item{B}{ the number of Bootstrap samples. See \bold{Details} section for more info.}
  \item{\dots}{ additional arguments; currently none is used.}
}
\details{
  \code{GoF.rasch} performs a parametric Bootstrap test based on Pearson's chi-squared statistic. The Bootstrap 
  approximation to the reference distribution is preferable compared with 
  the ordinary Chi-squared approximation since the latter is not valid especially
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
    \eqn{[1 + \sum\limits_{i=1}^B I(T_i  > T_{obs})] / (B + 1).}{[1 + \{\# T_i  > T_{obs}\}]/(B + 1).}}
  }
}

\value{
An object of class \code{GoF.rasch} with components,
    \item{Tobs}{the value of the Pearson's chi-squared statistic for the observed data.}
    \item{B}{the \code{B} argument specifying the number of Bootstrap samples used.}
    \item{call}{the matched call of \code{object}.}
    \item{p.value}{the \eqn{p}-value of the test.}
}

\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}

\seealso{
    \code{\link{rasch}}
}

\examples{

## GoF for the Rasch model for the Lsat data:
fit <- rasch(LSAT)
GoF.rasch(fit)

}

\keyword{multivariate}
