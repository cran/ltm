\name{margins}
\alias{margins}
\title{ Fit on the margins }
\description{
  Checks the fit on the two- and three-way margins for \code{ltm} and \code{rasch} objects.
}
\usage{
margins(object, type = c("two-way", "three-way"), nprint = 3, 
                rule = 3.5)
}
\arguments{
  \item{object}{ an object inheriting either from class \code{ltm} or class \code{rasch}.}
  \item{type}{ the type of margins to be used. }
  \item{nprint}{ a numeric value determining the number of margins with the largest residuals to be printed.}
  \item{rule}{ the rule of thumb used in determining the goodness-of-fit.}
}
\details{
 Rather than looking at the whole set of response patterns, we can look at the two- and three-way margins. 
 In the case of the former, we can construct the \eqn{2 \times 2} contingency tables obtained by taking the variables 
 two at a time. Comparing the observed and expected two-way margins is analogous to comparing the observed 
 and expected correlations when judging the fit of a factor analysis model. The comparison is made using the 
 so called chi-squared residuals. As a rule of thumb residuals greater than 3 (or 4) are indicative of poor fit. 
 Analogous procedure we follow for the three-way margins.
}
\value{
An object of class \code{margins} with components,
    \item{margins}{an array containing the values of chi-squared residuals.}
    \item{type}{the type of margins that were calculated.}
    \item{nprint}{the value of the \code{nprint} argument.}
    \item{combs}{all possible two- or three-way combinations of the items.}
    \item{rule}{the value of the \code{rule} argument.}
}
\references{
Bartholomew, D. (1998). Scaling unobservable constructs in social science. 
\emph{Applied Statistics} \bold{47}: 1-13.


Bartholomew, D. and Knott, M. (1999). \emph{Latent Variable Models
and Factor Analysis}, 2nd ed. London: Arnold.

Bartholomew, D., Steel, F., Moustaki, I. and Galbraith, J. (2002).
\emph{The Analysis and Interpretation of Multivariate Data for
Social Scientists}. London: Chapman and Hall.
}

\seealso{ \code{\link{ltm}}, \code{\link{rasch}} }
\examples{
## Two- and Three-way residuals for the Rasch model
m <- rasch(Lsat)
margins(m)
margins(m, "three")

## Two- and Three-way residuals for the two-factor model
m <- ltm(Wirs~z1+z2)
margins(m)
margins(m, "three")

## Two- and Three-way residuals for the interaction model
m <- ltm(Wirs~z1*z2)
margins(m, rule=3, nprint=2)
margins(m, "three", rule=3, nprint=2)

}
\keyword{ multivariate }
