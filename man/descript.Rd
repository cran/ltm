\name{descript}
\alias{descript}

\title{
    Descriptive Statistics for Response Matrices
}

\description{
  Computes the percentages of positive and negative responses for each item, as well 
  the pairwise associations between items (see \bold{Details} for more info).
}

\usage{
descript(X, n.print = 10, ...)
}

\arguments{
  \item{X}{ a data.frame containing binary variables. }
  \item{n.print}{numeric indicating the number of pairwise associations with the highest \eqn{p}-values 
        to be printed.}
  \item{\dots}{ extra argument passed to \code{chisq.test} (e.g., \code{simulate.p.value}, \code{B}, etc.) }
}

\details{
    Before an analysis with latent variable models, it is useful to inspect the data for evidence of
    positive correlations. In the case of binary data, this ad hoc check can be performed by constructing 
    the \eqn{2 \times 2}{2 by 2} contingency tables for all possible pairs of items and examine the Chi-squared 
    \eqn{p}-values.
}

\value{
An object of class \code{ltm} with components,
    \item{perc}{a numeric matrix containing the percentages of positive and negative responses for each item.}
    \item{items}{a numeric matrix containing the frequencies for the total scores.}
    \item{pw.ass}{a matrix containing the \eqn{p}-values for the pairwise association between the items.}
    \item{n.print}{the value of the \code{n.print} argument.}
    \item{name}{the name of argument \code{X}.}
}

\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}
\examples{

## Descriptives for Wirs data:
descript(Wirs, 3)

}
\keyword{ multivariate }
