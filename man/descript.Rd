\name{descript}
\alias{descript}
\title{ Descriptive Statistics for Response Matrices }
\description{
  Computes the percentages of positive and negative responses for each item, as well 
  the pairwise associations between items (see Details for more info).
}
\usage{
descript(X, n.print = 10, ..., print = TRUE)
}
\arguments{
  \item{X}{ a data.frame containing binary variables. }
  \item{n.print}{if \code{print=TRUE} gives the number of pairwise associations with the highest
  \eqn{p}-values to be printed.
  }
  \item{\dots}{ extra argument passed to \code{chisq.test} (e.g., \code{simulate.p.value}, \code{B}) }
  \item{print}{ logical; if \code{TRUE} the results are printed.}
}
\details{
    Before an analysis with latent variable models, it is useful to inspect the data for evidence of
    positive correlations. In the case of binary data, this ad hoc check can be performed by constructing 
    the \eqn{2 \times 2} contingency tables for all possible pairs of items and examine the chi-squared \eqn{p}-values.
}
\value{
If \code{print=FALSE} a list is returned with the following components,
    \item{perc}{a matrix containing the percentages of positive and negative responses for each item.}
    \item{pw.ass}{a matrix containing the \eqn{p}-values for the pairwise association between the items.}
}
\examples{

## Descriptives for Wirs data:
descript(Wirs)
descript(Wirs, 3)

}
\keyword{ multivariate }
