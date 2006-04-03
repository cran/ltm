\name{rcor.test}
\alias{rcor.test}

\title{
    Pairwise Associations between Items
}

\description{
  Computes and tests the pairwise associations between items using a correlation coefficient
}

\usage{
rcor.test(mat, \dots)
}

\arguments{
  \item{mat}{ a numeric matrix or numeric data.frame containing the manifest variables. }
  \item{\dots}{extra arguments passed to \code{cor()} and \code{cor.test()}.}
}

\value{
An object of class \code{rcor.test} with components,
    \item{cor.mat}{the correlation matrix.}
    \item{p.values}{a numeric three column matrix containing the \eqn{p}-values for all the combinations of items.}

The print method for class \code{rcor.test} returns a square matrix in which the upper diagonal part contains
the estimates of the correlation coefficients, and the lower diagonal part contains the corresponding \eqn{p}-values.
}

\note{
    \code{rcor.test()} is more appropriate for informal testing of polytomous items.
}

\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}

\examples{

## pairwise associations for Environment data:
rcor.test(data.matrix(Environment), method = "kendall")

## pairwise associations for independent normal random variates:
mat <- matrix(rnorm(1000), 100, 10)
rcor.test(mat)
rcor.test(mat, method = "kendall")
rcor.test(mat, method = "spearman")

}
\keyword{ multivariate }