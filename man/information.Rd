\name{information}
\alias{information}

\title{
    Area under the Test Information Function
}

\description{
  Computes the amount of test information for a fitted IRT model, in a specified range.
}

\usage{
information(object, range, items = NULL, ...)
}

\arguments{
  \item{object}{an object inheriting from either class \code{grm}, class \code{ltm} or class \code{rasch}.}
  \item{range}{a numeric interval for which the test information should be computed.}
  \item{items}{the items for which the information should be computed; the default \code{NULL} corresponds
                to all the items, which is equivalent to the test information.}
  \item{\dots}{extra arguments passed to \code{integrate()}.}
}

\details{
    The amount of information is computed as the area under the Item or Test Information Curve in the specified 
    interval, using \code{integrate()}.
}

\value{
A list with components,
    \item{Info in range}{the amount of information in the specified interval.}
    \item{Total Info}{the total amount of information; typically this is computed as the amount of information 
            in the interval \eqn{(-10, 10)}.}
    \item{Proportion in range}{the proportion of information in the specified range, i.e., 
        \code{"Info in range" / "Total Info"}.}
}

\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}

\seealso{
  \code{\link{plot.grm}},
  \code{\link{plot.ltm}},
  \code{\link{plot.rasch}}
}

\examples{

m <- rasch(Lsat)
information(m, c(-2, 0))
information(m, c(0, 2), items = 3)

}

\keyword{ multivariate }
