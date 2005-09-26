\name{fitted.ltm}
\alias{fitted.ltm}

\title{ fitted values for ltm objects }

\description{
  Computes the expected frequencies for a matrix of response patterns.
}

\usage{
\method{fitted}{ltm}(object, resp.patterns = NULL, \dots)
}

\arguments{
  \item{object}{ an object inheriting from class \code{ltm}. }
  \item{resp.patterns}{a matrix of response patterns with columns denoting the items; if \code{NULL} 
        the expected frequencies are computed for the observed response patterns. }
  \item{\dots}{ additional arguments; currently none is used. }
}

\value{
a numeric matrix including the response patterns of interest with the their expected frequencies.
}

\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}

\seealso{
    \code{\link{ltm}}
}

\examples{

m <- ltm(Wirs ~ z1)
fitted(m)

}
\keyword{methods}
