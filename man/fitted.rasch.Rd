\name{fitted.rasch}
\alias{fitted.rasch}

\title{ fitted values for rasch objects }

\description{
  Computes the expected frequencies for a matrix of response patterns.
}

\usage{
\method{fitted}{rasch}(object, resp.patterns = NULL, \dots)
}

\arguments{
  \item{object}{ an object inheriting from class \code{rasch}. }
  \item{resp.patterns}{ a matrix of response patterns with columns denoting the items; if \code{NULL} 
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
    \code{\link{rasch}}
}

\examples{

m <- rasch(Wirs)
fitted(m)

}

\keyword{methods}
