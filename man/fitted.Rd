\name{fitted}
\alias{fitted.grm}
\alias{fitted.ltm}
\alias{fitted.rasch}
\alias{fitted.tpm}

\title{ Fitted Values }

\description{
  Computes the expected frequencies for vectors of response patterns.
}

\usage{

\method{fitted}{grm}(object, resp.patterns = NULL, \dots)

\method{fitted}{ltm}(object, resp.patterns = NULL, \dots)

\method{fitted}{rasch}(object, resp.patterns = NULL, \dots)

\method{fitted}{tpm}(object, resp.patterns = NULL, \dots)
}

\arguments{
  \item{object}{ an object inheriting from either class \code{grm}, class \code{ltm}, class \code{rasch}, or class \code{tpm}. }
  \item{resp.patterns}{a matrix or a data.frame of response patterns with columns denoting the items; if \code{NULL} 
        the expected frequencies are computed for the observed response patterns. }
  \item{\dots}{ additional arguments; currently none is used. }
}

\value{
a numeric matrix including the response patterns of interest with their expected frequencies.
}

\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}

\seealso{
    \code{\link{grm}},
    \code{\link{ltm}},
    \code{\link{rasch}},
    \code{\link{tpm}}
}

\examples{

m <- grm(Science[c(1,3,4,7)])
fitted(m, resp.patterns = matrix(1:4, nr = 4, nc = 4))

m <- ltm(Wirs ~ z1)
fitted(m)

}
\keyword{methods}
