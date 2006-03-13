\name{mult.choice}
\alias{mult.choice}

\title{
    Multiple Choice Items to Binary Responses 
}

\description{
it converts multiple choice items to a matrix of binary responses.
}

\usage{
mult.choice(data, correct)
}

\arguments{
  \item{data}{ a matrix or data.frame containing the manifest variables. }
  \item{correct}{a vector of length \code{ncol(data)} with the correct responses.}
}

\value{
a matrix of 0/1 values indicating wrong/correct answers.
}

\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}
\examples{

dat <- data.frame(cbind(It1 = sample(4, 100, TRUE),
                        It2 = sample(4, 100, TRUE),
                        It3 = sample(5, 100, TRUE),
                        It4 = sample(5, 100, TRUE),
                        It5 = sample(4, 100, TRUE),
                        It6 = sample(5, 100, TRUE)))
dat[] <- lapply(dat, function (x) { x[sample(100, 4)] <- NA; x })
crct <- c(3, 2, 5, 3, 4, 5)
####################
mult.choice(dat, crct)

}
\keyword{ multivariate }
