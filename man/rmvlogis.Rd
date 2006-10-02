\name{rmvlogis}
\alias{rmvlogis}

\title{
    Random Responses under Dichotomous IRT models
}

\description{
  Produces binary random variates under the Rasch, two-parameter and three parameter logistic models.
}

\usage{
rmvlogis(n, thetas, guessing = FALSE, link = c("logit", "probit"), 
         distr = c("normal", "logistic"))
}

\arguments{
  \item{n}{a scalar indicating the number of response patterns to simulate.}
  \item{thetas}{ a numeric matrix with row representing the items and columns the parameters. 
    See \bold{Details} for more info.}
  \item{guessing}{ if \code{TRUE} binary variates are simulated under the three parameter model.}
  \item{link}{ a character string indicating the link function to use. Options are logit and probit. }
  \item{distr}{ a character string indicating the distribution of the latent variable. Options are Normal and Logistic. }
}

\details{

The binary variates are simulated under the following parameterization for the probability of correctly responding in 
the \eqn{i}th item: \deqn{\pi_i = c_i + (1 - c_i) g(\beta_{1i} + \beta_{2i} z),}{ \pi_i = c_i + (1 - c_i) g(beta_{1i} 
+ beta_{2i} z),} where \eqn{z} denotes the latent variable, and \eqn{\beta_{1i}} and \eqn{\beta_{2i}} are the first 
and second columns of \code{thetas}, respectively, and \eqn{g()} is the link function. If \code{guessing = TRUE} then 
\eqn{c_i}'s should be located in the third column of \code{thetas}.

}

\value{
a numeric matrix with \code{n} rows and columns the number of items, containing the simulated binary variates.
}

\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}

\seealso{
  \code{\link{ltm}},
  \code{\link{rasch}},
  \code{\link{tpm}}
}

\examples{

# 10 response patterns under a Rasch model
# with 5 items
rmvlogis(10, cbind(seq(-2, 2, 1), 1))

}
\keyword{ multivariate }
