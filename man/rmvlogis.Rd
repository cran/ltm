\name{rmvlogis}
\alias{rmvlogis}

\title{
    Generate Random Responses Patterns under Dichotomous IRT models
}

\description{
  Produces Bernoulli random variates under the Rasch, two-parameter and three parameter logistic models.
}

\usage{
rmvlogis(n, thetas, IRT = TRUE, link = c("logit", "probit"), 
         distr = c("normal", "logistic", "log-normal"))
}

\arguments{
  \item{n}{a scalar indicating the number of response patterns to simulate.}
  \item{thetas}{ a numeric matrix with rows representing the items and columns the parameters. 
    See \bold{Details} for more info.}
  \item{IRT}{ logical; if \code{TRUE} \code{thetas} are under the IRT parameterization. 
    See \bold{Details} for more info.}
  \item{link}{ a character string indicating the link function to use. Options are logit and probit. }
  \item{distr}{ a character string indicating the distribution of the latent variable. Options are normal, logistic, 
    and log-normal. }
}

\details{

The binary variates can be simulated under the following parameterizations for the probability of correctly responding in 
the \eqn{i}th item. If \code{IRT = TRUE} \deqn{\pi_i = c_i + (1 - c_i) g(\beta_{2i} (z - \beta_{1i})),}{ \pi_i = c_i + 
(1 - c_i) g(beta_{2i} (z - beta_{1i})),} whereas as if \code{IRT = FALSE}  \deqn{\pi_i = c_i + (1 - c_i) g(\beta_{1i} + 
\beta_{2i} z),}{ \pi_i = c_i + (1 - c_i) g(beta_{1i} + beta_{2i} z),} \eqn{z} denotes the latent variable, 
\eqn{\beta_{1i}} and \eqn{\beta_{2i}} are the first and second columns of \code{thetas}, respectively, and \eqn{g()} 
is the link function. If \code{thetas} is a three-column matrix then the third column should contain the guessing 
parameters \eqn{c_i}'s.

}

\note{

For both options \code{distr = "logistic"} and \code{distr = "log-normal"} the simulated random variates for \eqn{z} are
standardized, using the theoretical mean and variance of the Logistic and log-Normal distribution, respectively.

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
