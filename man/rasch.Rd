\name{rasch}
\alias{rasch}
\title{ Rasch Model }
\description{
  Fit the Rasch model under the Item Response Theory approach.
}
\usage{
rasch(dat, start.val, na.action = NULL, control = list())
}
\arguments{
  \item{dat}{a data.frame (that will be converted to a numeric matrix using 
        \code{data.matrix()}) or a numeric matrix of manifest variables. The binary responses
        must be in 0/1 format.}
  \item{start.val}{ a matrix of starting values for the
        algorithm (rows denote items and columns latent structure). If it is not supplied randomly
        chosen starting values are used instead.}
  \item{na.action}{ the \code{na.action} to be used on \code{dat}. In case of missing data, if 
  \code{na.action=NULL} the model uses the available cases, i.e., it takes 
        into account the observed part of sample units with missing values (valid under MAR mechanisms 
        if the model is correctly specified). If you want to apply a complete case analysis then use 
        \code{na.action=na.exclude}.}
  \item{control}{a list of control values,
    \describe{
        \item{iter.qN}{ the number of quasi-Newton iterations. Default 150.}
        \item{GHk}{ the number of Gauss-Hermite quadrature points. Default 20.}
        \item{method}{ the optimization method to be used in \code{optim}. Default "BFGS".}
        \item{verbose}{ logical; if \code{TRUE} info about the optimization procedure are printed.}
    }
  }
}
\details{ 
The Rasch model is special case of the unidimensional latent 
trait model when all the discrimination parameters are equal. 
This model was first discussed by Rasch (1960) and it is used 
mainly in educational testing where the aim is to study the 
abilities of a particular set of individuals.

The model is defined as follows \deqn{y_i = \theta_{i0} + \beta 
z,}{ y_i = theta_i + beta z, } where \eqn{y_i} denotes the response 
for the \emph{i}th item, \eqn{\theta_i} denotes the difficulty 
parameter for the \emph{i}th item, \eqn{\beta} is the 
discrimination parameter and \eqn{z} denotes the latent variable.

The optimization algorithm works under the constraint that the 
discrimination parameter is always positive.

}
\value{ An object of class \code{rasch} with components,
    \item{coefficients}{the loadings' values at convergence.}
    \item{log.Lik}{the log-likelihood value at convergence.}
    \item{convergence}{the convergence identifier returned by \code{optim}.}
    \item{hessian}{the Hessian matrix at convergence returned by \code{optim}.}
    \item{patterns}{a list with two components: (i) \code{mat} a numeric matrix 
    that contains the observed response patterns. (ii) \code{dat} a data.frame that contains the observed and expected 
    frequencies for each observed response pattern.}
    \item{GH}{a list with two components used in the Gauss-Hermite rule: (i) \code{Z} a numeric matrix that contains 
    the quadrature points. (ii) \code{GHw} a numeric vector that contains the corresponding  weights.} 
    \item{max.sc}{the maximum absolute value of the score vector at convergence.}
    \item{X}{the responses data matrix.}
    \item{control}{the values used in the \code{control} argument.}
    \item{call}{the matched call.}
}
\section{Warning}{ 
In case the Hessian matrix at convergence is not positive definite, try
to re-fit the model. \code{rasch} will use new random starting values.
}
\references{
Baker, F. and Kim, S-H. (2004). \emph{Item Response Theory}, 2nd ed. 
New York: Marcel Dekker.

Rasch, G. (1960). \emph{Probabilistic Models for Some 
Intelligence and  Attainment Tests}. Copenhagen: Paedagogiske 
Institute.
}
\seealso{
  \code{\link{coef.rasch}},
  \code{\link{summary.rasch}},
  \code{\link{anova.rasch}},
  \code{\link{plot.rasch}},
  \code{\link{margins}},
  \code{\link{factor.scores}}
}
\examples{

## The Rasch model for the Wirs data:
rasch(Wirs)

## The Rasch model for the Lsat data:
rasch(Lsat)

}
\keyword{multivariate}
\keyword{regression}
