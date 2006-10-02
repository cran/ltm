\name{factor.scores}
\alias{factor.scores}
\alias{factor.scores.grm}
\alias{factor.scores.ltm}
\alias{factor.scores.rasch}
\alias{factor.scores.tpm}

\title{ Factor Scores - Ability Estimates }

\description{
  Computation of factor scores for \code{grm}, \code{ltm}, \code{rasch} and \code{tpm} models.
}

\usage{
factor.scores(object, \dots)

\method{factor.scores}{grm}(object, resp.patterns = NULL, method = c("EB", "MI"), 
                    B = 5, \dots)

\method{factor.scores}{ltm}(object, resp.patterns = NULL, method = c("EB", "MI", 
                    "Component"), B = 5, robust.se = FALSE, \dots)

\method{factor.scores}{rasch}(object, resp.patterns = NULL, method = c("EB", "MI"), 
                    B = 5, robust.se = FALSE, \dots)

\method{factor.scores}{tpm}(object, resp.patterns = NULL, method = c("EB", "MI"), 
                    B = 5, \dots)
}

\arguments{
  \item{object}{ an object inheriting either from class \code{grm}, class \code{ltm} or class \code{rasch}.}
  \item{resp.patterns}{a matrix or a data.frame of response patterns with columns denoting the items; if \code{NULL} 
        the factor scores are computed for the observed response patterns.}
  \item{method}{a character supplying the scoring method. For \code{ltm} objects available methods are:
  Component scores, Empirical Bayes and Multiple Imputation.
  For \code{grm} and \code{rasch} objects available methods are: Empirical Bayes and Multiple Imputation. 
  See \bold{Details} section for more info.}
  \item{B}{the number of multiple imputations to be used if \code{method = "MI"}.}
  \item{robust.se}{logical; if \code{TRUE} the sandwich estimator is used for the estimation of the covariance
  matrix of the MLEs. See \bold{Details} section for more info.}
  \item{\dots}{ additional argument; currently none is used. }
}

\details{
  Factor scores or ability estimates are summary measures of the posterior distribution \eqn{p(z|x)}, 
  where \eqn{z} denotes the vector of latent variables and \eqn{x} the vector of manifest variables.
  
  Usually as factor scores we assign the modes of the above posterior distribution evaluated at the MLEs. These
  Empirical Bayes estimates (use \code{method = "EB"}) and their associated variance are good measures of the 
  posterior distribution while \eqn{p \rightarrow \infty}{p -> infinity}, where \eqn{p} is the number of items. 
  This is based on the result \deqn{p(z|x)=p(z|x; \hat{\theta})(1+O(1/p)),}{p(z|x)=p(z|x; \hat{\theta})(1+O(1/p)),}
  where \eqn{\hat{\theta}} are the MLEs. However, in cases where \eqn{p} and/or \eqn{n} (the sample size) is small 
  we ignore the variability of plugging-in estimates but not the \emph{true} parameter values. A solution to this 
  problem can be given using Multiple Imputation (MI; use \code{method = "MI"}). In particular, MI is used the 
  other way around, i.e.,
   \describe{
    \item{Step 1:}{Simulate new parameter values, say \eqn{\theta^*}, from \eqn{N(\hat{\theta}, C(\hat{\theta}))},
    where \eqn{C(\hat{\theta})} is the large sample covariance matrix of \eqn{\hat{\theta}} (if \code{robust.se = TRUE}, 
    \eqn{C(\hat{\theta})} is based on the sandwich estimator).}
    \item{Step 2:}{Maximize \eqn{p(z|x; \theta^*)} \emph{wrt} \eqn{z} and also compute the associated 
    variance to this mode.}
    \item{Step 3:}{Repeat steps 1-2 \code{B} times and combine the estimates using the known formulas of MI.}
  }
  
  This scheme explicitly acknowledges the ignorance of the true parameter values by drawing from their large sample
  posterior distribution while taking into account the sampling error. The modes of the posterior distribution 
  \eqn{p(z|x; \theta)}{p(z|x; theta)} are numerically approximated using the BFGS algorithm in \code{optim()}. 
  
  The Component scores (use \code{method = "Component"}) proposed by Bartholomew (1984) is an alternative method
  to scale the sample units in the latent dimensions identified by the model that avoids the calculation of the 
  posterior mode. However, this method is not valid in the general case where nonlinear latent terms are assumed.
}

\value{
  An object of class \code{fscores} is a list with components,
  \item{score.dat}{the \code{data.frame} of observed response patterns including, observed and expected 
        frequencies as well as the factor scores.}
  \item{method}{a character giving the scoring method used.}
  \item{B}{the number of multiple imputations used; relevant only if \code{method = "MI"}.}
  \item{call}{a copy of the matched call of \code{object}.}
}

\references{
Bartholomew, D. (1984) Scaling binary data using a factor model. \emph{Journal of the Royal 
Statistical Society, Series B}, \bold{46}, 120--123.

Bartholomew, D. and Knott, M. (1999) \emph{Latent Variable Models
and Factor Analysis}, 2nd ed. London: Arnold.

Bartholomew, D., Steel, F., Moustaki, I. and Galbraith, J. (2002)
\emph{The Analysis and Interpretation of Multivariate Data for
Social Scientists}. London: Chapman and Hall.

Rizopoulos, D. and Moustaki, I. (2006) Generalized latent variable models
with nonlinear terms. \emph{submitted for publication}.
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

## Factor Scores for the Rasch model
fit <- rasch(LSAT)
factor.scores(fit) # Empirical Bayes


## Factor Scores for the two-parameter logistic model
fit <- ltm(Abortion ~ z1)
factor.scores(fit, method = "MI", B = 20) # Multiple Imputation

## Factor Scores for the graded response model
fit <- grm(Science[c(1,3,4,7)])
factor.scores(fit, resp.patterns = rbind(1:4))

}

\keyword{ multivariate }
