\name{ltm}
\alias{ltm}
\title{ Latent Trait Model - Latent Variable Model for Binary Data}
\description{
  Fit a latent trait model under the Item Response Theory approach.
}
\usage{
ltm(formula, start.val, constraint=NULL, na.action = NULL, 
        control = list())
}
\arguments{
  \item{formula}{ a two-sided formula providing the responses data matrix and describing the latent 
        structure. In the left side of \code{formula} either a data.frame (that will be converted to 
        a numeric matrix using \code{data.matrix()}) or a numeric matrix of manifest variables must be 
        supplied. The binary responses must be in 0/1 format. In the right side of \code{formula} only 
        two latent variables are allowed with codenames \code{z1}, \code{z2}. Interaction and quadratic 
        terms can also be used (see Details section for more info).}
  \item{start.val}{ a matrix of starting values for the
        algorithm (rows denote items and columns latent structure, with first column the intercepts). 
        If it is not supplied randomly chosen starting values are used instead.}
  \item{constraint}{a numeric vector with 3 elements, denoting the item, the factor and
        the value of the constraint. Look at the Details section for more info.}
  \item{na.action}{ the \code{na.action} to be used on the data frame in the left-side of \code{formula}.
        In case of missing data, if \code{na.action=NULL} the model uses the available cases, i.e., it takes 
        into account the observed part of sample units with missing values (valid under MAR mechanisms if the
        model is correctly specified). If you want to apply a complete case analysis then use 
        \code{na.action=na.exclude}.}
  \item{control}{ a list of control values,
    \describe{
        \item{iter.em}{ the number of EM iterations. Default 40.}
        \item{iter.qN}{ the number of quasi-Newton iterations. Default 150.}
        \item{GHk}{ the number of Gauss-Hermite quadrature points. Default 15.}
        \item{method}{ the optimization method to be used in \code{optim}. Default "BFGS".}
        \item{verbose}{ logical; if \code{TRUE} info about the optimization procedure are printed.}
    }
  }
}
\details{
The latent trait model is the analogous of the factor 
analysis model for binary observed data. The model assumes that 
the dependencies between the observed response variables (known as items) 
can be interpreted by a small number of latent variables. The model 
formulation is under the IRT approach.

The usual form of the latent trait model assumes linear latent 
variable effects (Bartholomew and Knott, 1999; Moustaki and Knott, 2000). \code{ltm} fits the 
linear one- and two-factor models but also provides extensions described 
by Rizopoulos and Moustaki (2005) to include nonlinear latent variable effects.

The linear one- and two-factor model are unidentified under orthogonal 
rotations on the factors' space. To achieve identifiability you 
can fix the value of one loading using the \code{constraint} 
argument. 

The fit is done by optimizing the marginal log-likelihood using 
the conditional independence assumption, i.e., conditionally on 
the latent structure the items are independent Bernoulli variates 
under the logit link. The required integrals are approximated 
using the Gauss-Hermite rule. The optimization procedure used is 
a hybrid algorithm. The procedure initially uses a moderate 
number of EM iterations (see \code{control} argument 
\code{iter.em}) and then switches to quasi-Newton (see 
\code{control} arguments \code{method} and \code{iter.qN}) 
iterations until convergence. In case of large number of items, 
the `Conjugate Gradient' optimization method might be preferable. 
Due to the use of Gauss-Hermite integration combined with the relatively
large data sets encountered in multivariate analysis the use of optimized
BLAS (e.g., Goto's BLAS) is highly recommended. 
}
\value{
An object of class \code{ltm} with components,
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
    \item{ltn.struct}{a character vector describing the latent structure.}
    \item{control}{the values used in the \code{control} argument.}
    \item{call}{the matched call.}
}
\section{Warning}{ 
In case the Hessian matrix at convergence is not positive definite, try
to re-fit the model. \code{ltm} will use new random starting values.

The inclusion of nonlinear latent variable effects produces more
complex likelihood surfaces which might possess a number of local
maxima. To ensure that the maximum likelihood value has been
reached re-fit the model a number of times (simulations showed
that usually 10 times are adequate to ensure global convergence).
}
\references{
Baker, F. and Kim, S-H. (2004). \emph{Item Response Theory}, 2nd ed. 
New York: Marcel Dekker.

Bartholomew, D. and Knott, M. (1999). \emph{Latent Variable Models
and Factor Analysis}, 2nd ed. London: Arnold.

Bartholomew, D., Steel, F., Moustaki, I. and Galbraith, J. (2002).
\emph{The Analysis and Interpretation of Multivariate Data for
Social Scientists}. London: Chapman and Hall.

Moustaki, I. and Knott, M. (2000). Generalized latent trait
models. \emph{Psychometrika} \bold{65}: 391-411.

Rizopoulos, D. and Moustaki, I. (2005). Nonlinear effects in the
generalized latent variable model. \emph{submitted for
publication}.
}
\seealso{
  \code{\link{coef.ltm}},
  \code{\link{summary.ltm}},
  \code{\link{anova.ltm}},
  \code{\link{plot.ltm}},
  \code{\link{margins}},
  \code{\link{factor.scores}}
}
\examples{
## One-factor model using the WIRS data
ltm(Wirs~z1)

## One-factor and a quadratic term
## using the Mobility data
ltm(Mobility~z1+I(z1^2))


## Two-factor model with an interaction term
## using the WIRS data
ltm(Wirs~z1*z2)

## A two-factor model with the constraint that the loading
## for the 3rd item and the 2nd factor equals 1
ltm(Wirs~z1+z2, constraint=c(3,2,1))

## One-factor model with 20 quadrature points
## 20 EM iterations
ltm(Lsat~z1, control=list(GHk=20, iter.em=20))

}
\keyword{multivariate}
\keyword{regression}
