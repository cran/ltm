\name{plot.ltm}
\alias{plot.ltm}
\title{ Plot method for ltm objects }
\description{
  Produces the Item Characteristic Curves or Surfaces for fitted \code{ltm} objects.
}
\usage{
plot.ltm(x, cx = NULL, cy = NULL, ...)
}
\arguments{
  \item{x}{ an object inheriting from class  \code{ltm}. }
  \item{cx}{ the x-coordinate to be passed to \code{legend}. Used only for the one-factor model. }
  \item{cy}{ the y-coordinate to be passed to \code{legend}. Used only for the one-factor model. }
  \item{\dots}{ extra graphical parameters to be passed to \code{lines} (one-factor case) or to \code{persp} (two-factors case).  }
}
\details{
  Item characteristic curves (also known as item response functions) show how the probability
  of a positive response, in each item, increases with the values of the latent variables.
  
  If \code{x} is a linear two-factor model, then the plot of the standardized loadings (see \code{\link{coef.ltm}}) 
  is also produced.
}
\note{
  When two latent variables are used, \code{plot.ltm} uses \code{options(graphics.record = TRUE)}, i.e.,
  the plots are available for scrolling.
}
\seealso{ \code{\link{ltm}}}
\examples{

## Item Characteristic Curves for the one-factor model:
m <- ltm(Wirs~z1)
plot(m)

\dontrun{
## because of an error during R CMD CHECK; you can run it.
## Item Characteristic Surfaces for the interaction model:
m <- ltm(Wirs~z1*z2)
plot(m)
}
}
\keyword{ multivariate }
