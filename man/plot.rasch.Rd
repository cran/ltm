\name{plot.rasch}
\alias{plot.rasch}
\title{ Plot method for rasch objects }
\description{
  Produces the Item Characteristic Curves or Surfaces for fitted \code{rasch} objects.
}
\usage{
plot.rasch(x, cx = NULL, cy = NULL, ...)
}
\arguments{
  \item{x}{ an object inheriting from class  \code{rasch}. }
  \item{cx}{ the x-coordinate to be passed to \code{legend}. }
  \item{cy}{ the y-coordinate to be passed to \code{legend}. }
  \item{\dots}{ extra graphical parameters to be passed to \code{lines}.}
}
\details{
  Item characteristic curves (also known as item response functions) show how the probability
  of a positive response, in each item, increases with the values of the latent variable (ability).
}
\seealso{ \code{\link{rasch}}}
\examples{

## Item Characteristic Curves for the Wirs data:
m <- rasch(Wirs)
plot(m)

## Item Characteristic Curves for the Lsat data:
m <- rasch(Lsat)
plot(m, cx=1.5, cy=0.4)

}
\keyword{ multivariate }
