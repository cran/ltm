\name{plot.rasch}
\alias{plot.rasch}
\title{ Plot method for rasch objects }
\description{
  Produces the Item Characteristic Curves for fitted \code{rasch} objects.
}
\usage{
\method{plot}{rasch}(x, items = NULL, legend = FALSE, cx = -3.4, cy = 0.9, \dots)
}
\arguments{
  \item{x}{ an object inheriting from class  \code{rasch}. }
  \item{items}{ a numeric vector denoting which items to plot; if \code{NULL} all items are plotted.}
  \item{legend}{ logical; if \code{TRUE} a legend is printed. }
  \item{cx}{ the x-coordinate to be passed to \code{legend}. }
  \item{cy}{ the y-coordinate to be passed to \code{legend}. }
  \item{\dots}{ extra graphical parameters to be passed to \code{plot}, \code{lines} and \code{legend}.}
}
\details{
  Item characteristic curves (also known as item response functions) show how the probability
  of a positive response, in each item, increases with the values of the latent variable (ability).
}
\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}

\seealso{ \code{\link{rasch}}}

\examples{

## Item Characteristic Curves for the Wirs data;
## plot only items 1, 3 and 5:
m <- rasch(Wirs)
plot(m, items = c(1, 3, 5), lwd = 3, cex = 1.4)


## Item Characteristic Curves for the Lsat data;
## plot all items plus a legend
m <- rasch(Lsat)
plot(m, legend = TRUE, cx = 1.5, cy = 0.4, lwd = 3, cex = 1.4)

}
\keyword{methods}
