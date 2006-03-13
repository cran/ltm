\name{plot}
\alias{plot.grm}
\alias{plot.ltm}
\alias{plot.rasch}
\title{ Plot method for fitted IRT models}
\description{
  Produces the Item Characteristic or Item Information Curves for fitted IRT models.
}
\usage{
\method{plot}{grm}(x, type = c("ICC", "IIC"), items = NULL, category = NULL, 
        legend = FALSE, cx = "top", cy = NULL, ncol = NULL, 
        col = palette(), lty = 1, \dots)

\method{plot}{ltm}(x, type = c("ICC", "IIC"), items = NULL, legend = FALSE, 
        cx = "topleft", cy = NULL, ncol = 1, col = palette(), 
        lty = 1, \dots)

\method{plot}{rasch}(x, type = c("ICC", "IIC"), items = NULL, legend = FALSE, 
        cx = "topleft", cy = NULL, ncol = 1, col = palette(), 
        lty = 1, \dots)

}
\arguments{
  \item{x}{ an object inheriting from either class \code{grm}, class \code{ltm} or class \code{rasch}. }
  \item{type}{ the type of plot; "ICC" refers to Item Response Category Characteristic Curves whereas "IIC" to 
                Item Information Curves. }
  \item{items}{ a numeric vector denoting which items to plot; if \code{NULL} all items are plotted; if 0 and
                \code{type = "IIC"} the Test Information Curve is plotted. }
  \item{category}{a scalar indicating the response category for which the curves should be plotted; if \code{NULL}
                all categories are considered.}
  \item{legend}{ logical; if \code{TRUE} a legend is printed. }
  \item{cx}{ the x-coordinate to be passed to \code{legend()}. }
  \item{cy}{ the y-coordinate to be passed to \code{legend()}. }
  \item{ncol}{ the \code{ncol} argument of \code{legend()} .}
  \item{col}{ the colors to use; recycling is used if necessary. }
  \item{lty}{ the types of lines to use; recycling is used if necessary. }  
  \item{\dots}{ extra graphical parameters to be passed to \code{plot()}, \code{lines()}, \code{legend()} and 
                \code{text()}.}
}
\details{
  Item response category characteristic curves show how the probability of responding in the \eqn{k}th category, 
  in each item, changes with the values of the latent variable (ability).
  
  The item information curves indicate the relative ability of an item to discriminate among contiguous trait scores
  at various locations along the trait continuum. The test information curve, which is the sum of item information 
  curves, provides a visual depiction of where along the trait continuum a test is most discriminating 
  (Reise and Waller, 2002).
  
  If \code{x} is an \code{ltm} object representing a linear two-factor model, then the plot of the standardized 
  loadings (see \code{\link{coef.ltm}}) is also produced.

}

\references{
Reise, S. and Waller, N. (2002) Item response theory for dichotomous assessment data. In Drasgow, F. and Schmitt, N.,
editors, \emph{Measuring and Analyzing Behavior in Organizations}. San Francisco: Jossey-Bass.
}

\section{Warning}{
    Since the \code{\dots} argument is passed to both \code{plot()} and \code{legend()} an error will occur
    if you pass an argument that is not shared by the two functions (e.g., \code{plot(object, legend = TRUE, 
    cex.lab = 1.1)} will fail). In such cases you could specify the required option(s) using \code{par()}; see
    the \bold{Examples} for \code{plot.rasch()}. 
}

\author{
    Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.be}
}

\seealso{
    \code{\link{information}},
    \code{\link{grm}},
    \code{\link{ltm}},
    \code{\link{rasch}}
}

\examples{

# Examples for plot.grm()

m <- grm(Science[c(1,3,4,7)])

## Item Response Category Characteristic Curves for 
## the Science data
op <- par(mfrow = c(2, 2))
plot(m, lwd = 2, legend = TRUE, ncol = 2)
# re-set par()
par(op)

## Item Characteristic Curves for the 2nd category,
## and items 1 and 3
plot(m, category = 2, items = c(1, 3), lwd = 2, legend = TRUE, cx = "right")

## Item Information Curves for the Science data;
plot(m, type = "IIC",  legend = TRUE, cx = "topright", lwd = 2, cex = 1.4)

## Test Information Function for the Science data;
plot(m, type = "IIC",  items = 0, lwd = 2)

###################################################


# Examples for plot.ltm()

## Item Characteristic Curves for the two-parameter logistic
## model; plot only items 1, 2, 4 and 6:
m <- ltm(Wirs ~ z1)
plot(m, items = c(1, 2, 4, 6), lwd = 3, cex = 1.4)

## Test Information Function under the two-parameter logistic
## model for the Lsat data
m <- ltm(Lsat ~ z1)
plot(m, type = "IIC", items = 0, lwd = 2, cex.lab = 1.2, cex.main = 1.3)
info <- information(m, c(-3, 0))
text(x = 2, y = 0.5, labels = paste("Total Information:", round(info$Total, 3), 
    "\n\nInformation in (-3, 0):", round(info$Info, 3), 
    paste("(", round(100 * info$Proportion, 2), "\%)", sep = "")), cex = 1.2)

## Item Characteristic Surfaces for the interaction model:
m <- ltm(Wirs ~ z1 * z2)
plot(m)

###################################################


# Examples for plot.rasch()

## Item Characteristic Curves for the Wirs data;
## plot only items 1, 3 and 5:
m <- rasch(Wirs)
plot(m, items = c(1, 3, 5), lwd = 3, cex = 1.4)

m <- rasch(Lsat)

## Item Characteristic Curves for the Lsat data;
## plot all items plus a legend; increase 'cex.lab' 
## and 'cex.main' using par()
op <- par(cex.lab = 1.6, cex.main = 2)
plot(m, legend = TRUE, cx = "right", lwd = 3, cex = 1.4)
par(op)

## Item Information Curves, for the first 3 items; include a legend
plot(m, type = "IIC", items = 1:3, legend = TRUE, lwd = 2, cx = "topright")

## Test Information Function
plot(m, type = "IIC", items = 0, lwd = 2, cex.lab = 1.1)
info <- information(m, c(-2, 0))
text(x = 2, y = 0.5, labels = paste("Total Information:", round(info$Total, 3), 
    "\n\nInformation in (-2, 0):", round(info$Info, 3), 
    paste("(", round(100 * info$Prop, 2), "\%)", sep = "")), cex = 1.2)
}
\keyword{methods}
