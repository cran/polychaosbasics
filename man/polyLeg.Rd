\name{polyLeg}
\alias{polyLeg}
\title{
Calculate Legendre Polynomials from a Dataset
}
\description{
  This function calculates Legendre polynomials on
  a user LHS.
  
  Legendre polynomials are computed
  after calibration within the bounds [-1, +1].
}
\usage{
polyLeg(lhs, Y, degree)
}
\arguments{
  \item{lhs}{ matrix with as many columns
    as inputs. Dataset of  inputs. Generally, a space filling design is used for
    forming
    this dataset. Typically, this is a simple LHS (see  McKay, 1979)
    or a modified  LHS.
}
  \item{Y}{
    vector of length equal to the number of rows in \code{lhs}.
    Model outputs.
}
\item{degree}{
  integer greater than 1. Maximal degree of the polynomial. 
}
}
\value{
  An objet of class \code{\linkS4class{PCEpoly}}.
}
\references{
 McKay, M.D. and Beckman, R.J. and Conover, W.J. 
    1979. \dQuote{A Comparison of Three Methods for Selecting Values of
      Input Variables in the Analysis of Output from a Computer Code}.In
    \emph{Technometrics},
 21 (2). 239-245p. 
  }    
\seealso{
 \itemize{ 
    \item  Function \code{\link{analyticsPolyLeg}}  builds
  Legendre polynomials from a simulated dataset.
  \item
  Function \code{\link{PCESI}} calculates PCE sensivity indexes
from the returned
  object.
  }
}
\examples{
load(system.file("extdata", "FLORSYS1extract.Rda",
   package="polychaosbasics"))
degree <- 4 # polynomial degree
lhs <- FLORSYS1extract[, -ncol(FLORSYS1extract)] # inputs
Y <- FLORSYS1extract[,ncol(FLORSYS1extract)] #  output
pce <- polyLeg(lhs, Y, degree) 
print(pce)
}


\keyword{ regression }
