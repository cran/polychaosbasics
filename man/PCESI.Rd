\name{PCESI}
\alias{PCESI}
\title{
Compute Sensitivity Indexes from a PCE Design
}
\description{
  Calculation of PCE sensitivity indexes
  and related results.  
}
\usage{
PCESI(poly)
}
\arguments{
  \item{poly}{
    an object of class \code{\linkS4class{PCEpoly}}.
    The design to analyze  (a Legendre polynomial).
}
}
\value{
  An object of class \code{\linkS4class{PCEfit}}.

}
\note{
  \itemize{
  \item
  By default, only a part of the returned object is displayed
  by the functions \code{print()} and \code{show()}.
  To see the hidden components, use the function \code{print()}
  with the option \code{all=TRUE} or the function \code{getNames}
  (see methods of class \code{\linkS4class{PCEfit}}).
  \item
  It is adviced to increase gradually the polynomial degree, up to the
  returned object contains a \code{R2}  value near
  from 1 and  a low \code{RMSEP}  value.
  }
}

\seealso{
  \itemize{ 
    \item 
  Functions \code{\link{polyLeg}} and \code{\link{analyticsPolyLeg}},
  creators of objects from
  class \code{\linkS4class{PCEpoly}}.
  \item
  Class \code{\linkS4class{PCEfit}} for description of
  the returned structure.
  }
  
}
\examples{
# Dataset simulated by  using the Ishigami function 
nlhs <- 200 # number of rows 
degree <- 6 # polynomial degree
set.seed(42)# fix the seed for reproductible results
pce <- analyticsPolyLeg(nlhs, degree, 'ishigami')# build the PCE design
ret <- PCESI(pce) # compute the PCE sensivity indexes
print(ret, all=TRUE)
}
\keyword{ regression }
