\name{show}
%\alias{show}
\alias{show,PCEpoly-method}
\alias{show,PCEfit-method}
\alias{show,PCEdesign-method}

\title{
Methods 'show' for Classes of the Package 'polychaosbasics'
}
\description{
  Methods for the generic function '\code{show}' are defined
  for the classes \code{\linkS4class{PCEpoly}},
  \code{\linkS4class{PCEdesign}}
  and \code{\linkS4class{PCEfit}}.
  They do the same thing as the \code{print} methods
  when those are invoked without any
  argument.
}
%\usage{
%show(object)
%}
\arguments{
  \item{object}{
    object from class \code{\linkS4class{PCEpoly}},
    \code{\linkS4class{PCEdesign}}
    or \code{\linkS4class{PCEfit}}.
}
}
\value{
  Doesn't return any value.
}
\seealso{
Classes
  \code{\linkS4class{PCEpoly}},
    \code{\linkS4class{PCEdesign}} and
  \code{\linkS4class{PCEfit}}.
}
\examples{
# Build Legendre polynomial degree 6 on a dataset
# simulated by using the Ishigami function:
pce <- analyticsPolyLeg(100, 6, 'ishigami')
# Standard display the returned PCEpoly object:
pce # it is equivalent to 'show(pce)'
}
\keyword{ methods  }
