\name{getNames}
\alias{getNames}
\alias{getNames,PCEpoly-method}
\alias{getNames,PCEfit-method}
\title{
Display Structure of a Class
}
\description{
 Display the names, class and 
 length of all the slots of a
 \code{\linkS4class{PCEpoly}} or \code{\linkS4class{PCEfit}}
 object.
 
}
\usage{
getNames(object)
}
\arguments{
  \item{object}{
object from class \code{\linkS4class{PCEpoly}} or \code{\linkS4class{PCEfit}}.
}
}
\details{
  It is a generic function. Its  methods  are defined in classes
  \code{\linkS4class{PCEpoly}} and
  \code{\linkS4class{PCEfit}}.
}
\value{
Doesn't return any value.
}

\seealso{
 Classes
  \code{\linkS4class{PCEpoly}} and
  \code{\linkS4class{PCEfit}}.
}
\examples{
# Build Legendre polynomial degree 6 on a dataset
# simulated by using the Ishigami function:
pce <- analyticsPolyLeg(100, 6, 'ishigami')
# Display what contains the returned PCEpoly object:
getNames(pce)
}

