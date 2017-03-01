\name{polychaosbasics-package}
\alias{polychaosbasics-package}
\alias{polychaosbasics}
\docType{package}
\title{
\packageTitle{polychaosbasics}
}
\description{
\packageDescription{polychaosbasics}
}
\details{

  Legendre chaos polynomials are calculated on a
  provided dataset  by function  \code{\link{polyLeg}}
  or on a simulated LHS by function \code{\link{analyticsPolyLeg}}.
  
  Then, from the object returned by these functions,
  the \code{\link{PCESI}} function calculates
  sensitivity indexes, metamodel coefficients and some other
  results.
}
\author{
\packageAuthor{polychaosbasics}

Maintainer: \packageMaintainer{polychaosbasics}
}
\references{
  \itemize{ 
    \item Metamodeling and global sensitivity analysis for computer models with correlated inputs: A practical approach tested with 3D light interception computer model.
J.-P. Gauchi, A. Bensadoun, F. Colas, N. Colbach.
In \emph{Environmental Modelling \& Software},
Volume 92, June 2017. p. 40-56.
http://dx.doi.org/10.1016/j.envsoft.2016.12.005
    \item
  Global sensitivity analysis using polynomial chaos expansions.
  Bruno Sudret.
  In \emph{Reliability Engineering and System Safety},
  Vol. 93, Issue 7, July 2008, pages 964-979.
  }
}
\examples{
# First example:
# the dataset is simulated by using the Ishigami function 
nlhs <- 200 # number of rows
degree <- 6 # polynomial degree
set.seed(42)# fix the seed for reproductible results
pce <- analyticsPolyLeg(nlhs, degree, 'ishigami') # build Legendre polynomial
ret <- PCESI(pce) # compute the PCE sensitivity indexes
print(ret)
# Illustrate the result by a plot:
# plot the computer model output against the metamodel output
y.hat <- ret@y.hat   # metamodel output
y.obs <- pce[, "Y"] # computer model output
\dontrun{
X11() 
plot(y.hat, y.obs,
      xlab="metamodel output", ylab="computer model output",
      main="Ishigami test", sub="Scatter plot and regression line")
# Add the regression line
reg <- lm(y.hat ~ y.obs) # linear regression
lines(reg$fitted.values, y.obs)
}

# Second example:
# the dataset is a user dataset
load(system.file("extdata", "FLORSYS1extract.Rda",
   package="polychaosbasics"))
degree <- 4 # polynomial degree
lhs <- FLORSYS1extract[, -ncol(FLORSYS1extract)] # inputs
Y <- FLORSYS1extract[,ncol(FLORSYS1extract)] #  output 
pce <- polyLeg(lhs, Y, degree) # build Legendre polynomial
ret <- PCESI(pce) # compute the PCE sensitivity indexes
print(ret, all=TRUE)
}
\keyword{ package }
\keyword{ regression}
