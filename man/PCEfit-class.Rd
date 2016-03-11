\name{PCEfit-class}
\Rdversion{1.1}
\docType{class}
\alias{PCEfit-class}
%alias{show,PCEfit-method}
\alias{print.PCEfit}
%\alias{getNames,PCEfit-method}

\title{Class \code{"PCEfit"}}
\description{
  Container of the results of PCE sensitivity indexes  computation.
}
\section{Objects from the Class}{
Objects from this class are created by calls to function  \code{\link{PCESI}}.
}
\section{Slots}{
  \describe{
    \item{\code{indexes}:}{ matrix with as many rows as inputs and 3
      columns. Values of the PCE sensitivity indexes.
      The row labels are the inputs numbers.
      The column labels are LE, PE, TPE.
      \itemize{
      \item
      \code{indexes[i, "LE"]} is the Linear Effect of the  input \code{i}.
      \item
      \code{indexes[i, "PE"]} is the Polynomial Effect
      (called "SU" in Sudret, 2008). It is the  effect of the
      monomials in which only the input \code{i} appears.
      \item
      \code{indexes[i, "TPE"]} is the Total  Polynomial Effect
      (often called "SUT"). It is the  effect of all the
      monomials in which the input \code{i} appears.
    }
    }
    \item{\code{indexes.percent}:}{matrix.
      Percentages of the PCE sensitivity indexes, i.e
       values of  \code{indexes} expressed as percentages
      of the sums of their  columns.
    }
      
    \item{\code{fit}:}{vector of length 2. The values of R2
      and RMSEP (RMSEP: Root Mean Square Error Prediction).
    }
    \item{\code{IMSI}:}{vector of length equal to the number of
      monomials. Individual monomial sensitivity
      indexes.
      }
    \item{\code{coef}:}{vector of length equal to
    the number of monomials plus one. Regression
    coefficients. The first one is the constant term.
  }
    \item{\code{y.hat}:}{vector of length  equal to
    the number of rows of the dataset. Metamodel outputs.
  }
  \item{\code{design}:}{object of class \code{\linkS4class{PCEdesign}}. Matrix
    coding the polynomial structure.
  }
  \item{\code{call.PCEpoly}:}{expression of class \sQuote{call}. The command
    which creates the  \code{\linkS4class{PCEpoly}} object
    used as input in the creator command.}
  }
}
\section{Methods}{
  \describe{
    \item{getNames}{\code{signature(object = "PCEfit")}: display the names, class and 
      length of all the components. See the description
      of the generic function
    \code{\link{getNames}}.}
        \item{print}{\code{signature(object = "PCEfit", all=FALSE, ...)}:
      method of function \code{\link[base:print.default]{print}}. When
      option \code{all} is set to FALSE (the default), 
      only  the components \code{indexes}, \code{indexes.percent} and
      \code{fit}      are printed.
      The additional arguments are passed to the
      \code{\link[base:print.default]{print.default}} function. }

    \item{show}{\code{signature(object = "PCEfit")}:  same as
      function \code{print}, without any arguments. }
	 }
}
\references{
  Global sensitivity analysis using polynomial chaos expansions.
  Bruno Sudret.
  In \emph{Reliability Engineering and System Safety},
  Vol. 93, Issue 7, July 2008, pages 964-979.
}
\seealso{
  
    Function  \code{\link{PCESI}}, creator of objects from this class.
    
}
\examples{
showClass("PCEfit")
}
\keyword{classes}
