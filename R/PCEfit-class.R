###################################################################
# polychaosbasics R package
# Copyright INRA 2017
# INRA, UR1404, Research Unit MaIAGE
# F78350 Jouy-en-Josas, France.
#
# URL: http://genome.jouy.inra.fr/logiciels/polychaosbasics
#
# This file is part of polychaosbasics R package.
# polychaosbasics is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# See the GNU General Public License at:
# http://www.gnu.org/licenses/
#
###################################################################
#########################################################
# Class to store the result of function 'PCESI'
#########################################################
setClass("PCEfit", slots=c(
                      indexes="matrix", # SI
                     indexes.percent="matrix", # %SI 
                       fit="vector", # R2 and RMSEP
                     IMSI="vector", #Individual Monomial SI
                       coef="vector", # les betas
                     y.hat="vector", # les y chapeau
                     design="PCEdesign", # la structure du polynome,
                     call.PCEpoly="call" # le call du PCEpoly,
                     ))

#########################################################
# print method
# "all": option to fix the display. Vector. Valid values are:
# "TRUE" : Beta coefficients and y.hat are displayed
# " ..." : all options passed as it to print

print.PCEfit <- function (x, all=FALSE, ...) {
     cat("PCE indexes:\n")
    print(x@indexes, ...)
     cat("\nPCE indexes (percentages):\n")
    print(round(x@indexes.percent, 2),...)
     cat("\nPCE fit:\n")
     print(x@fit,  ...)

     if (all) {
     cat("\n")
         print(x@design)
      cat("Number of observations:", length(x@y.hat), "\n")

     cat("\nAlso included:")
         cat("\n * slot 'IMSI' (Individual Monomial Sensitivity Indexes). Length: ")
         cat(length(x@IMSI))
         cat("\n * slot 'coef' (Regression coefficients). Length: ")
          cat(length(x@coef))
         cat("\n * slot 'y.hat' (Metamodel output). Length: ")
         cat(length(x@y.hat))
         cat("\n * slot 'call.PCEpoly' (Design creation command).\n")
   } # fin all

         return(invisible())
} # end print.PCEfit

#########################################################
# show method
show.PCEfit  <- function(object){
  print.PCEfit(object)
    return(invisible())
} # end show.PCEfit


setMethod("show", signature(object="PCEfit"),
          definition=show.PCEfit)

                

#########################################################
#    getNames      method
# --------------------------------------
getNames.PCEfit <- function(object){

  slotnames <- slotNames(object)
  for ( a in slotnames){
    

    cat(" Slot: ",a,".", sep="")
    cde <- paste("class(object@", a, ")", sep ="")
    cat(" Class: \"", eval(parse(text=cde)), "\".", sep="")
    cde <- paste("dim(object@", a, ")", sep ="")
    z <- eval(parse(text=cde))
     if (!is.null(z)) {
     cat(" Dimension:",  paste("(", paste(z, collapse=", "), ")", sep=""), ".", sep="")
   } else {
     cde <- paste("length(object@", a, ")", sep ="")
    z <- eval(parse(text=cde))
     if (!is.null(z)) {
     cat(" Length:",  paste("(", paste(z, collapse=", "), ")", sep=""), ".", sep="")
      }
   }
switch(a,
       indexes = {
         cat(" PCE indexes")
       },
       indexes.percent = {
         cat(" Percentages of PCE indexes")
       },
       fit = {
         cat(" R2 and RMSEP Root Mean Square Error Prediction)")
         },
       IMSI = {
          cat(" Individual Monomial Sensitivity Indexes")
        },
       coef = {
         cat(" Regression coefficients")
        },
       y.hat = {
         cat(" Fitted values of the response")
        },
       design = {
         cat(" The polynomial structure")
       },
        call.PCEpoly = {
          cat(" The command which creates the input design")
       },
        cat("Unknown slot ", a, "")
       ) # fin switch

    cat("\n")
    
  } # fin a

         return(invisible())
} # fin getNames



  
         
setMethod("getNames", signature(object="PCEfit"),
          definition=getNames.PCEfit)

     
