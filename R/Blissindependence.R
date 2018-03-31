#' Bliss Independence Model
#' 
#' This function returns fractional response levels for when these are based on
#' Bliss Independence Model.
#' 
#' @param doseInput Dose-response dataframe containing \code{"d1"} and
#'   \code{"d2"} columns
#' @param parmInput Numeric vector or list with appropriately named
#'   parameter inputs. Typically, it will be coefficients from a
#'   \code{MarginalFit} object.
#' @param ... Further arguments that are currently unused

Blissindependence <- function(doseInput, parmInput, ...){
  
  pars <- parmInput
  
  #We need the percentage value between 0 and 1 for Bliss estimated effect
  Hilleq <- function(dose,b,logEC50){
    Hilleqread <- 1/(1 + (exp(logEC50)/dose)^(abs(b)))
  }
  
  pred1 <- Hilleq(doseInput[, "d1"], pars["h1"], pars["e1"])
  pred2 <- Hilleq(doseInput[, "d2"], pars["h2"], pars["e2"])
  
  #Bliss independence combination
  applyfunction <- function(param){
    #prediction value in percentage
    predcombo <- param[1]+param[2]-param[1]*param[2]
    #scale back to readout value by multiplying with the highest maximum response
    predcombo <- ifelse(param[4] > param[5] ,param[3]+(param[4]-param[3])*predcombo ,param[3]+(param[5]-param[3])*predcombo)
    return(predcombo)
    }
  
  apply(cbind(pred1,pred2,pars["b"], pars["m1"],pars["m2"]),1, applyfunction)
}
