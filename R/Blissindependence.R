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
  
  #Bliss independence combination
  applyfunction <- function(pred1, pred2,L,U1,U2){
    #prediction value in percentage
    predcombo <- pred1+pred2-pred1*pred2
    #scale back to readout value by multiplying with the highest maximum response
    predcombo <- ifelse(U1 > U2 ,L+(U1-L)*predcombo ,L+(U2-L)*predcombo)
    return(predcombo)
    }
  
  pred1 <- Hilleq(doseInput[, "d1"], pars["h1"], pars["e1"])
  pred2 <- Hilleq(doseInput[, "d2"], pars["h2"], pars["e2"])
  
  apply(cbind(pred1,pred2,pars["b"], pars["m1"],pars["m2"]),1, applyfunction)
}
