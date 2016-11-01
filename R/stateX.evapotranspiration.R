#' Computes the evapotranspiration
#'
#'  Computes the evapotranspiration. So far as a function of the snow coverage and the temperature.
#'  Both the snow coverage and the temperature are distributed over several level zones.
#' @param htemp temperature for each level zone
#' @param sca snow coverage for each level zone
#' @return The output is a list with the evapotranspiraion (eatemp) as a value.
#' @keywords evapotranspiration
#' @export
#' @examples
#' \dontrun{
#' stateX()
#' }
stateX.evapotranspiration <-function(htemp,sca){

  snowfree <- 1- sum(sca)/length(sca)

  # mean of temperature at snowfree elevation bands
  if(snowfree>0.05) {
    indice <- seq(1,round(length(htemp)*snowfree))
    eatemp <- sum(htemp[indice])/round(length(htemp)*snowfree)
  } else {
    eatemp <- sum(htemp)/length(htemp)
  }

  update <- list(eatemp = eatemp)

  return(update)
}
