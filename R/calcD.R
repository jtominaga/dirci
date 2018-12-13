#' Calculates
#' \code{calcD} Calculates variables needed for fitcond
#'
#' @param data A dataframe.
#' @param D    Difference in total and stomatal resistances to CO2 diffusion
#' @param rco2 Total resistance to CO2 diffusion
#' @param rsc  Stomatal resistance to CO2 diffusion
#'
#' @return calcD calculates resistances required for fitcond
#' @importFrom stats nls
#' @export
#'
#'
#'
calcD <- function(data){
  data$rco2   <- 1 / data$gco2
  data$rsc    <- 1 / data$gsc
  data$D <- data$rco2 - data$rsc
  return(data)
}
