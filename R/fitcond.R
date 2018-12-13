#' Fits cuticular and intercellular conductance to data
#' \code{fitcond} Fits cuticular and intercellular conductances
#'
#' @param data A dataframe.
#' @param rco2 Resistance to CO2 diffusion
#' @param gco2 Conductance to CO2
#' @param rsc  Stomatal resistance to CO2
#' @param gsc  Stomatal conductance to CO2
#' @param gsw  Stomatal conductance to water
#' @param gcw  Cuticular conductance to water
#' @param gias Intercellular conductance to water?
#'
#' @return fitcond fits cuticular and intercellular conductances
#' @importFrom stats nls
#' @export
#'
#'
#'
fitcond <- function(data, rco2, gco2, rsc, gsc, gsw, gcw, gias){
  rco2   <- ifelse(missing(rco2) == TRUE, 1 / gco2, rco2)
  rsc    <- ifelse(missing(rsc)  == TRUE, 1 / gsc,  rsc)
  data$D <- data$rco2 - data$rsc
  m      <- nls(data$D ~ 1.6 * (1 / (data$gsw - data$gcw) - 1 / data$gsw)
                + 1 / data$gias)

}
