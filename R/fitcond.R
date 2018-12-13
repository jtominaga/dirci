#' Fits cuticular and intercellular conductance to data
#' \code{fitcond} Fits cuticular and intercellular conductances
#'
#' @param data A dataframe.
#' @param D    Difference in total and stomatal resistances to CO2 diffusion
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
fitcond <- function(data, D, gsw, gcw, gias){
  m      <- nls(D ~ 1.6 * (1 / (gsw - gcw) - 1 / gsw)
                + 1 / gias, data = data,
                start = list(gcw = 0, gias = 0.1))
  cond   <- coef(m)
  output <- as.list(c(m, cond, summary(m)))
  return(output)
}
