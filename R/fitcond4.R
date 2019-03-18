#' Fits cuticular and intercellular conductance to data
#' \code{fitcond4} Fits cuticular and intercellular conductances
#'
#' @param data A dataframe.
#' @param rtc..rtc Difference in total and stomatal resistances to CO2 diffusion
#' @param gsw  Stomatal conductance to water
#' @param gcw  Cuticular conductance to water
#' @param x Intercellular conductance to water?
#' @param a Initial value for gcw in curve fitting
#' @param b Initial value for gias in curve fitting
#'
#' @return fitcond4 fits cuticular and intercellular conductances
#' @importFrom stats nls
#' @importFrom stats coef
#' @export
#'
#'
#'
fitcond4 <- function(data, rtc..rtc, gsw, gcw, x, a, b){
  a <- ifelse(missing(a) == TRUE, 0.005, a)
  b <- ifelse(missing(b) == TRUE, 2, b)
  m      <- nls(rtc..rtc ~ 1.6 * (1 / (gsw - gcw) - 1 / gsw)
                + x, data = data,
                start = list(gcw = a, x = b))
  cond   <- coef(m)
  output <- as.list(c(m, cond, summary(m)))
  return(output)
}
