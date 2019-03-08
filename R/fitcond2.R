#' Fits cuticular and intercellular conductance to data
#' \code{fitcond2} Fits cuticular and intercellular conductances
#'
#' @param data A dataframe.
#' @param rtc..rtc Difference in total and stomatal resistances to CO2 diffusion
#' @param gsw  Stomatal conductance to water
#' @param gcw  Cuticular conductance to water
#' @param gias Intercellular conductance to water?
#' @param a Initial value for gcw in curve fitting
#' @param b Initial value for gias in curve fitting
#' @param c exponential factor accounting for pathlength complexity in gias
#'
#' @return fitcond fits cuticular and intercellular conductances
#' @importFrom stats nls
#' @importFrom stats coef
#' @export
#'
#'
#'
fitcond2 <- function(data, rtc..rtc, gsw, gcw, gias, a, b, c){
  a <- ifelse(missing(a) == TRUE, 2, a)
  b <- ifelse(missing(b) == TRUE, 0.5, b)
  c <- ifelse(missing(c) == TRUE, 1, c)
  m      <- nlsLM(rtc..rtc ~ 1.6 * (1 / (gsw - gcw) - 1 / gsw)
                + 1 / (gias ^ p), data = data,
                start = list(gcw = a, gias = b, p = c))
  cond   <- coef(m)
  output <- as.list(c(m, cond, summary(m)))
  return(output)
}
