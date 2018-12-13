#' Calculating mesophyll conductance via fluorescence
#' \code{procharley} Calculates mesophyll conductance and chloroplastic CO2
#'
#' @param data Dataframe
#' @param a leaf absoptance
#' @param b fraction of absorbed quanta that reaches PSII
#' @param gstar CO2 compensation point in absence of Rd
#' @param gea activation energy for gstar
#' @param gc constant for the temperature response of gstar
#' @param PhiPS2 PSII quantum yield in the light
#' @param Rd dark respiration
#' @param Tleaf specifies leaf temperature to use
#'
#' @return
#' @export
#'
#' @examples
procharley <- function(data, a, b, gstar, gea, gc, Rd, Tleaf){
  a <- ifelse(missing(a) == TRUE, 0.84, a)
  b <- ifelse(missing(b) == TRUE, 0.5, b)
  #Need temperature response of gstar below
  gstar <- ifelse(missing(gstar) == TRUE, 37.43, gstar)
  gea <- ifelse(missing(gea) == TRUE, 24.46, gea)
  gc <- ifelse(missing(gc) == TRUE, 13.49, gc)

  data$gstar <- exp(gc - gea / (8.314 * (data$leaf + 273.15)))

  data$J <- a * b * data$Qin * data$PhiPS2
  data$Cc <- (data$gstar * (data$J + 8 * (data$A + data$Rd)) /
                (data$J - 4 * (data$A + data$Rd)))
  data$gCO2 <- data$A / (data$Ci - data$Cc)
}
