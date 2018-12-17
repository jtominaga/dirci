#' Calculating mesophyll conductance via fluorescence
#' \code{procharley} Calculates mesophyll conductance and chloroplastic CO2
#'
#' @param data Dataframe
#' @param a leaf absoptance
#' @param b fraction of absorbed quanta that reaches PSII
#' @param gstar CO2 compensation point in absence of Rd
#' @param gea activation energy for gstar
#' @param gc constant for the temperature response of gstar
#' @param Rd dark respiration
#' @param Tleaf specifies leaf temperature to use
#' @param cimeas TRUE or FALSE, was Ci directly measured?
#'
#' @return procharley fits mesophyll conductance via the variable J method
#' @export
#'
procharley <- function(data, a, b, gstar, gea, gc, Rd, Tleaf, cimeas){
  a <- ifelse(missing(a) == TRUE, 0.84, a)
  b <- ifelse(missing(b) == TRUE, 0.5, b)
  cimeas <- ifelse(missing(cimeas) == TRUE, TRUE, FALSE)
  #Need temperature response of gstar below
  gstar <- ifelse(missing(gstar) == TRUE, 37.43, gstar)
  gea <- ifelse(missing(gea) == TRUE, 24.46, gea)
  gc <- ifelse(missing(gc) == TRUE, 13.49, gc)

  data$gstar <- exp(gc - gea / (0.008314 * (data$leaf + 273.15)))

  data$J <- a * b * data$Qin * data$PhiPS2
  data$Cc <- (data$gstar * (data$J + 8 * (data$A + data$Rd)) /
                (data$J - 4 * (data$A + data$Rd)))
  data$gco2_calc <- data$A / (data$Ci - data$Cc)
  ifelse(cimeas == TRUE, data$gco2 <- data$A / (data$CO2_B - data$Cc),
         data$gco2 <- data$gco2_calc)
  data$rco2   <- 1 / data$gco2
  data$rsc    <- 1 / data$gsc
  data$rdiff <- data$rco2 - data$rsc
}
