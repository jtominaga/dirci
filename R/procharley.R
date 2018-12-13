procharley <- function(data, a, b, gstar){
  a <- ifelse(missing(a) == TRUE, 0.84, a)
  b <- ifelse(missing(b) == TRUE, 0.5, b)
  #Need temperature response of gstar below
  gstar <- ifelse(missing(gstar) == TRUE, 42.5, gstar)

  data$J <- a * b * data$Qin * PhiPS2
  data$Cc <- (gstar * (data$J + 8 * (data$A + R)) /
                (data$J - 4 * (data$A + R)))

}
