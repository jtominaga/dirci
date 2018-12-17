#' Reads files from the Li-Cor 6800 including Ci measured directly
#' \code{read_6800dir} Reads Li-Cor 6800 files, which are deliminated by spaces and tabs.
#'
#' @param filename A character string of the form: "mydata".
#' @param skiplines A nuber specifying the number of header lines to skip.
#' @param cimin Minimum Ci cutoff
#' @param cimax Maximum Ci cutoff
#' @param amin Minimum A cutoff
#' @param amax Maximum A cutoff
#' @param co2list vector of CO2 reference values used
#'
#' @return read_6800 imports a Li-Cor 6800 file as a data frame
#' @importFrom utils read.delim
#' @export
#'
#'
#'
read_6800dir <- function(filename, skiplines, cimin, cimax, amin, amax, co2list){
  skiplines <- ifelse(missing(skiplines) == TRUE, 54, skiplines)
  cimin <- ifelse(missing(cimin) == TRUE, 0, cimin)
  cimax <- ifelse(missing(cimax) == TRUE, 2000, cimax)
  amin <- ifelse(missing(amin) == TRUE, -10, amin)
  amax <- ifelse(missing(amax) == TRUE, 100, amax)

  # Read in file for column names ---------------------------
  colname <- read.delim(filename, sep = "\t", skip = skiplines,
                        header = TRUE, fill = TRUE)

  # Read in file --------------------------------------------
  data <- read.delim(filename, sep = "\t", skip = skiplines + 2,
                     header = FALSE, fill = TRUE)

  # Assign column names -------------------------------------
  colnames(data) <- colnames(colname)

  # Pare down data ------------------------------------------
  data <- data[data$Ci > cimin, ]
  data <- data[data$Ci < cimax, ]
  data <- data[data$A > amin, ]
  data <- data[data$A < amax, ]
  data$CR <- round(data$CO2_r, digits = 1)
  data$test <- match(data$CR, co2list)
  data <- data[!is.na(data$test),]

  data$cidifference <- data$Ci - data$CO2_B

  # Print data ----------------------------------------------
  return(data)
}
