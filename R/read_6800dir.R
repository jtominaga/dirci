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
read_6800dir <- function(filename, skiplines, cimin, cimax, amin, amax,
                         co2list, log_freq){
  skiplines <- ifelse(missing(skiplines) == TRUE, 56, skiplines)
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
  data$CR <- round(data$CO2_r, digits = -1)
  data$test <- match(data$CR, co2list)
  data <- data[!is.na(data$test),]
  for (i in 1:length(data$co2_adj)){
    ifelse(i == 1, data$matchcount[i] <-  0,
           ifelse(data$co2_adj[i] - data$co2_adj[i-1] != 0,
                  data$matchcount[i] <-  max(data$matchcount[1:i]) + 1,
                  data$matchcount[i] <-  max(data$matchcount[1:i])))
    ifelse(i == 1, data$co2count[i] <-  0,
           ifelse(data$CR[i] - data$CR[i-1] != 0,
                  data$co2count[i] <-  max(data$co2count[1:i]) + 1,
                  data$co2count[i] <-  max(data$co2count[1:i])))
  }
  data$cidifference <- data$Ci - data$Ci.m.
  data <- data[complete.cases(data$Ci), ]
  data$co2count <- as.factor(data$co2count)

  #Looks for log time spacing anomalies to separate data into sets. this may
  #include different leaves, different temperatures, etc.
  data$timeset <- data$elapsed
  for (i in 1:length(data$elapsed)){
    ifelse(i == 1, data$timeset[i] <- 0,
           data$timeset[i] <- data$elapsed[i] - data$elapsed[i - 1])
  }
  data$meas_set <- data$timeset
  for (i in 1:length(data$timeset)){
    ifelse(i == 1, data$meas_set <- rep(1, length(data$meas_set)),
           ifelse(data$timeset[i] < 5 * log_freq,
                  data$meas_set[i] <- max(data$meas_set[1:i]),
                  data$meas_set[i] <- max(data$meas_set[1:i]) + 1))
  }

  #This section looks for the last match within a set, and equalizes the match
  #across all measurements within that set.
  data$co2_adj_new <- rep(0, length(data$co2_adj))
  data$h2o_adj_new <- rep(0, length(data$h2o_adj))
  datagroupsplit <- split(data, data$meas_set)
  for (i in 1:length())

  datasplit <- split(data, data$co2count)
  for (i in 1:length(datasplit)){
    datasplit[[i]] <- datasplit[[i]][datasplit[[i]]$matchcount ==
                                       max(datasplit[[i]]$matchcount),]
  }
  data_pared <- do.call("rbind", datasplit)

  data_list <- list("Partial" = data, "Pared" = data_pared)

  # Print data ----------------------------------------------
  return(data_list)
}
