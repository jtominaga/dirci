% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_6800dir.R
\name{read_6800dir}
\alias{read_6800dir}
\title{Reads files from the Li-Cor 6800 including Ci measured directly
\code{read_6800dir} Reads Li-Cor 6800 files, which are deliminated by spaces and tabs.}
\usage{
read_6800dir(filename, skiplines, cimin, cimax, amin, amax, co2list,
  log_freq, deltaP)
}
\arguments{
\item{filename}{A character string of the form: "mydata".}

\item{skiplines}{A nuber specifying the number of header lines to skip.}

\item{cimin}{Minimum Ci cutoff}

\item{cimax}{Maximum Ci cutoff}

\item{amin}{Minimum A cutoff}

\item{amax}{Maximum A cutoff}

\item{co2list}{vector of CO2 reference values used}

\item{log_freq}{Frequency in seconds of logging data}

\item{deltaP}{Overpressure value}
}
\value{
read_6800 imports a Li-Cor 6800 file as a data frame
}
\description{
Reads files from the Li-Cor 6800 including Ci measured directly
\code{read_6800dir} Reads Li-Cor 6800 files, which are deliminated by spaces and tabs.
}
