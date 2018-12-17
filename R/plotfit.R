#' Plots fitcond
#' \code{plotfit} Plots fitcond
#'
#' @param data A dataframe.
#' @param fit  The output from fitcond
#'
#' @return plotfit plots the output of fitcond
#' @importFrom stats nls
#' @importFrom stats coef
#' @import ggplot2
#' @export
#'
plotfit <- function(data, fit){
  funct <- function(gsw){1.6 * (1 / (gsw - fit[[7]]) - 1 / gsw) + 1 / fit[[8]]}
  ggplot(data = data, aes(x = data$gsw, y = data$rdiff))+
    stat_function(data = data, fun = funct, size = 2, colour = "grey")+
    geom_point(size = 2)+
    theme_bw()+
    theme()
}
