#' Computes classical box-plots.
#'
#' Is based on quartiles.
#'
#' @param x A vector. The data set under study.
#' @param ylab A character. The label to be printed in the y axis of eventual
#'   plots.
#'
#' @return characteristics of the boxplot
#' @export
#'
#' @examples
#' classicBoxPlot(rnorm(100), ylab="example")
classicBoxPlot = function(x, ylab=NULL){
  Q = as.numeric(quantile(x = x, probs = c(.25, .5, .75), type = 1))
  IQD = (Q[3] - Q[1])
  LL = max(min(x, na.rm=T), Q[1] - 1.5*IQD)
  UL = min(max(x, na.rm=T), Q[3] + 1.5*IQD)
  bp = boxplot(x, plot = F, ylab=ylab)
  quantities = list(LL=LL, Q1 = Q[1], Q2 = Q[2], Q3 = Q[3], UL=UL)
  print(as.matrix(quantities))
  bp$stats = as.matrix(as.numeric(quantities))
  # bp$stats[1] = LL
  # bp$stats[2] = Q[1]
  # bp$stats[3] = Q[2]
  # bp$stats[4] = Q[3]
  # bp$stats[5] = UL
  bp2 = bxp(z = bp, ylab = ylab)#, show.names = TRUE)
  outlier_indexes = which(x<quantities$LL | x>quantities$UL)
  outlier_values = x[outlier_indexes]
  # print(paste("outlier indexes:", outlier_indexes))
  # print(paste("outlier values:", outlier_values))
  print(cbind(outlier_index = outlier_indexes
              , outlier_value = outlier_values))
  return (bp2)
}
