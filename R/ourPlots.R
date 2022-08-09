#'Time series plots
#'
#'A time series plot of single predictors and combiners is saved at the desired
#'path or plotted.
#'
#'@param all.series A data frame. The first time series values are related to
#'  single predictors and the last ones are related to combiners. The dates
#'  related to the time series values must also be present.
#'@param n A numeric. The size of the training set.
#'@param v A numeric. The size of the validation set. It is not adequately used
#'  in the moment.
#'@param m A numeric. The size of the test set.
#'@param seriesName A character. The mnemonic of the time series. This text is
#'  used to name the file to be saved.
#'@param nCombinators A numeric. The number of combination predictors in the
#'  data set.
#'@param ylab A character. The label of the y-axis. If \code{NULL},
#'  \code{seriesName} is considered.
#'@param RESULTS_PATH A character. The directory path at which the figure must
#'  be saved. If \code{NULL}, then the figure is just plotted.
#'
#'@return None.
#'@export
#'
#'@author Paulo Renato A Firmino
#' @examples ...
generateTimeSeriesGraphic = function(all.series, n, v=0, m
                                     , seriesName=seriesNm_i
                                     , nCombinators = 5, ylab = NULL
                                     , RESULTS_PATH = NULL
                                     , dateColName = "date"){
  n_m = nrow(all.series)
  dates = as.character(all.series[[dateColName]])
  if(length(dates)==0){
    dates = 1:n_m
  }
  else{
    nmCols = colnames(all.series)
    dateIndex = which(nmCols==dateColName)
    all.series = all.series[,-c(dateIndex)]
  }
  min_y = min(all.series, na.rm = TRUE)
  max_y = max(all.series, na.rm = TRUE)
  max_y = max_y #+ 0.2*(max_y - min_y)
  nSeries = ncol(all.series)
  if(is.null(nSeries)){
    nSeries=1
  }
  nSingleModels = nSeries - 1 - nCombinators
  all.colors = #colors()
    c("aliceblue", "antiquewhite", "aquamarine", "azure", "beige", "bisque", "black", "blanchedalmond", "blue", "blueviolet", "brown", "burlywood", "cadetblue", "chartreuse", "chocolate", "coral", "cornflowerblue", "cornsilk", "cyan", "darkblue", "darkcyan", "darkgoldenrod", "darkgray", "darkgreen", "darkgrey", "darkkhaki", "darkmagenta", "darkolivegreen", "darkorange", "darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue", "darkslategray", "darkslategrey", "darkturquoise", "darkviolet", "deeppink", "deepskyblue", "dimgray", "dimgrey", "dodgerblue", "firebrick", "floralwhite", "forestgreen", "gainsboro", "ghostwhite", "gold", "goldenrod", "gray", "green", "greenyellow", "grey", "honeydew", "hotpink", "indianred", "ivory", "khaki", "lavender", "lavenderblush", "lawngreen", "lemonchiffon", "lightblue", "lightcoral", "lightcyan", "lightgoldenrod", "lightgoldenrodyellow", "lightgray", "lightgreen", "lightgrey", "lightpink", "lightsalmon", "lightseagreen", "lightskyblue", "lightslateblue", "lightslategray", "lightslategrey", "lightsteelblue", "lightyellow", "limegreen", "linen", "magenta", "maroon", "mediumaquamarine", "mediumblue", "mediumorchid", "mediumpurple", "mediumseagreen", "mediumslateblue", "mediumspringgreen", "mediumturquoise", "mediumvioletred", "midnightblue", "mintcream", "mistyrose", "moccasin", "navajowhite", "navy", "navyblue", "oldlace", "olivedrab", "orange", "orangered", "orchid", "palegoldenrod", "palegreen", "paleturquoise", "palevioletred", "papayawhip", "peachpuff", "peru", "pink", "plum", "powderblue", "purple", "red", "rosybrown", "royalblue", "saddlebrown", "salmon", "sandybrown", "seagreen", "seashell", "sienna", "skyblue", "slateblue", "slategray", "slategrey", "snow", "springgreen", "steelblue", "tan", "thistle", "tomato", "turquoise", "violet", "violetred", "wheat", "white", "whitesmoke", "yellow", "yellowgreen")
  aux = max(1, nSeries)
  plot_colors <- all.colors[7]
  if(nSingleModels>0){
    plot_colors = c(plot_colors, all.colors[seq(from=50, by=1, length.out = nSingleModels)])
  }
  if(nCombinators>0){
    plot_colors = c(plot_colors, all.colors[seq(from=(50+nSingleModels+1), by=1, length.out = nCombinators)])
  }
  # pch <- c(NA,rep(1,aux),seq(from=2, by=2, length.out = nCombinators))#rep(NA, nSeries)# an integer code for one of a set of graphics symbols
  # lty <- c(1,rep(3,aux), rep(3, nCombinators))#line type. Line types can either be specified as an integer (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash)
  #pch_seq = rep(2:6, (aux+nCombinators))[1:(aux+nCombinators)]
  pch <- c(NA,1:aux)#rep(NA, nSeries)# an integer code for one of a set of graphics symbols
  lty <- c(1, 2:aux)#line type. Line types can either be specified as an integer (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash)
  lwd <- c(3, rep(1, nSingleModels), rep(1, nCombinators))# line width
  types = c("l", rep("b",nSingleModels), rep("b", nCombinators))#1-character string giving the type of plot desired. The following values are possible, for details, see plot: "p" for points, "l" for lines,...

  plotForecasts = function(modelsType = "SingleModels", seriesIndexes) {
    if(!is.null(RESULTS_PATH)){
      png(filename = paste(RESULTS_PATH, seriesName, "_", modelsType, "_Series.png", sep=""), width = 2.7*480, height = 1.25*480)
      #png(filename = paste(RESULTS_PATH, seriesName, "_Series.png", sep=""), width = 2.7*480, height = 1.25*480)
    }
    # Ploting the plot box
    mai = c(3,2.5,0,0); mar = c(1,1,1,1); mgp = c(3,1,0); par(family="serif", mar=mar, mgp=mgp, mai=mai)
    plot(x=c(1,(n+v+m)), y=c(min_y, 1.2*max_y), type="n", ylim=c(min_y,max_y), axes=FALSE, ann=FALSE)#, lwd=lwd[i], lty=lty[i], pch=NA, col=plot_colors[i])
    by_x = max(round(0.1*n_m), 1); at_x = sort(c(n, (n_m-1), n_m, seq(from=1, to=n_m, by = by_x)))
    by_y = round((max_y - min_y)/5, 2)
    par(family="serif", mar=mar, mgp=mgp, mai=mai)
    axis(1, at=at_x, lab=dates[at_x], las=2
         , cex.lab=1, cex.axis = 3, cex=2)#, las=2)#lab=seq(from=1, to=seriesSize, by = by_x))
    axis(2, at=seq(from=round(min_y,2), to=round(max_y, 2), by = by_y), las=1
         , cex.lab=2, cex.axis = 3, cex=1)#, las=2)
    # Create box around plot
    box()
    title(xlab="date", col.lab=rgb(0,0,0)
          , cex.lab=3, cex.axis = 3, cex=1, line = 14)#, las=2)
    if(is.null(ylab)){ylab=seriesName}
    title(ylab=ylab, col.lab=rgb(0,0,0)
          , cex.lab=3, cex.axis = 3, cex=1, line = 10)#, las=2)
    mar = c(10,10,1,1); mgp = c(4,1,0); par(family="serif", mar=mar, mgp=mgp, mai=mai)
    # blocking training, validation, and test sets
    #  if(from==1){
    #points(x=c(n+1:(n+v+m)), y=all.series$target[n_treinamento+1:N], type="l", lty=1, col="black", lwd=lwd+2)
    # Insere a linha de divide a parte a ser prevista da de treinamento
    lines(x=c(n+.5,n+.5),c(min_y,max_y), col="blue", lwd=3, lty=2)
    #lines(x=c((n+v),(n+v)),c(min_y,max_y), col="orange", lwd=2, lty=1)
    #  }

    #Plot the series
    legend = NULL
    seriesNm = dimnames(all.series)[[2]]
    nSeries = length(seriesIndexes)
    if(nSeries==1){
      i = 1
      lines(x=(1:(n+v+m)), y=all.series, type=types[i], pch=pch[i], col=plot_colors[i], ylim=c(min_y,max_y), ann=FALSE, lwd=lwd[i], lty=lty[i])#, axes=FALSE
      legend = c(legend, seriesNm[i])
    }
    else{
      for (j in 1:nSeries){
        i = seriesIndexes[j]
        lines(x=(1:(n+v+m)), y=all.series[,i], type=types[i], pch=pch[i], col=plot_colors[i], ylim=c(min_y,max_y), ann=FALSE, lwd=lwd[i], lty=lty[i])#, axes=FALSE
        legend = c(legend, seriesNm[i])
      }
    }
    # Create a title with a red, bold/italic font
    title(main="", col.main="red", font.main=4)
    # title(xlab= "time index", col.lab=rgb(0,0,0))
    # title(ylab= seriesName, col.lab=rgb(0,0,0))

    # Create a legend
    #  legend <- c(expression(u[t]), expression(hat(u)[t]), expression(x[paste(t,1)]), expression(x[paste(t,2)]), expression(x[paste(t,3)]), expression(SA))
    legend[1] = seriesName
    legend(x=max(.1*n, 1), y=max_y, legend=legend, cex=3, col=plot_colors[seriesIndexes]
           , pch=pch[seriesIndexes], lty=lty[seriesIndexes], lwd = 1*lwd[seriesIndexes], ncol=5, bty="n")
    text(x=0.3*(n+v+m), y=0.94*max_y,
         labels = "www.mesor.com.br", pos = 3, vfont = c("sans serif","bold")
         , cex = 3, col="blue", bg = "orange", bty="o")
    if(!is.null(RESULTS_PATH)){
      dev.off()
    }
  }
  singleSeriesIndexes = 1
  if(!is.null(nSeries)) {
    singleSeriesIndexes = 1:(nSingleModels+1)
  }
  plotForecasts(modelsType = "SingleModels", seriesIndexes = singleSeriesIndexes)
  if(nCombinators>0){
    combinedSeriesIndexes = c(1, (nSingleModels+2):nSeries)
    plotForecasts(modelsType = "CombinedModels", seriesIndexes = combinedSeriesIndexes)
  }
}
RESULTS_PATH = "C:/Users/praf6/Downloads/"
timeSeriesDf = read.csv(file = paste(RESULTS_PATH, "Brazil_timeSeries.csv", sep="")
                , header = TRUE, sep = "")
View(timeSeriesDf)
#TIME SERIES
n_m = nrow(timeSeriesDf) #the total size of the series
trainingPercentage = .7 #the percentage of data dedicated to the train set
n = round(trainingPercentage*n_m) # the training set size
m = n_m - n # the testing set size

generateTimeSeriesGraphic(all.series = timeSeriesDf, n = n, m = m
                          , seriesName = "COV_BR", nCombinators = 0
                          , ylab = "nº casos Covid-19"
                          , RESULTS_PATH = RESULTS_PATH)
