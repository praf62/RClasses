# source('./R/iqv_function.R', encoding = 'UTF-8')

#' A simple function for obtaining a single mode of a given sample.
#'
#' For quantitative variables, the Czuber's mode can be adopted, depending on
#' the value of the ratio: total amplitude divided by the sample size. It is
#' supposed that just one category/subinterval is the most frequent. For general
#' purposes, sophistication must be introduced in the code. the R package
#' \code{\link{modeest}} also involves a number of functions for mode
#' calculation.
#'
#' @param sample A vector. The data set under study.
#' @param xlab A character. The label to be printed in the x axis of eventual
#'   plots.
#' @param toPrint A logical. If TRUE, then plots and frequency distributions are
#'   exhibited.
#' @param minimalAmplitudeRatioForGrouping A numeric. For quantitative
#'   variables, if \code{amplitude(sample)/length(sample)} is lesser than
#'   minimalAmplitudeRatioForGrouping) then no subinterval construction of the
#'   data is necessary.
#'
#' @return A vector, numeric or character, depending on the nature of
#'   \code{sample}. The respective mode.
#' @export
#'
#' @examples
#' x = rnorm(100)
#' ourMode(sample = x, xlab = "example", toPrint = TRUE)
ourMode=function(sample, xlab="", toPrint = TRUE, minimalAmplitudeRatioForGrouping = .1){
  x=sample
  modes=NULL
  isSimpleApproach = TRUE#if quantitative with little variability or qualitative
  isQuantitative = ifelse(class(x)=="numeric" | class(x)=="integer", TRUE, FALSE)
  if(isQuantitative){
    n = length(x)
    limits = range(x, na.rm=TRUE)
    amplitude = limits[2]-limits[1]
    ratio = amplitude/n
    if(ratio >= minimalAmplitudeRatioForGrouping){
      isSimpleApproach = FALSE
    }
  }

  #if(class(x)=="character" | class(x)=="factor" | class(x)=="ordered"){# if the variable is QUALITATIVE
  if(isSimpleApproach){
    distFreq=table(x)
    if(toPrint){
      if(isQuantitative){
        plot(table(x), ylab="frequencia absoluta", xlab=xlab, main="")
      }
      else {
        barplot(distFreq, xlab=xlab, main="", ylab="frequencia absoluta")
      }
    }
    modesFreq = distFreq[distFreq==max(distFreq)]
    modes = names(modesFreq)
  }
  else {#if(class(x)=="numeric" | class(x)=="integer"){#if the variable is quantitative
    n=length(x)
    k = round(sqrt(n))
    limits=range(x, na.rm=T)
    amplitude=limits[2]-limits[1]
    h = amplitude/k
    breaks = seq(from=limits[1], to=limits[2], by=h)
    groupedData = cut(x, breaks, include.lowest=TRUE, right=FALSE)
    distFreq=table(groupedData)
    if(toPrint){
      print(cbind(distFreq))
      hist(x, breaks, right = FALSE, ylab="frequencia absoluta", freq=TRUE, xlab=xlab, main="")
    }
    categories=names(distFreq)
    maxFreq = max(distFreq, na.rm = T)
    modalIntervals = as.numeric(which(distFreq==maxFreq))
    minIndex = maxIndex = modalIntervals
    n_modalIntervals = length(modalIntervals)
    l = breaks[modalIntervals]
    if(n_modalIntervals > 1
       & length(which(modalIntervals==(modalIntervals[1]:modalIntervals[n_modalIntervals])))
       ==n_modalIntervals){
      maxFreq = maxFreq*n_modalIntervals
      minIndex = modalIntervals[1]
      maxIndex = modalIntervals[n_modalIntervals]
      l = l[1]
      h = h*n_modalIntervals
    }
    freqAnt=ifelse(length(which(modalIntervals==1))>0,0,distFreq[[minIndex-1]])
    freqPost=ifelse(length(which(modalIntervals==k))>0,0,distFreq[[maxIndex+1]])
    deltaAnt=maxFreq-freqAnt
    deltaPost=maxFreq-freqPost
    modes=l+(deltaAnt/(deltaAnt+deltaPost))*h
    if(class(x)=="numeric" &  toPrint){
      abline(v=modes, col = "blue", lwd=2)
    }
#     modalIntervals = distFreq[distFreq==max(distFreq)]
#     maxFreq=distFreq[[1]]
#     l=limits[1];
#     for(i in 2:k){
#       if(distFreq[[i]]>maxFreq){
#         l=breaks[i]
#         maxFreq=distFreq[[i]]
#         modalInterval=i
#       }
#     }
#     freqAnt=0
#     if(modalInterval>1)
#       freqAnt=distFreq[[modalInterval-1]]
#     freqPost=0
#     if(modalInterval<k)
#       freqPost=distFreq[[modalInterval+1]]
#     deltaAnt=maxFreq-freqAnt
#     deltaPost=maxFreq-freqPost
#     mode=l+(deltaAnt/(deltaAnt+deltaPost))*h
    if(class(x)=="integer"){#if the variable is "DISCRETE"
      modes=round(modes)
    }
  }
  print(paste("mode:", modes))
  return (modes)#returning the computed mode
}

# BD01 = read.csv("C:/cloud/Dropbox/Public/UFCA/Ensino/CRAN R_aulas/data/BD01.csv", sep=";", dec="."); View(BD01) #to automatically read the dataset
# ourMode(sample = BD01$Fornecedor, xlab = "Fornecedor")#applying ourMode function to the BD01$Fornecedor sample
#
# ourMode(sample=BD01$Degradacao, xlab="Degradação")#applying ourMode function to the BD01$Degradacao sample
# ourMode(BD01$TempoFalha, "Tempo at? a Falha")#applying ourMode function to the BD01$TempoFalha sample
# ourMode(BD01$nReincidenciaFalhas, "nReincidenciaFalhas")#applying ourMode function to the BD01$nFalhas sample
# x=runif(10);ourMode(x, "Unif");#mfv(x, method="naive", bw=(max(x)-min(x)/round(sqrt(length(x)))))
