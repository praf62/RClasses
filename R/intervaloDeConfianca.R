confidenceInterval = function(sample = NULL, confidenceLevel=NULL, N=NULL
                              , populationalSd=NULL){
  n=length(sample)
  sampleMean = mean(sample, na.rm=TRUE)
  sampleMean = round(sampleMean, 3)
  v_1_alpha_2 = 1-(1-confidenceLevel)/2; text=NULL
  quant_m = NULL; text=NULL; sd = NULL
  if(!is.null(populationalSd)){#sd is the parameter value
    quant_m = round(qnorm(p = v_1_alpha_2), 2)
    sd = populationalSd
  } else {#sd is an  estimate of the parameter
    quant_m = round(qt(p = v_1_alpha_2, df=(n-1)), 3)#QUANTIL DA T-STUDENT
    text = paste(", df=", (n-1), sep="")
    sd = sd(sample, na.rm=TRUE)
    sd = round(sd, 3)
  }
  correctionFactor = ifelse(is.finite(N), sqrt((N-n)/(N-1)), 1)
  correctionFactor = round(correctionFactor, 3)
  sd_sampleMean = (sd/sqrt(n))*correctionFactor
  sd_sampleMean = round(sd_sampleMean, 3)
  lower = sampleMean - quant_m*sd_sampleMean
  upper = sampleMean + quant_m*sd_sampleMean
  lower = round(lower, 3)
  upper = round(upper, 3)
  print(paste("quant_m=", quant_m, ", sampleMean=", sampleMean
              , ", sd_sampleMean=",sd_sampleMean
              , ", lower=", lower, ", upper=", upper, text, sep=""))
  #print(paste(, sep=""))
  return(list(lower=lower, upper=upper))
}

#binarySample = sample
confidenceIntervalForProportion= function(binarySample, N=Inf, confidendeLevel = .93, isConservative=TRUE){
  p_obs = mean(binarySample)
  n= length(binarySample)
  correctionFactor = ifelse(is.finite(N), sqrt((N-n)/(N-1)), 1)
  sigma = ifelse(isConservative==TRUE, .5, sqrt(p_obs*(1-p_obs)))
  sigma_p = (sigma/sqrt(n)) *correctionFactor
  alpha = 1 - confidendeLevel
  z = qnorm(p=(1-alpha/2))
  l = p_obs - z*sigma_p
  u = p_obs + z*sigma_p
  print(paste("(l, u): ", l, u))
}
#N = 60
#N = Inf
sampleContinuous = rexp(n = 20, rate = 1)
mean(sampleContinuous)
boxplot(sampleContinuous)
confidenceInterval(sample = sampleContinuous, confidenceLevel = .95
                   , N = 100, populationalSd = NULL)
#confidenceIntervalForProportion(binarySample = sample, N = N, isConservative = TRUE, confidendeLevel = .93)
