TCL_populacaoFinita = function(n, N){
  #srand(0)
  alturas = rnorm(n=N, mean = 1.66, sd = .03); #View(alturas)
  nSize = length(n)
  for(i in 1:nSize){
    amostra = NULL
    for (j in 1:100){
      amostra[j] = mean(sample(x=alturas, size = n[i]))
    }
    #ourHistogram(x=amostra, xlab = "média amostral", main = paste("n=",n[i]), density = TRUE)
    media = 1.66
    desvPad = (.03/sqrt(n[i]))*sqrt((N-n[i])/(N-1))
    fdp_normal = function(x){
      dnorm(x, mean = media, sd = desvPad)
    }
    hist(amostra, main=paste("n=",n[i], "-População Finita"), freq = FALSE)
    curve(expr = fdp_normal, type = "l", col="blue", add=TRUE)
  }
  #curve(expr = fdp_normal, type = "l", col="blue", add=TRUE)
}
TCL_populacaoInfinita = function(n){
  #srand(0)
  nSize = length(n)
  for(i in 1:nSize){
    amostra = NULL
    for (j in 1:1000){
      # amostra[j] = mean(rnorm(n=n[i], mean = 1.66, sd = .03))
      amostra[j] = mean(rexp(n=n[i], rate = 1))
    }
    #ourHistogram(x=amostra, xlab = "média amostral", main = paste("n=",n[i]), density = TRUE)
    # media = 1.66 #mean(amostra)
    # desvPad = .03/sqrt(n[i])#sd(amostra)
    media = 1
    desvPad = 1/sqrt(n[i])#sd(amostra)
    fdp_normal = function(x){
      dnorm(x, mean = media, sd = desvPad)
    }
    hist(amostra, main=paste("n=",n[i], "-População Infinita"), freq = FALSE)
    curve(expr = fdp_normal, type = "l", col="blue", add=TRUE)
  }
  #curve(expr = fdp_normal, type = "l", col="blue", add=TRUE)
}
# TCL_populacaoFinita(n=c(10, 50, 100, 1000), N=10000)
TCL_populacaoInfinita(n=c(5, 10, 50, 100, 1000))
trash = function(){
  x = rexp(n = 1000, rate = 1)
  hist(x)
}

