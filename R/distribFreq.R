#' Para construção de distribuição de frequências nos moldes da disciplina.
#'
#' Dá tratamento especial para cada tipo de variável e recorre ao método mais simples para quantitativas.
#'
#' @param x Um conjunto de dados, em forma de vetor. Os dados sobre os quais deseja-se a distribuição de frequências.
#' @param varName Um texto, representando o nome da variável sob estudo.
#'
#' @return Uma lista, contemplando:
#'
#' qualitativeData - Os dados qualitativizados
#'
#' distFreq - A distribuição de freuências
#'
#' grid -  A grade de pontos que compõem as categorias, para o caso de quantitaitvas
#' @export
#'
#' @examples
#' distFreq(x = c(x = runif(100), varName = "exemplo"))
distFreq=function(x, varName=""){
  ret = NULL
  # if the variable is QUALITATIVE
  if(class(x)=="character" | class(x)=="factor" | class(x)=="ordered"){
    distFreq = table(x)
    ret=list(qualitativeData = x, distFreq = distFreq, grid = names(distFreq))
  }
  #if the variable is quantitative    minimum = min(x); maximum = max(x)
  else if(class(x)=="numeric" | class(x)=="integer"){
    maximum = max(x, na.rm=TRUE)
    minimum =  min(x, na.rm=TRUE)
    n = length(x)
    k = round(sqrt(n))
    h = (maximum-minimum)/k
    #if(class(x)=="integer") h = round(h)
    grid = seq(minimum, maximum, by=h)
    qualitativedData =
      cut(x, grid, include.lowest=TRUE, right=FALSE)
    tb = table(qualitativedData)
    tbRelat = tb/n
    tbCum = cumsum(tb)
    tbCumRelat = tbCum/n
    aux = cbind(tb, tbRelat, tbCum, tbCumRelat)
    ret=list(qualitativeData = qualitativedData, distFreq = tb, grid = grid)
    #ret = list(grouped = freq, distribution = aux)
    print(aux)
    #hist(x, breaks=grid, main = paste("Histograma:", varName), xlab = varName, ylab="frequencia absoluta", col = "royalblue4")
  }
  return(ret)
}

# tempos=c(2, 14, 1.2, 7, 5.4, 3, 7, 3.5, 4, 1.9, 4.7, 1.3, 7.9, 9.5, 1.2)
# rec = distFreq(tempos, varName="tempo at? a falha")
