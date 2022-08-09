#para ter controle sobre os números pseudo-aleatórios gerados...
# set.seed(1)#para controlar a sequência gerada de números pseudo-aleatórios

#amostragem aleatoria simples - AASimples
getSimpleRandomSample = function(dataFrame, sampleSize){
  N = nrow(dataFrame) #tamanho da população
  n = sampleSize # tamanho da amostra
  #amostra aleatória de índices
  sampleIndexes = sample(x = 1:N, size = n, replace = FALSE)
  #separando a amostra aleatória correspondente aos índices sorteados
  sampleData = dataFrame[sampleIndexes, ]
  return(sampleData)
}
#amostragem aleatoria sistemática - AASistemática
getMapingBasedSimpleRandomSample = function(dataFrame, sampleSize){
  N = nrow(dataFrame) #tamanho da população
  n = sampleSize # tamanho da amostra
  ti = 1; tf = N # mínimo e máximo do intervalo de chaves
  d = round((tf - ti + 1)/n) # distância entre índices subsequentes
  #primeiro índice sorteado aleatoriamente (como na AASimples)
  i0 = sample(x = ti:tf, size = 1, replace = FALSE)
  #sorteando índices a partir de i0, distanciados por d
  sampleIndexes = i0 + seq(from=0, by = d, length.out = n)
  #contornando o problema de índice maior que o máximo possível
  sampleIndexes[sampleIndexes>tf] =
    sampleIndexes[sampleIndexes>tf] - tf + ti - 1
  #separando a amostra aleatória correspondente aos índices sorteados
  sampleData = dataFrame[sampleIndexes, ]
  return(sampleData)
}
#amostragem aleatoria estratificada proporcional - AAEstrProp
getProportionalStratifiedRandomSample = function(dataFrame
                                                 , sampleSize
                                                 , stratumVariable){
  #distribuição da variável de estrato na população
  n = sampleSize
  probDist = prop.table(table(dataFrame[[stratumVariable]])); #View(probDist)
  ns = round(n*probDist); #View(ns)
  stratums = names(probDist)
  nStratums = length(stratums)
  sampleData = NULL
  for(i in 1:nStratums){
    stratum_i = stratums[i]
    dataFrame_i = dataFrame[dataFrame[[stratumVariable]] == stratum_i, ]#View(dataFrame_i, title = stratum_i)
    N_i = nrow(dataFrame_i) #tamanho da iª população
    n_i = as.integer(ns[i]) # tamanho da iª amostra
    sampleData_i = getSimpleRandomSample(dataFrame = dataFrame_i, sampleSize = n_i)#View(sampleData_i, title = paste("amostra_", stratum_i) )
    sampleData = rbind(sampleData, sampleData_i)
  }
  return(sampleData)
}
#amostragem aleatória simples por mapeamento - AASM
getMapingSimpleRandomSample = function(bounds, sampleSize){
  dimensions = names(bounds)
  nDimensions = length(dimensions)
  # N = nrow(dataFrame) #tamanho da população
  n = sampleSize # tamanho da amostra
  #amostra aleatória de coordenadas
  sampleCoordinates = list()
  for(i in 1:nDimensions){#i=1
    dim_i = dimensions[i]
    #amostra aleatória simples para a dimensão dim_i
    rs_i = runif(n = n, min = bounds[[i]][["min"]], max = bounds[[i]][["max"]])
    sampleCoordinates[[dim_i]] = rs_i
  }
  sampleCoordinates = as.data.frame(sampleCoordinates)#View(sampleCoordinates)
  return(sampleCoordinates)
}

#Descritiva (script01)
descritiva = function(data){
  #para carregar funções criadas por mim
  debugSource('./R/classicBoxPlot.R', encoding = 'UTF-8')
  debugSource('./R/distribFreq.R', encoding = 'UTF-8')
  debugSource('./R/iqv_function.R', encoding = 'UTF-8')
  debugSource('./R/mode_function.R', encoding = 'UTF-8')
  distFreq(x = data$Fornecedor)
  ourMode(sample = data$Fornecedor, xlab = "Fornecedor", toPrint = TRUE)
  ourMode(sample = data$TempoFalha, xlab = "Tempo até a falha", toPrint = TRUE)#, minimalAmplitudeRatioForGrouping = .2)
  ourMode(sample = data$nReincidenciaFalhas, xlab = "Nº de reincidências de falhas", toPrint = TRUE, minimalAmplitudeRatioForGrouping = .2)
}

#Exemplo: Conjunto de dados de manutenção
#Ler o conjunto
BD01 <- read.csv(file = "./data/BD01.csv", dec = ",", encoding="UTF-8", sep=";", quote="")
#visualizar o conjunto
# View(BD01)

# aaSimples_data = getSimpleRandomSample(dataFrame = BD01, sampleSize = 5)
# View(aaSimples_data)
# descritiva(aaSimples_data)

# aaSistematica = getMapingBasedSimpleRandomSample(dataFrame = BD01, sampleSize = 5)
# View(aaSistematica)
#
# aaEstrProp = getProportionalStratifiedRandomSample(dataFrame = BD01
#                                                    , sampleSize = 5
#                                                    , stratumVariable = "Fornecedor")
# View(aaEstrProp)

#Exemplo: Mapeamento
bounds = list(lat = list(min = -10, max = -1)
              , long = list(min = 15, max = 20)
              , tempo = list(min = 7, max = 22))
aasm_data = getMapingSimpleRandomSample(bounds = bounds, sampleSize = 5)
View(aasm_data)


# descritiva(aaEstrProp)


#apenas paara exemplificar algumas coisas...
rascunho = function(){
  N = 1000
  n = 10
  indexes = sample(x = seq(from=1, to=N, by=1), size = n, replace = FALSE)

#pergunta leoncio
  a = c(1:10)
  b = c(2:11)
  m_c = cbind(col_a = a, col_b = b); View(m_c) #acumula colunas
  m_r = rbind(rowl_a = a, row_b = b); View(m_r)#acumula linhas
}
