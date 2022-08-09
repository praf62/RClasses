distFrequencia =
  matrix(data =c(  50, 3, 2#original table
                , 900, 27, 18)
         # matrix(data =c(  5, 3, 2
         #                  , 945, 27, 18)
                , nrow = 2, ncol = 3, byrow = TRUE
      , dimnames =
         list(c("Abortou", "Não abortou")
            , c("0 mg de Y", "100 mg de Y", "500 mg de Y")))
View(distFrequencia)
ct = chisq.test(x=distFrequencia)#Teste de associação Qui-Quadrado de Pearson
ct$p.value
ct$statistic#Pearson's Chisquared Statistic
ct$expected
# A mensagem de erro no teste de Pearson reflete o fato de haver valores esperados menores que 5.
# Nessas situações, é melhor recorrer ao teste exato de Fisher:
ft = fisher.test(x = distFrequencia)
ft$p.value

chisq.association.test = function(distFrequencia){
  oCol = colSums(distFrequencia)#o total observado de cada coluna
  oRow = rowSums(distFrequencia)#o total observado de cada coluna

  # expected = ...
  # residuals = (observed - expected)/sqrt(expected)#the Pearson residuals, (observed - expected) / sqrt(expected).
  # statistic = sum(residuals^2)#Estatística Qui-Quadrado
  # df = length(observed)-1#nº de graus de liberdade
  # p.value = pchisq(q = statistic, df=df, lower.tail = FALSE)
  # ret = list(statistic = statistic, df = df
  #            , p.value = p.value, observed = observed
  #            , expected = expected, residuals = residuals)
  # return(ret)
}
# cat = chisq.adherence.test(o, e)#Teste de associação Qui-Quadrado de Pearson
# cat$p.value#o p-valor do teste

#Decomposição em Valores Singulares
#Análise de Correspondência:
#@book{beh2014correspondence,
# title={Correspondence analysis: Theory, practice and new strategies},
# author={Beh, Eric J and Lombardo, Rosaria},
# year={2014},
# publisher={John Wiley \& Sons}
# }
#Análise de Semântica Latente

associationPlot_qualitativeVariables = function(distFrequencia){
  #CALCULANDO OS RESÍDUOS:
  #e_ij = ni. X n.j / n..#valor esperado para a célula da linha i e coluna j (supondo independência - H0)
  #PE_ij = e_ij/n..#probabilidade de ocorrer a conjunção da célula ij sob H0 (independência)
  #PO_ij = o_ij/n..#frequência relativa de ocorrer a conjunção da célula ij
  #residuo_ij = (PO_ij - PE_ij)/sqrt(PE_ij)
  n.. = sum(distFrequencia)#Tamanho amostral
  PO = distFrequencia/n..#distribuição conjunta relativa observada
  pi. = rowSums(PO)#frequência relativa total por linha (marginal da variável-linha)
  p.j = colSums(PO)#frequência relativa total por coluna (marginal da variável-coluna)
  rD = diag(pi.)#matriz diagonal da marginal da variável-linha
  cD = diag(p.j)#matriz diagonal da marginal da variável-coluna
  PE = pi.%*%t(p.j)#distribuição conjunta esperada (sob H0: independência)
  residuals = solve(sqrt(rD)) %*% (PO-PE) %*% solve(sqrt(cD))#
  dimnames(residuals) = dimnames(distFrequencia)
  # View(residuals)

  # dec = svd(x = distFrequencia)#singular value decomposition
  dec = svd(x = residuals)#singular value decomposition
  eigenvalues = dec$d^2
  totalSum = sum(eigenvalues)
  eigenvalues = eigenvalues / totalSum
  PearsonChisquaredStatistic = n..*totalSum
  U = dec$u; S = diag(dec$d); V = dec$v#t(U)U = t(V)V = I (matriz identidade)
  rownames(U) = rownames(distFrequencia)
  rownames(V) = colnames(distFrequencia)
  minDim = min(dim(distFrequencia))
  colnames(U) = paste("Concept", 1:minDim, sep="_")
  colnames(V) = paste("Concept", 1:minDim, sep="_")
  # View(U);View(V);
  k=2#number of components/concepts/eigenvectors to considerer
  Uk = U[,1:k]#the representation of the terms in the k-dimensional space
  Sk = S[1:k, 1:k]#the strength of the components
  Vk = V[, 1:k]#the representation of the documents in the k-dimensional space (easy to store for query search)
  # View(Uk); View(Vk)

  # PLOTING
  # concept space of documents
  library(ggplot2)
  # concept space of terms and documents
  termsLabels = rownames(Uk)
  docsLabels = rownames(Vk)
  df = data.frame(  Concept_1 = c(Uk[,1], Vk[,1])
                    , Concept_2 = c(Uk[,2], Vk[,2])
                    , type = c(  rep("Desfecho", nrow(distFrequencia))
                                 , rep("Dose", ncol(distFrequencia)))
                    , label = c(termsLabels, docsLabels))
  rownames(df)
  # View(df)
  gg =
  ggplot(data= df, aes(x=Concept_1, y=Concept_2, colour = type)) +
    xlab(paste("Concept 1 (", round(100*eigenvalues[1], 1), "%)", sep="")) +
    ylab(paste("Concept 2 (", round(100*eigenvalues[2], 1), "%)", sep="")) +
    geom_point(show.legend = FALSE)+
    directlabels::geom_dl(aes(label = label), method = "smart.grid")
  # ggsave(gg)
  print(gg)

  ret = list(residuals = residuals
             , PearsonChisquaredStatistic = PearsonChisquaredStatistic
             , n.. = n.., PO = PO, PE = PE
             , pi. = pi., p.j = p.j, eigenvalues = eigenvalues
             , eigenvectors = list(U = U, V = V))
  return(ret)
}
apqv = associationPlot_qualitativeVariables(distFrequencia)
apqv$residuals
apqv$PearsonChisquaredStatistic
apqv$eigenvalues

#2º caso
distFrequencia2 =
  matrix(data =c( 50, 3, 2, 0
                , 1,  6, 10, 50
                , 2,  1, 50, 90)
         , nrow = 3, byrow = TRUE
         , dimnames =
           list(c("Sem sintomas", "Mal-estar", "Aborto")
                , c("0 mg de Y", "50 mg de Y", "100 mg de Y", "500 mg de Y")))
View(distFrequencia2)
associationPlot_qualitativeVariables(distFrequencia2)

#PACKAGES FOR CORRESPONDENCE ANALYSIS...
# install.packages("ca")
library(ca)
?ca
ca(distFrequencia)
# ?HairEyeColor
# View(HairEyeColor)
# data("author");
# ?author
selikoff.dat<-matrix(c(310, 212, 21, 25, 7, 36, 158, 35, 102,
                       35, 0, 9, 17, 49, 51, 0, 0, 4, 18, 28),nrow = 5)#From Beh et al. (2014)
dimnames(selikoff.dat) <- list(paste(c("0-9", "10-19", "20-29",
                                       "30-39", "40+")), paste(c("None", "Grade 1", "Grade 2", "Grade 3")))
testData =selikoff.dat
# View(testData)
cat = ca(testData)
plot(cat)
summary(cat)
apqvT = associationPlot_qualitativeVariables(testData)
apqvT$eigenvalues
apqvT$eigenvectors$U
cat$sv^2
# cat$



