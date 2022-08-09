adicao=function(valor1, valor2){
  resultado = valor1 + valor2
  return(resultado)
}
adicao(2, 3)
simulandoLancamentoDeMoeda = function(nLancamentos=10){
  set.seed(3)
  x=runif(n=nLancamentos)
  resultados=ifelse(x<=0.5,"Cara", "Coroa")
  return(resultados)
}
nLancamentos  = 100000
sim1 = simulandoLancamentoDeMoeda(nLancamentos=nLancamentos)
head(sim1)
df = table(sim1); df
# View(df)
# df[["Cara"]]
print(prop.table(df))
library(doParallel)
sampleSizes = seq(from = 1, to = nLancamentos, by = max(1, nLancamentos/100))
n_sampleSizes = length(sampleSizes)
propCara =
  foreach(i = 1:n_sampleSizes, .combine = c)%do%{#i=100
    n_i = sampleSizes[i]
    df_i = table(sim1[1:n_i])
    dp_i = prop.table(df_i)
    ret = tryCatch(expr = {dp_i[["Cara"]]}
                   , error = function(e){
                     # message(paste("Error in 'dp_i[["Cara"]]'.", e));
                     return(0)
                   })
    return(ret)
  }
plot(x = sampleSizes, y = propCara, type="h"
     , xlab="tamanho amostral", ylab = "proporção de caras")
lines(x = sampleSizes, y = rep(0.5, n_sampleSizes), type="l", col = "red")
