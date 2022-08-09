#Testes de aderência
#0. Exemplo do lançamento de dado por 300 vezes
# o = c(43, 49, 56, 45, 66, 41)#observado
o = c(43, 49, 56, 45, 66, 41)#observado (situação de assimetria)
n = sum(o)#tamanho da amostra
p0 = rep(1/6, 6)#distribuição pretendida (Uniforme)
#p: a distribuição de probabilidades subjacente a 'o'
e = n*p0#frequências esperadas (sob H0: p = p0)
plot(o, type="h")
lines(e, col="blue", type="l")

ct = chisq.test(x = o)#Teste de homogeneidade Qui-Quadrado de Pearson
ct$statistic #a estatística de teste
ct$p.value#o p-valor do teste (a probabilidade das difereças
          # observadas, ou maiores, terem se dado ao acaso,
          # supondo H0 verdadeira)
ct = chisq.test(x = o, p = p0)#Teste de homogeneidade Qui-Quadrado de Pearson
ct$p.value#o p-valor do teste (igual ao anterior...)

chisq.adherence.test = function(observed, expected){
  residuals = (observed - expected)/sqrt(expected)#the Pearson residuals, (observed - expected) / sqrt(expected).
  statistic = sum(residuals^2)#Estatística Qui-Quadrado
  df = length(observed)-1#nº de graus de liberdade
  p.value = pchisq(q = statistic, df=df, lower.tail = FALSE)
  ret = list(statistic = statistic, df = df
             , p.value = p.value, observed = observed
             , expected = expected, residuals = residuals)
  return(ret)
}
cat = chisq.adherence.test(o, e)#Teste de aderência Qui-Quadrado de Pearson
cat$p.value#o p-valor do teste

#1. (Bussab & Morettin) Um estudo sobre 150 acidentes de trabalho numa
# indústria revelou a distribuição de frequências ao lado. Verifique se os
# acidentes se dão de maneira homogênea ao longo da semana
nAcidentes = c(32, 40, 20, 25, 33)
ct = chisq.test(x = nAcidentes)#Teste de homogeneidade Qui-Quadrado de Pearson
ct$p.value#o p-valor do teste
ct$statistic
#com alpha=10%, rejeito H0 (decido que o nº de acidentes varia
# ao longo dos dias da semana)

#2. (Bussab & Morettin) Estuda-se se a distribuição de Poisson (λ=3.87)
# adere ao conjunto de dados sintetizados na tabela
# abaixo, sobre o nº de substâncias radioativas desintegradas em uma
# amostra de 2608 unidades de tempo de 7.5 segundos.
#Nº de desintegrações 0 1 2 3 4 5 6 7 8 9 ≥10
# Frequência observada 57 203 383 525 532 408 273 139 45 27 16
o = c(57, 203, 383, 525, 532, 408, 273, 139, 45, 27, 16)
p0 = dpois(x = 0:9, lambda = 3.87)
p0 = c(p0, 1-sum(p0))
n = sum(o)#tamanho da amostra
e = n*p0#frequencias esperadas
which(e<=5)
plot(o, type="h")
lines(e, col="blue", type="l")
cat = chisq.adherence.test(observed = o, expected = e)
cat$p.value
cat$statistic

#3. Questiona-se se os dados a seguir são normalmente distribuídos
amostra=c(7.53, 7.67, 12.78, 10.82, 10.74, 10.64, 7.75, 7.61, 10.83, 8.49)
x_bar = mean(amostra)
s = sd(amostra)
n = length(amostra)
hist(amostra, freq = FALSE)
normDensity = function(x){
  dnorm(x, mean = x_bar, sd = s)
}
curve(expr = normDensity(x), from =  min(amostra)
      , to = max(amostra), col="blue", add=TRUE)
kst = ks.test(x = amostra, "pnorm", mean=x_bar, sd = s)#Teste de Kolmogorov-Smirnov
kst$p.value
kst$statistic
sht = shapiro.test(x = amostra)#Teste de normalidade (adequado para pequenas amostras)
sht$p.value
sht$statistic

#4. Questiona-se se os dados a seguir são exponencialmente distribuídos.
amostra=c(0.93, 0, 1.09, 0.62, 0.37, 0.54, 0.74, 0.06, 0.46, 0.75)
n = length(amostra)
x_bar = mean(amostra)
lambda = 1/x_bar
hist(amostra, freq = FALSE)
expDensity = function(x){
  dexp(x,  rate = lambda)
}
curve(expr = expDensity(x), from =  min(amostra)
      , to = max(amostra), col="blue", add=TRUE)
kst = ks.test(x = amostra, "pexp", rate = lambda)
kst$p.value
kst$statistic

#5. Com números pseudo-aleatórios
amostra=rexp(n = 100, rate = 1.0)
x_bar = mean(amostra)
lambda = 1/x_bar
hist(amostra, freq = FALSE)
expDensity = function(x){
  dexp(x,  rate = lambda)
}
curve(expr = expDensity(x), from =  min(amostra)
      , to = max(amostra), col="blue", add=TRUE)
kst = ks.test(x = amostra, "pexp", rate = lambda)
kst$p.value

kst = ks.test(x = amostra, "pnorm", mean = x_bar, sd= sd(amostra))
kst$p.value

