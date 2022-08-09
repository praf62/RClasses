#VARIÁVEIS ALEATÓRIAS CONTÍNUAS
#Uniforme Contínua
x = 1:6
fx = dunif(x = x, min = min(x), max = max(x))
plot(x, fx, type='l')

#Exponencial
#x: Tempo até a falha de um avião (em anos)
lambda = .001#taxa de ocorrências de falhas por ano (unidade de medida)
x = 0:1000
fx = dexp(x = x, rate = lambda)
plot(x, fx, type='l')

Rx = pexp(q = 10, rate = lambda, lower.tail = FALSE)#P(X>x)
plot(x, Rx, type='l')

#Normal
#X: estatura de uma pessoa qualquer, do grupo de interesse (em metros)
m = 1.68 #(estatura em metros)
s = 0.3
x = seq(m - 3*s, m + 3*s, length.out = 100)
fx = dnorm(x = x, mean = m, sd = s)
plot(x, fx, type='l')

Fx_3s_esquerda = pnorm(q = m - 3*s, mean = m, sd = s)
Fx_3s_direita = pnorm(q = m + 3*s, mean = m, sd = s)
Fx_3s_direita - Fx_3s_esquerda
Fx = pnorm(q = x, mean = m, sd = s)
plot(x, Fx, type='l')

#Misturando distribuições
P_x_maiorque1.9 = pnorm(q = 1.9, mean = m, sd = s
                        , lower.tail = FALSE)#P(X>1.9)
#Y: nº de pessoas do grupo com estatura superior a 1.9m
#O grupo envolve 200 pessoas
#Y~binomial (n=200, p=P_x_maiorque1.9)
pbinom(q=20, size = 200, prob = P_x_maiorque1.9
       , lower.tail = FALSE)#P(Y>20)



