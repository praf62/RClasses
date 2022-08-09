#VARIÁVEIS ALEATÓRIAS DISCRETAS
#Uniforme Discreta
x = 1:6#exemplo do dado
Px = rep(1/length(x), length(x))
plot(x, Px, type='h')

#Geométrica
p = .0001#probabilidade de sucesso em uma tentativa
x = 1:20
Px = dgeom(x =x, prob = p)/(1-p)#Sigma_X = {1, 2, 3, ...}
plot(x, Px, type='h')
Rx = pgeom(q = 100, prob = p, lower.tail = FALSE)/(1-p)#P(X>x)
plot(x, Rx, type='h')

#Binomial
p = .5#probabilidade de sucesso em uma tentativa
n = 100#tamanho amostral
x = 0:n
Px = dbinom(x = x, size = n, prob = p)
plot(x, Px, type='h')
choose(1e+100,30)

#Hipergeométrica
N=20#tamanho da população
K=8#nº de sucessos na população
n=6#tamanho da amostra
x = 0:(min(n, K))
Px = dhyper(x = x, m = K, n = (N-K), k = n)
plot(x, Px, type='h')

#Poisson
lambda = 1.2#taxa de ocorrências por unidade de medida
x = 0:20
Px = dpois(x = x, lambda = lambda)
#dpois(x = 10, lambda = 4.3) = exp(-4.3)*(10^4.3)/factorial(10)
cbind(x, Px)
plot(x, Px, type='h')
ppois(10, lambda = 4.3)


#Mistura...
#Y: nº de novos casos de Covid-19 por dia em dado local
#Y~Poisson(lambda=1.2)
p = ppois(q =  3, lambda = lambda, lower.tail = FALSE)#P(Y>=3)
n=10
#X: nº de locais em que ocorre ao menos 3 casos de Covid-19 por dia, de uma
#amostra de 10 locais
#X~binomial(n=10, p=P(Y>=3))
x = 0:n
Px = dbinom(x = x, size = n, prob = p)
plot(x, Px, type='h')

