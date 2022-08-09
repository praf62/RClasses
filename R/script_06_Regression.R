###### Modelo de regressão Linear Simples - MRLS ###########

## Exemplo 1: Os dados a seguir correspondem às variáveis renda
## familiar (X) e gasto com alimentação - Y (em unidades monetárias)
## para uma amostra de 27 famílias. Em que:
## X = Renda Familiar
## Y = Gasto com Alimentação

## Dados
x <- c(3,5,10,10,20,20,20,30,40,50,60,70,70,80,100,100,100,
       120,120,140,150,180,180,200,200, 1000, 2000)
y <- c(1.5,2.0,6.0,7.0,10.0,12.0,15.0,8.0,10.0,20.0,20.0,25.0,
       30.0,25.0,40.0,35.0,40.0,30.0,40.0,40.0,50.0,40.0,50.0,
       60.0,50.0, 173, 293)
data0 = cbind(consumo = y, renda = x); View(data0)

##### Análise de correlação linear entre as variáveis ########
## Gráfico: Diagrama de Dispersão
 plot(x,y, xlab="Renda Familiar (U.M.)", ylab="Gasto com Alimentação (U.M.)",
     pch=16, las=1,
     main="Gasto com alimentação em função da renda\n familiar em U.M.")

################## Ajuste do MRLS
## Y = a + b*x
model <- lm(y ~ x); model
model_log = lm(log(y) ~log(x))#modelo log-linear
prediction = function(model, x){
  coef = as.numeric(model$coefficients)
  y_estimate = coef[1] + coef[2]*x
  return(y_estimate)
}
prediction(model, x = c(170, 180, 1000, 2000))
exp(prediction(model_log, x = log(c(170, 180, 1000, 2000))))
lines(x, model$fitted.values, col="blue")
lines(x, exp(model_log$fitted.values), col="red")

## Ex.2: Precipitação semanal de chuva
#s_t - estação do ano na semana t - Verão (0) e outono (1)
# x_t - precipitação observada na semana t (em milímetros  - mm)
s_t = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)#Estação do ano
x_t = c(2.9, 3.8, 10.5, 17, 16.7, 19, 23.7, 24.9, 24.2, 23.3)#precipitacao
data = cbind(s_t, x_t); View(data)
#Construa um modelo linear que permita estimar a precipitação
# na semana t (x_t) a partir da precipitação observada na
# semana anterior (x_{t-1}) e da estação do ano (s_t)
size = nrow(data)
#x_t = x_t[1:size]
#s_t = s_t[1:size]
x_t_1 = c(NA, x_t[1:(size-1)])

data2 = cbind(x_t, x_t_1, s_t); View(data2)
pairs(data2)
model2 = lm(x_t ~ x_t_1 + s_t); model2

## Análise detalhada para o ajuste do modelo
ajuste = summary(model); ajuste
ajuste_log = summary(model_log); ajuste_log
ajuste2 = summary(model2); ajuste2
#Obs: Tb envolve o teste (coluna 'Pr(>|t|)')
## H0: beta  = 0 ## não existe relação linear entre X e Y
## H1: beta != 0 ## existe relação linear entre X e Y

## Testes de Normalidade (importante para os testes de
# hipóteses realizados até aqui)

################# ANÁLISE RESIDUAL DO MRLS
###### Verificar se os resíduos:
# são normalmente distribuídos?
# têm variância constante (homoscedasticidade)?
# são independentes?
# existem Outliers influentes?

res = resid(model)  ## Os resíduos do modelo MRLS
# res = model$residuals

##### Normalidade dos resíduos
Norm_test <-function(x){
  require(nortest)
  t1 <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x)) # KS
  t2 <- lillie.test(x)                     # Lilliefors
  t3 <- shapiro.test(x)                    # Shapiro-Wilk
  # t4 <- ad.test(x)                         # Anderson-Darling
  testes <- c(t1$method, t2$method, t3$method)#, t4$method)
  valorp <- c(t1$p.value, t2$p.value, t3$p.value)#, t4$p.value)
  resultados <- cbind(valorp)
  rownames(resultados) <- testes
  print(resultados, digits = 4)
}
## Obs.: shapiro.test sugere-nos preferência em relação ao
##       ks.test para amostras de pequenas (n < 30)
Norm_test(res)
Norm_test(model2$residuals)

##### Verificar a homoscedasticidade
## O teste Breusch-Pagan
# H0: há a homoscedasticidade (a variância do erro
# permanece constante ao longo de valores de Y?)
# H1: não há homoscedasticidade
library(lmtest)
bptest(model)
## Como 0.06444 > 0.05, não rejeita-se H0
?bptest
bptest(model2)


## Análise gráfica: resíduos padronizados x valores ajustados (ou preditivos)
#  Este gráfico deve apresentar pontos dispostos aleatoriamente
#  sem nenhum padrão definido
z = rstandard(model)#retorna os resíduos padronizados ((x - mean)/sd)
py <- predict(model)
plot(py, z, ylim=c(-2.1,2.1), pch=16, ylab="Resíduos padronizados",
     xlab="Gastos com alimentação preditos",
     main="Gráficos para análise dos resíduos padronizados")
abline(h=0, lty=3, lwd=2)
abline(h=2, lty=2, col="red")
abline(h=-2, lty=2, col="red")

### Histograma dos resíduos padronizados
hist(z, nclass=5)

##### Verificar independência dos resíduos
## Para testar o pressuposto da independência dos resíduos
## pode-se utilizar o teste de Durbin-Watson (DW).
# H0: os resíduos são não correlacionados
# H1: os resíduos são correlacionados
dwtest(model)
dwtest(model2)

#Outliers
boxplot(model$residuals)
boxplot(model2$residuals)
##### Não parece existir Outliers
cook = cooks.distance(model2)
plot(cook, ylab="Distância de Cooks")
# identify(1:length(cook),cook)

## Medidas de Leverage
xx <- model.matrix(model)
lev <- hat(xx)
plot(lev, ylab="Medidas de leverage")
# identify(1:200,lev)

## Considera-se a distância de cook ou  Leverage elevado quando, ver Pestana e Gageiro, 2005
# COOK > 4/(n-(p+1))
#LEV > 3*(p+1)/n    ## para amostras de dimensão reduzida
#LEV > 2*(p+1)/n    ## para amostras grandes
p=2
n=length(model$residuals)
COOK = 4/(n-p-1); cook[cook > COOK]
LEV1 = 3*(p+1)/n; lev[lev > LEV1]
LEV2 = 2*(p+1)/n; lev[lev > LEV2]
model$residuals[c(22,25)]
## Gráfico: Resíduos padronizados x Leverage
# plot(lev, res1_p, xlab="Medidas de leverage", ylab="Resíduos padronizados")


## Obs.: Uma distância de Cook elevada significa que o resíduo ? elevado,
##       ou a Leverage para essa observação é elevada, ou ambas as situações.
         # Considera-se que observações com distância de Cook superior a 1
         # são excessivamente influentes.
plot(model)

## Gráfico de dispersão do MRLS
plot(x,y, xlab="Renda Familiar (U.M.)", ylab="Gasto com Alimentação (U.M.)",
     pch=16, las=1,
   main="Gasto com alimentação em função da renda\n familiar de uma amostra de 25 fam?lias.")
abline(model, col='blue', lwd=2)
mtext(expression(hat(y) == 5.4005+0.2558*x), line=-1.5)
mtext(expression(r^2 == 0.9102),line=-3)


## Tabela de Análise de Variância - ANOVA
ANOVA = anova(model); ANOVA

## Coeficiente de Determinação
R2 = ajuste$r.squared; R2

## Coeficiente de Determinação Ajustado
# n = length(x)    ## tamanho da amostra
# p = 1            ## número de variáveis explicativas.
# R_ajus = 1 - ((n-1)/(n -(p+1)))*(SQRes/SQT); R_ajus
# Ou simplesmente usando o software
ajuste$adj.r.squared


## Calculo do coeficiente de correlação
r = cor(x,y); r
#coincide com sign(beta)*sqrt(R2)

