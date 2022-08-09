dados = data.frame(
  target = c(42.13, 41.31, 54.30
                      , 40.00, 48.08, 54.97
                      , 45.93, 25.69, 34.51
                      , 21.16)#Tempo de cirurgia
  , group = c(rep(c("P1", "P2", "P3"), 3), "P1"))#Médico
View(dados)

boxplot(target ~  group, data = dados
        , xlab = "Procedimento", ylab="Tempo de cirurgia")
#Análise de Variâncias - ANOVA
model = aov(target ~ group, data = dados)
summary(model)
#Esta ANOVA se baseia em 2 grupos de suposições:
# As variâncias são equivalentes ao longo dos "groups"
# As observações das amostras de cada group vêm de uma normal
#Teste de Igualdade/Homogeneidade de Variâncias:
#H0: as variâncias são iguais
#H1: Ao menos duas variâncias diferem entre si
# install.packages("car")
library(car)
lt = leveneTest(dados$target, dados$group)
lt$`Pr(>F)`

#Teste de normalidade de cada group:
#H0: As observações das amostras de cada group vêm de uma normal
#H1: As observações das amostras de ao menos um group não vêm de uma normal
groupNormality.test = function(  quantitativeVariable
                               , qualitativeVariable){
  library(doParallel); #library(nortest)
  groupsNames = names(table(qualitativeVariable))
  ngroups = length(groupsNames)
  p.values = foreach(i = 1:ngroups)%do%{#i=1
    g_i = groupsNames[i]
    sample_i = quantitativeVariable[qualitativeVariable == g_i]
    # lt = lillie.test(x = sample_i)
    # kst = ks.test(x = sample_i, "pnorm"
    #               , mean = mean(sample_i)
    #               , sd = sd(sample_i))
    # return(kst$`Pr(>F)`)
    # return(kst$p.value)
    sh = shapiro.test(x = sample_i)
    return(sh$p.value)
  }
  names(p.values) = paste("p.value", groupsNames, sep="_")
  ret = as.data.frame(p.values)
  return(ret)
}
gnt = groupNormality.test(quantitativeVariable = dados$target
                          , qualitativeVariable = dados$group)
gnt
#Se ambas as hipóteses (sobre as variâncias e normalidade)
# não forem satisfeitas, recorrer a outro teste,
# como o Kruskall-Wallis, é uma saída:
kwt = kruskal.test(target ~ group, data = dados)
kwt$p.value

#Identificando aonde haveriam diferenças
tm = TukeyHSD(model)
plot(tm)

