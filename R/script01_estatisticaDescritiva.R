# ler o conjunto de dados
BD01 = read.csv(file = "G:/Meu Drive/UFCA/Ensino/CRAN R_aulas/RClasses/data/BD01.csv", dec = ".", encoding="UTF-8", sep=";", quote="")
#para visualizar o conjunto
View(BD01)
#funções básicas (distribuições de frequência)
df = table(BD01$Fornecedor)
prop.table(df)#dist de frequências relativas
barplot(df, col = "red", xlab = "Fornecedor", ylab="Frequencia absoluta")#gráfico de barras
df2 = table(BD01$TempoFalha)#não gera informação
cbind(df2)

#Construindo nova variável, que respeite a ordenação do nível de estresse
table(BD01$Degradacao)
BD01$Degradacao = factor(BD01$Degradacao, order = TRUE,
                                    levels = c("Baixo", "Medio", "Alto"))
tbDegrad = table(BD01$Degradacao)
cbind(freAbs = tbDegrad, freqRel = prop.table(tbDegrad))
#View(BD01)
# DESNECESSÁRIO:
# Degradacao_ordinal = BD01$Degradacao#
# Degradacao_ordinal[Degradacao_ordinal=="Baixo"] = "1.Baixo"
# Degradacao_ordinal[Degradacao_ordinal=="Medio"] = "2.Medio"
# Degradacao_ordinal[Degradacao_ordinal=="Alto"] = "3.Alto"
# BD01$Degradacao_ordinal = Degradacao_ordinal
tb_forn_degrad = table(BD01$Fornecedor, BD01$Degradacao)

#para carregar funções criadas por mim
# debugSource
source('./R/distribFreq.R', encoding = 'UTF-8')
source('./R/classicBoxPlot.R', encoding = 'UTF-8')
source('./R/mode_function.R', encoding = 'UTF-8')
source('./R/iqv_function.R', encoding = 'UTF-8')

# distribuição de frequências dos dados
distFreq(x = BD01$Fornecedor)
df_TempoFalha = distFreq(x = BD01$TempoFalha)
q_05_tempoFalha = quantile(x = BD01$TempoFalha, probs = c(.05))

tbNReinc = table(BD01$nReincidenciaFalhas)
cbind(tbNReinc)
plot(tbNReinc)
hist(BD01$TempoFalha)

classicBoxPlot(x=BD01$TempoFalha, ylab = "Tempo até a falha")
# install.packages("qcc")
# library(qcc)
# pareto.chart(table(BD01$Degradacao))

#desenhando a distribuição dos dados
plot(tb_forn_degrad)#duas variáveis qualitativas
plot(x = BD01$nReincidenciaFalhas, y = BD01$TempoFalha)#duas variáveis quantitativas
boxplot(BD01$TempoFalha~BD01$Fornecedor)#uma qualitativa e uma quantitativa

#medidas de posição
ourMode(sample = BD01$Fornecedor, xlab = "Fornecedor", toPrint = TRUE)
ourMode(sample = BD01$TempoFalha, xlab = "Tempo até a falha", toPrint = TRUE, minimalAmplitudeRatioForGrouping = .2)
ourMode(sample = BD01$nReincidenciaFalhas, xlab = "Nº de reincidências de falhas", toPrint = TRUE, minimalAmplitudeRatioForGrouping = .2)
median(BD01$TempoFalha)
mean(BD01$TempoFalha)

#medidas de dispersão
df_fornecedor  = table(BD01$Fornecedor)

iqv(freq = df_fornecedor, varName = "Fornecedor")
#simulando um case com pouca variabilidade
df_fornecedor[1] = 0
iqv(freq = df_fornecedor, varName = "Fornecedor")
#simulando um case com muita variabilidade
df_fornecedor[1] = 226
iqv(freq = df_fornecedor, varName = "Fornecedor")

Amplitude = max(BD01$TempoFalha, na.rm=TRUE) - min(BD01$TempoFalha, na.rm=TRUE)
q = quantile(x = BD01$TempoFalha, probs = c(.25, .75), na.rm = TRUE)
AIQ = as.numeric(q[2] - q[1])
print(AIQ)

variancia = var(BD01$TempoFalha, na.rm=TRUE)
dp = sd(BD01$TempoFalha, na.rm=TRUE)#sqrt(variancia)
media = mean(x =  BD01$TempoFalha, na.rm = TRUE)
cv = dp/media

#erro absoluto médio
residuos = BD01$TempoFalha - media
eam = mean(abs(residuos))


##trash
# trash_data = (c(rep(10, 6), rep(9, 11), rep(8, 2), rep(7, 1)));
# trash_data = (c(rep("Sim", 13), rep("Não", 3), rep("Talvez", 4)));
# trash_tb = table(trash_data)
# iqv(freq = trash_tb, varName = "trash")
# mean(trash_data)

