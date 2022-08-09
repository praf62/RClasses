#Script -Leoncio/Yago
data_str = c("31/12/1996", "30/01/1997", "27/02/1997")
precipit = c(50, 60, 90)
dados = as.data.frame(cbind(data = as.character(data_str), precipitacao=precipit))
View(dados)
data_sep = strsplit (dados$data, split = "/" )
data_sep = t(as.data.frame(data_sep))
colnames(data_sep) = c("dia", "mes", "ano")
data_sep = as.data.frame(data_sep)
View(data_sep)
data_sep$precipitacao = precipit
boxplot(precipit ~ mes, data= data_sep)
