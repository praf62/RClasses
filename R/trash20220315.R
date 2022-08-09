library(readr)#para acessar a biblioteca readr
BD01 <- read_delim("data/BD01.csv", delim = ";",
                     escape_double = FALSE, trim_ws = TRUE)#para ler o conjunto BD01
View(BD01)#para visualizar o conjunto de dados
BD01$Fornecedor
distFreqAbs = table(BD01$Fornecedor) #para guardar a distribuição
prop.table(distFreqAbs)#para distribuição de rfequencia relativa
mean(BD01$Fornecedor)#para calcular a média
