#Este é um exemplo de como ler e analisar um conjunto de dados
BD01 =
  read.csv(file =
             "G:/Meu Drive/UFCA/Ensino/CRAN R_aulas/RClasses/data/BD01.csv"
           , encoding="UTF-8"
           , sep=";"
           , comment.char="#")
View(BD01)
