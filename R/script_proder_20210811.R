BD <- read.delim("C:/Users/praf6/Downloads/Teste 02_TiposDeVariaveis_proder_respostas_Respostas_ao_formulario.tsv", encoding="UTF-8", comment.char="#", sep="\t")
View(BD)
floresta = BD$Assinale.o.tipo.correspondente.de.cada.variavel..Area.desmatada.de.dada.floresta.
names(table(floresta))
floresta_fac = factor(x = floresta
                      , levels = c("Quantitativa Discreta"
                                  , "Quantitativa Contínua")
                      , ordered = TRUE)
table(floresta_fac)
barplot(table(floresta_fac))

aprovados = BD$Assinale.o.tipo.correspondente.de.cada.variavel..Número.de.aprovados.em.uma.disciplina.
tb = table(floresta_fac, aprovados)
plot(tb)
prop.table(tb)
prop.table(tb, margin = 2)

