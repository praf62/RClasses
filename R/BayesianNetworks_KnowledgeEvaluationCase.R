#Content available in page http://www.bnlearn.com/
#Minha dissertação: https://repositorio.ufpe.br/bitstream/123456789/5864/1/arquivo7409_1.pdf
#PACKAGE INSTALLATION AND LOADING
install.packages("BiocManager")
BiocManager::install()
BiocManager::install(c("biocLite"))
BiocManager::install(c("graph", "Rgraphviz", "RBGL"))
install.packages("gRain")
install.packages("bnlearn")
library(bnlearn)

#DESIGNING AND PLOTING THE BAYESIAN BELIEF NETWORK (BBN)
#PRIORS
cpt_hasKnowledgment = matrix(c(0.5, 0.5), ncol = 2, byrow = TRUE,
                             dimnames = list(c("Probability"), c("YES", "NO")))
View(cpt_hasKnowledgment)
#LIKELIHOODS
cpt_correctAnswer_Q01 = matrix(c(.9, .25,
                                .1, .75), ncol = 2, byrow = TRUE,
                               dimnames = list(correctAnswer_Q01 = c("YES", "NO"),
                                               hasKnowledgment = c("YES", "NO")))
cpt_correctAnswer_Q01

#THE BNN
net = model2network("[hasKnowledgment][correctAnswer_Q01|hasKnowledgment]")
dfit = custom.fit(net, dist = list(hasKnowledgment = cpt_hasKnowledgment,
                                   correctAnswer_Q01 = cpt_correctAnswer_Q01))

root.nodes(dfit)#Os nós-raíz
leaf.nodes(dfit)#Os nós folha
modelstring(dfit)#O modelo textual

#BBN (for qualitative analysis)
graphviz.plot(dfit, layout = "dot", #LAYOUT=dot, neato, twopi, circo, and fdp
              shape = "rectangle", #circle, ellipse or rectangle
                main = "Rede Bayesiana")

bn.fit.dotplot(fitted = dfit$hasKnowledgment
               , xlab = "Probability", ylab = "hasKnowledgment"
               , main = "hasKnowledgment prior distribution")#plot the prior
bn.fit.barchart(dfit$correctAnswer_Q01
               , xlab = "hasKnowledgment", ylab = "correctAnswer_Q01"
               , main = "P(correctAnswer_Q01|hasKnowledgment)")#plot the likelihood

#COMPUTING THE POSTERIOR/MARGINAL DISTRIBUTIONS
cpquery(fitted = dfit,
        event = (hasKnowledgment=="YES"),
        evidence = (correctAnswer_Q01=="YES"),
        method = "ls"#"lw" leads to an error
        , n = 10000000)#the sample size of the method
#Similar to cpquery
# my_cpdist = cpdist(fitted = dfit
#                    , nodes = "hasKnowledgment"
#                    , evidence = (correctAnswer_Q01 == "YES")
#                    , method = "ls"
#                    , n = 1000)#"lw" leads to an error
# prop.table(table(my_cpdist))


#INVOLVING A SECOND QUESTION, MORE DIFFICULT TO ANSWER...
#LIKELIHOODS
cpt_correctAnswer_Q02 = matrix(c(.9, .25,#mesmo com conhecimento, erra-se mais
                                 .1, .75)
                               , ncol = 2, byrow = TRUE,
                               dimnames = list(correctAnswer_Q02 = c("YES", "NO"),
                                               hasKnowledgment = c("YES", "NO")))
# cpt_correctAnswer_Q02

#THE BNN
net2 =
  model2network("[hasKnowledgment][correctAnswer_Q01|hasKnowledgment][correctAnswer_Q02|hasKnowledgment]")
dfit2 = custom.fit(net2, dist = list(hasKnowledgment = cpt_hasKnowledgment
                                     , correctAnswer_Q01 = cpt_correctAnswer_Q01
                                     , correctAnswer_Q02 = cpt_correctAnswer_Q02))
#BBN (for qualitative analysis)
graphviz.plot(dfit2, layout = "dot", #LAYOUT=dot, neato, twopi, circo, and fdp
              shape = "rectangle", #circle, ellipse or rectangle
              main = "Rede Bayesiana #2")#supõe-se a priori que as questões são independentes

bn.fit.dotplot(fitted = dfit2$hasKnowledgment
               , xlab = "Probability", ylab = "hasKnowledgment"
               , main = "hasKnowledgment prior distribution")#plot the prior
bn.fit.barchart(dfit2$correctAnswer_Q01
                , xlab = "hasKnowledgment", ylab = "correctAnswer_Q01"
                , main = "P(correctAnswer_Q01|hasKnowledgment)")#plot the likelihood
bn.fit.barchart(dfit2$correctAnswer_Q02
                , xlab = "hasKnowledgment", ylab = "correctAnswer_Q02"
                , main = "P(correctAnswer_Q02|hasKnowledgment)")#plot the likelihood

#COMPUTING THE POSTERIOR/MARGINAL DISTRIBUTIONS
cpquery(fitted = dfit2,
        event = (hasKnowledgment=="YES"),
        evidence = ((correctAnswer_Q01=="YES") & (correctAnswer_Q02=="YES")),
        method = "ls"#"lw" leads to an error
        , n = 100000)#the sample size of the method
