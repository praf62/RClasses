#Content available in page http://www.openreliability.org/faulttree-users-tutorial/start-faulttree-on-r/
#PACKAGE INSTALLATION AND LOADING
#install.packages("FaultTree", repos="http://R-Forge.R-project.org")
library(FaultTree)
# rm(tree_quedaAviao)
# rm(tree_nodesCaracteristics)
# rm(prob_quedaAviao)
#FAULT TREE DESIGN (QUALITATIVE ANALYSIS) FOR AIRPLANE FALL
tree_quedaAviao = ftree.make(type="and", name="Queda de Aviao")#Nó 01
tree_quedaAviao = addLogic(tree_quedaAviao, at=1, type="or", name="Erro Humano")#Nó 02 (at=1 implica que esse nó será filho do Nó 1)
tree_quedaAviao = addLogic(tree_quedaAviao, at=1, type="or", name="Falha Tecnologica")#Nó 03

tree_quedaAviao = addProbability(tree_quedaAviao, at=2, prob=.05, name="Fadiga", name2="no trabalho")
tree_quedaAviao = addProbability(tree_quedaAviao, at=2, prob=.1, name="Inexperiencia", name2="")
tree_quedaAviao = addProbability(tree_quedaAviao, at=2, prob=.15, name="Lapso", name2="de memoria")

tree_quedaAviao = addProbability(tree_quedaAviao, at=3, prob=.05, name="Sistema", name2="mecanico")
tree_quedaAviao = addProbability(tree_quedaAviao, at=3, prob=.09, name="Sistema", name2="eletrico")
tree_quedaAviao = addProbability(tree_quedaAviao, at=3, prob=.01, name="Sistema", name2="eletronico")
tree_quedaAviao = addProbability(tree_quedaAviao, at=3, prob=.005, name="Sistema", name2="software")
# View(tree_quedaAviao)

tree_nodesCaracteristics = ftree2html(tree_quedaAviao, write_file=TRUE)
browseURL("tree_quedaAviao.html")

#PROBABILITY OCCURRENCE CALCULATIONS
prob_quedaAviao = ftree.calc(tree_quedaAviao, use.bdd = TRUE)
View(prob_quedaAviao)
# ftree2html(prob_quedaAviao, write_file=TRUE)
browseURL("prob_quedaAviao.html")

#CUT SETS COMPUTATION
cs_quedaAviao<-cutsets(tree_quedaAviao)[[2]]
cs_quedaAviao
#cs_tags2<-apply(cs_quedaAviao[[2]], c(1,2),function(x) tree_quedaAviao$Tag[which(tree_quedaAviao$ID==x)])
nCs = nrow(cs_quedaAviao)
cutSets = NULL
library(doParallel)
for (i in 1:nCs) {#i=1
  cs_i_tags = cs_quedaAviao[i,]
  cs_i_length =length(cs_i_tags)
  cs_i_indexes =
    foreach(j = 1:cs_i_length, .combine = c) %do% {
      cs_i_tag_j = cs_i_tags[j]
      index_ij = which(tree_quedaAviao$Tag==cs_i_tag_j)
      return(index_ij)
    }
  cs_i = paste(paste("[", tree_quedaAviao$Name[cs_i_indexes], "_", tree_quedaAviao$Name2[cs_i_indexes], "]", sep=""), collapse ="^")
  cs_i = gsub(pattern = " ", replacement = "_", x = cs_i)
  cs_prob = prod(tree_quedaAviao$PBF[cs_i_indexes])#Os eventos do corte mínimo são supostos independentes entre si
  df_i  = cbind( CUT = cs_i, PROBABILITY = cs_prob)
  cutSets = rbind(cutSets, df_i)
}
View(cutSets)
