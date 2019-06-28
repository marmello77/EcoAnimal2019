############################################
#Universidade de Sao Paulo
#Instituto de Biociencias
#Topicos Avancados em Ecologia de Animais (BIE0315) - 2019
#Profs. Jose Carlos Motta Jr. & Marco Mello
#Monitores: Silara Batista, Bruno Ferreto & Julia Trevisan
#Pratica de Computador III
#Tema: Socialidade - 27/05/19
#Agradecimentos: Danilo Muniz nos ajudou a escrever o codigo de R usado para filtrar os dados originais
#Artigo fonte dos dados: https://doi.org/10.1007/s00265-018-2608-1  
#Obs: os dados originais do artigo foram filtrados para a especie Desmodus rotundus, considerando apenas a coocorrencia em abrigos
############################################


#Limpe o console
cat("\014")  

#Defina o diretorio de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Remova todos os objetos previos
rm(list= ls())

#Carregue os pacotes necessarios
library(ggplot2)
library(igraph)
library(reshape)
library(tidyverse)
library(dplyr)


#####################################
##### ROOSTS
#####################################


nodesR <- read.delim("nodes.roosts.txt")
linksR <- read.delim("links.roosts.txt")
head(nodesR)
head(linksR)
morcegosR = graph_from_data_frame(d=linksR, vertices=nodesR, directed=F)

class(morcegosR)
morcegosR
vertex_attr(morcegosR)
edge_attr(morcegosR)


#Subdividir por sexo
morcegosRF = induced.subgraph(morcegosR, (which(V(morcegosR)$Sex == "F")))
V(morcegosRF)$Sex

morcegosRM = induced.subgraph(morcegosR, (which(V(morcegosR)$Sex == "M")))
V(morcegosRM)$Sex


################## GRAFOS


##### AMBOS OS SEXOS
#Defina um layout para o grafo
lR <- layout_nicely(morcegosR)

#Defina uma curvatura que evite sobreposicao de links
curvesR = curve_multiple(morcegosR)

#Defina a espessura dos links para usar no grafo
E(morcegosR)$width = (E(morcegosR)$weight-min(E(morcegosR)$weight))/(max(E(morcegosR)$weight)-min(E(morcegosR)$weight))

#Defina as cores dos nós
V(morcegosR)$color = V(morcegosR)$Sex
V(morcegosR)$color = gsub("F","pink",V(morcegosR)$color)
V(morcegosR)$color = gsub("M","blue",V(morcegosR)$color)
V(morcegosR)$color

#Plote o grafo e exporte-o como uma imagem
png(filename= "morcegosR.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(morcegosR, 
     vertex.color = V(morcegosR)$color, 
     vertex.frame.color= V(morcegosR)$color, 
     vertex.size=8,
     vertex.label=V(morcegosR)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.color = adjustcolor("grey", alpha.f = .5), 
     edge.curved=curvesR,
     edge.width = 1,
     layout=lR)
legend(x = 0.7, y = 0.9, title="Sexo",
       legend = c("Fêmeas", "Machos"), pch = c(16,16),   
       text.col = "gray20", title.col = "black", 
       box.lwd = 0, cex = 2, col=c("pink", "blue"))
par(mfrow=c(1,1))
dev.off()


##### FEMEAS
#Defina um layout para o grafo
lF <- layout_nicely(morcegosRF)

#Defina uma curvatura que evite sobreposicao de links
curvesF = curve_multiple(morcegosRF)

#Defina a espessura dos links para usar no grafo
E(morcegosRF)$width = (E(morcegosRF)$weight-min(E(morcegosRF)$weight))/(max(E(morcegosRF)$weight)-min(E(morcegosRF)$weight))

#Defina as cores dos nós
V(morcegosRF)$color = V(morcegosRF)$Sex
V(morcegosRF)$color = gsub("F","pink",V(morcegosRF)$color)
V(morcegosRF)$color = gsub("M","blue",V(morcegosRF)$color)
V(morcegosRF)$color

#Plote o grafo
png(filename= "morcegosRf.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(morcegosRF, 
     vertex.color = V(morcegosRF)$color, 
     vertex.frame.color= V(morcegosRF)$color, 
     vertex.size=8,
     vertex.label=V(morcegosRF)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.color = adjustcolor("grey", alpha.f = .5), 
     edge.curved=curvesF,
     edge.width = 1,
     layout=lF)
par(mfrow=c(1,1))
dev.off()


##### MACHOS
#Defina um layout para o grafo
lM <- layout_nicely(morcegosRM)

#Defina uma curvatura que evite sobreposicao de links
curvesM = curve_multiple(morcegosRM)

#Defina a espessura dos links para usar no grafo
E(morcegosRM)$width = (E(morcegosRM)$weight-min(E(morcegosRM)$weight))/(max(E(morcegosRM)$weight)-min(E(morcegosRM)$weight))

#Defina as cores dos nós
V(morcegosRM)$color = V(morcegosRM)$Sex
V(morcegosRM)$color = gsub("F","pink",V(morcegosRM)$color)
V(morcegosRM)$color = gsub("M","blue",V(morcegosRM)$color)
V(morcegosRM)$color


#Plote o grafo
png(filename= "morcegosRm.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(morcegosRM, 
     vertex.color = V(morcegosRM)$color, 
     vertex.frame.color= V(morcegosRM)$color, 
     vertex.size=8,
     vertex.label=V(morcegosRM)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.color = adjustcolor("grey", alpha.f = .5), 
     edge.curved=curvesM,
     edge.width = 1,
     layout=lM)
par(mfrow=c(1,1))
dev.off()


################## MODULARIDADE


##### FEMEAS
modules1F = cluster_louvain(morcegosRF)
modules1F$modularity
V(morcegosRF)$color = modules1F$membership
edge.start <- ends(morcegosRF, es=E(morcegosRF), names=F)[,1]
edge.col <- V(morcegosRF)$color[edge.start]

png(filename= "morcegosRf-mod.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(modules1F, morcegosRF, 
     vertex.color = V(morcegosRF)$color, 
     vertex.frame.color= V(morcegosRF)$color, 
     vertex.size=10,
     vertex.label=V(morcegosRF)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.curved=curvesF,
     edge.width = 1,
     edge.color = edge.col,
     layout=lF)
par(mfrow=c(1,1))
dev.off()


##### MACHOS
modules1M = cluster_louvain(morcegosRM)
modules1M$modularity
V(morcegosRM)$color = modules1M$membership
edge.start <- ends(morcegosRM, es=E(morcegosRM), names=F)[,1]
edge.col <- V(morcegosRM)$color[edge.start]

png(filename= "morcegosRm-mod.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(modules1M, morcegosRM, 
     vertex.color = V(morcegosRM)$color, 
     vertex.frame.color= V(morcegosRM)$color, 
     vertex.size=10,
     vertex.label=V(morcegosRM)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.curved=curvesM,
     edge.width = 1,
     edge.color = edge.col,
     layout=lM)
par(mfrow=c(1,1))
dev.off()


################## CENTRALIDADE


##### FEMEAS
morcegosRF.grau = degree(morcegosRF)
max(morcegosRF.grau)
min(morcegosRF.grau)

V(morcegosRF)$size = morcegosRF.grau
V(morcegosRF)$size = (V(morcegosRF)$size-min(V(morcegosRF)$size))/(max(V(morcegosRF)$size)-min(V(morcegosRF)$size))

png(filename= "morcegosRF-cen.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(morcegosRF, 
     vertex.color = "pink", 
     vertex.frame.color= "pink", 
     vertex.size=V(morcegosRF)$size*10,
     vertex.label=V(morcegosRF)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.curved=curvesF,
     edge.width = 1,
     edge.color = "grey",
     layout=lF)
par(mfrow=c(1,1))
dev.off()


##### MACHOS
morcegosRM.grau = degree(morcegosRM)
max(morcegosRM.grau)
min(morcegosRM.grau)

V(morcegosRM)$size = morcegosRM.grau
V(morcegosRM)$size = (V(morcegosRM)$size-min(V(morcegosRM)$size))/(max(V(morcegosRM)$size)-min(V(morcegosRM)$size))

png(filename= "morcegosRM-cen.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(morcegosRM, 
     vertex.color = "blue", 
     vertex.frame.color= "blue", 
     vertex.size=V(morcegosRM)$size*10,
     vertex.label=V(morcegosRM)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.curved=curvesM,
     edge.width = 1,
     edge.color = "grey",
     layout=lM)
par(mfrow=c(1,1))
dev.off()



#####################################
##### GROUPS
#####################################



nodesG <- read.delim("nodes.groups.txt")
linksG <- read.delim("links.groups.txt")
head(nodesG)
head(linksG)
morcegosG = graph_from_data_frame(d=linksG, vertices=nodesG, directed=F)

class(morcegosG)
morcegosG
vertex_attr(morcegosG)
edge_attr(morcegosG)


#Subdividir por sexo
morcegosGF = induced.subgraph(morcegosG, (which(V(morcegosG)$Sex == "F")))
V(morcegosGF)$Sex

morcegosGM = induced.subgraph(morcegosG, (which(V(morcegosG)$Sex == "M")))
V(morcegosGM)$Sex


################## GRAFOS


##### AMBOS OS SEXOS
#Defina um layout para o grafo
lG <- layout_nicely(morcegosG)

#Defina uma curvatura que evite sobreposicao de links
curvesG = curve_multiple(morcegosG)

#Defina a espessura dos links para usar no grafo
E(morcegosG)$width = (E(morcegosG)$weight-min(E(morcegosG)$weight))/(max(E(morcegosG)$weight)-min(E(morcegosG)$weight))
#E(morcegosG)$width = E(morcegosG)$weight

#Defina as cores dos nós
V(morcegosG)$color = V(morcegosG)$Sex
V(morcegosG)$color = gsub("F","pink",V(morcegosG)$color)
V(morcegosG)$color = gsub("M","blue",V(morcegosG)$color)
V(morcegosG)$color


#Plote o grafo
png(filename= "morcegosG.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(morcegosG, 
     vertex.color = V(morcegosG)$color, 
     vertex.frame.color= V(morcegosG)$color, 
     vertex.size=8,
     vertex.label=V(morcegosG)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.color = adjustcolor("grey", alpha.f = .5), 
     edge.curved=curvesG,
     edge.width = 1,
     layout=lG)
legend(x = 0.7, y = 0.9, title="Sexo",
       legend = c("Fêmeas", "Machos"), pch = c(16,16),   
       text.col = "gray20", title.col = "black", 
       box.lwd = 0, cex = 2, col=c("pink", "blue"))
par(mfrow=c(1,1))
dev.off()


##### FEMEAS
#Defina um layout para o grafo
lF2 <- layout_nicely(morcegosGF)

#Defina uma curvatura que evite sobreposicao de links
curvesF2 = curve_multiple(morcegosGF)

#Defina a espessura dos links para usar no grafo
E(morcegosGF)$width = (E(morcegosGF)$weight-min(E(morcegosGF)$weight))/(max(E(morcegosGF)$weight)-min(E(morcegosGF)$weight))

#Defina as cores dos nós
V(morcegosGF)$color = V(morcegosGF)$Sex
V(morcegosGF)$color = gsub("F","pink",V(morcegosGF)$color)
V(morcegosGF)$color = gsub("M","blue",V(morcegosGF)$color)
V(morcegosGF)$color


#Plote o grafo
png(filename= "morcegosGf.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(morcegosGF, 
     vertex.color = V(morcegosGF)$color, 
     vertex.frame.color= V(morcegosGF)$color, 
     vertex.size=8,
     vertex.label=V(morcegosGF)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.color = adjustcolor("grey", alpha.f = .5), 
     edge.curved=curvesF2,
     edge.width = 1,
     layout=lF2)
par(mfrow=c(1,1))
dev.off()


##### MACHOS
#Defina um layout para o grafo
lM2 <- layout_nicely(morcegosGM)

#Defina uma curvatura que evite sobreposicao de links
curvesM2 = curve_multiple(morcegosGM)

#Defina a espessura dos links para usar no grafo
E(morcegosGM)$width = (E(morcegosGM)$weight-min(E(morcegosGM)$weight))/(max(E(morcegosGM)$weight)-min(E(morcegosGM)$weight))

#Defina as cores dos nós
V(morcegosGM)$color = V(morcegosGM)$Sex
V(morcegosGM)$color = gsub("F","pink",V(morcegosGM)$color)
V(morcegosGM)$color = gsub("M","blue",V(morcegosGM)$color)
V(morcegosGM)$color


#Plote o grafo
png(filename= "morcegosGm.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(morcegosGM, 
     vertex.color = V(morcegosGM)$color, 
     vertex.frame.color= V(morcegosGM)$color, 
     vertex.size=8,
     vertex.label=V(morcegosGM)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.color = adjustcolor("grey", alpha.f = .5), 
     edge.curved=curvesM2,
     edge.width = 1,
     layout=lM2)
par(mfrow=c(1,1))
dev.off()


################## MODULARIDADE


##### FEMEAS
modules1F2 = cluster_louvain(morcegosGF)
modules1F2$modularity
V(morcegosGF)$color = modules1F2$membership
edge.start <- ends(morcegosGF, es=E(morcegosGF), names=F)[,1]
edge.col <- V(morcegosGF)$color[edge.start]

png(filename= "morcegosGf-mod.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(modules1, morcegosGF, 
     vertex.color = V(morcegosGF)$color, 
     vertex.frame.color= V(morcegosGF)$color, 
     vertex.size=10,
     vertex.label=V(morcegosGF)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.curved=curvesF2,
     edge.width = 1,
     edge.color = edge.col,
     layout=lF2)
par(mfrow=c(1,1))
dev.off()


##### MACHOS
modules1M2 = cluster_louvain(morcegosGM)
modules1M2$modularity
V(morcegosGM)$color = modules1M2$membership
edge.start <- ends(morcegosGM, es=E(morcegosGM), names=F)[,1]
edge.col <- V(morcegosGM)$color[edge.start]

png(filename= "morcegosGm-mod.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(modules1M2, morcegosGM, 
     vertex.color = V(morcegosGM)$color, 
     vertex.frame.color= V(morcegosGM)$color, 
     vertex.size=10,
     vertex.label=V(morcegosGM)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.curved=curvesM2,
     edge.width = 1,
     edge.color = edge.col,
     layout=lM2)
par(mfrow=c(1,1))
dev.off()


################## CENTRALIDADE


##### FEMEAS
morcegosGF.grau = degree(morcegosGF)
max(morcegosGF.grau)
min(morcegosGF.grau)

V(morcegosGF)$size = morcegosGF.grau
V(morcegosGF)$size = (V(morcegosGF)$size-min(V(morcegosGF)$size))/(max(V(morcegosGF)$size)-min(V(morcegosGF)$size))

png(filename= "morcegosGf-cen.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(morcegosGF, 
     vertex.color = "pink", 
     vertex.frame.color= "pink", 
     vertex.size=V(morcegosGF)$size*10,
     vertex.label=V(morcegosGF)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.curved=curvesF2,
     edge.width = 1,
     edge.color = "grey",
     layout=lF2)
par(mfrow=c(1,1))
dev.off()


##### MACHOS
morcegosGM.grau = degree(morcegosGM)
max(morcegosGM.grau)
min(morcegosGM.grau)

V(morcegosGM)$size = morcegosGM.grau
V(morcegosGM)$size = (V(morcegosGM)$size-min(V(morcegosGM)$size))/(max(V(morcegosGM)$size)-min(V(morcegosGM)$size))

png(filename= "morcegosGm-cen.png", res= 300, height= 3000, width= 3500)
par(mfrow=c(1,1),mar=c(1,1,3,1))
plot(morcegosGM, 
     vertex.color = "blue", 
     vertex.frame.color= "blue", 
     vertex.size=V(morcegosGM)$size*10,
     vertex.label=V(morcegosGM)$name,
     vertex.label.color="white",
     vertex.label.cex=.5,
     edge.curved=curvesM2,
     edge.width = 1,
     edge.color = "grey",
     layout=lM2)
par(mfrow=c(1,1))
dev.off()
