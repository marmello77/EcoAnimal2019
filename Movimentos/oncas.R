############################################
#Universidade de Sao Paulo
#Instituto de Biociencias
#Topicos Avancados em Ecologia de Animais (BIE0315) - 2019
#Profs. Jose Carlos Motta Jr. & Marco Mello
#Monitores: Silara Batista, Bruno Ferreto & Julia Trevisan
#Pratica de Computador IV
#Tema: Socialidade - 03/06/19
#Agradecimentos: Milton Ribeiro nos indicou o data paper usado na pratica
#Artigo fonte dos dados: https://doi.org/10.1002/ecy.2379  
############################################

#Limpe o console
cat("\014")  

#Remova todos os objetos previos
rm(list= ls())

#Defina o diretorio de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Instale um pacote com mapas brasileiros
#install.packages("brazilmaps", dependencies = T)

#Instale estes pacotes direto dos GitHubs dos autores
#devtools::install_github("slowkow/ggrepel")
#devtools::install_github("oswaldosantos/ggsn")

#Carregue os pacotes necessarios
library(ggplot2)
library(ggmap)
library(ggsn)
library(maps)
library(mapdata)
library(ggrepel)
library(brazilmaps)


#######################


#Importe o arquivo com os pontos do GPS em graus decimais e transforme-o em um objeto de R. Aqui neste exemplo, selecionei apenas os dados da Fazenda Rio Negro, com 4 oncas individuais.
pontos = read.delim("pontos.txt", na.strings = "NA")

#De uma olhada no objeto que acabou de criar para conferir se esta como voce esperava
head(pontos)

#Troque os nomes das colunas para simplificar o codigo e confir-os
colnames(pontos) = c("event", "timestamp", "long", "lat", "taxon", "tag", "individual", "study", "country")
head(pontos)

#Transforme os individuos em fatores
pontos$individual = as.factor(pontos$individual)
class(pontos$individual)

#Exporte o objeto com os pontos em formato CSV para usar no Google My Maps (https://www.google.com/maps/d/)
write.csv(pontos, "pontos.csv", row.names=F)

#Teste algumas regioes para ver qual eh a que corresponde aos seus dados. Por exemplo:
map('world', region = "Brazil", fill = F)

#Defina qual sera a area usada como base do mapa
area <-map_data("world", region="Brazil", zoom=5) 

#De uma olhada no objeto que acabou de criar para conferir se esta como voce esperava
head(area)

#Confira a regiao exata onde caem os pontos registrados
min(pontos$long)
max(pontos$long)
min(pontos$lat)
max(pontos$lat)

#Crie objetos adicionais para restringir a area do mapa a regiao dos pontos
longs<-c(min(pontos$long)-0.01, max(pontos$long)+0.01)
lats<-c(min(pontos$lat)-0.01, max(pontos$lat)+0.01)


#######################


#Plote os pontos em um mapa simples
plot(pontos$long~pontos$lat)

#Agora plote os pontos em um mapa mais elaborado
g1 <- ggplot() + geom_polygon(data = area,
                             aes(x=long, y = lat, group = group),
                             fill = "lightgrey", color = "lightgrey") +
  xlim(longs) +
  ylim(lats) +
  coord_fixed(1.1) + 
  geom_polygon(data = area, 
               aes(x = long, y = lat, group = group), 
               color = "white", fill = NA, size = 0.04) +
  geom_point(data = pontos, aes(x = long, y = lat), 
             color = "red", 
             size = 2, 
             alpha = 0.6) +
  ggtitle("Localizações das onças") + 
  labs(x="Longitude", y = "Latitude") + 
  theme(text = element_text(size=14), #Ajuste os tamanhos das fontes 
        plot.title = element_text(size=20, hjust=0.5),
        axis.text.x = element_text(size = 10, angle=0, hjust=1),
        axis.text.y = element_text(size = 10, angle=0, vjust=1),
        axis.title.x = element_text(size = 12, angle=0),
        axis.title.y = element_text(size = 12, angle=90))

png(filename= "oncas.png", res= 300, height= 1500, width= 3000)
plot(g1)
dev.off()


#######################


#Plote novamente o mapa, mas identificando os individuos por cores
g2 <- ggplot() + geom_polygon(data = area,
                             aes(x=long, y = lat, group = group),
                             fill = "lightgrey", color = "lightgrey") +
  xlim(longs) +
  ylim(lats) +
  coord_fixed(1.1) + 
  geom_polygon(data = area, 
               aes(x = long, y = lat, group = group), 
               color = "white", fill = NA, size = 0.04) +
  geom_point(data = pontos, aes(x = long, y = lat, color = individual), 
             size = 2, 
             alpha = 0.6) +
  ggtitle("Localizações das onças") + 
  labs(x="Longitude", y = "Latitude") + 
  theme(text = element_text(size=14), #Ajuste os tamanhos das fontes 
        plot.title = element_text(size=20, hjust=0.5),
        axis.text.x = element_text(size = 10, angle=0, hjust=1),
        axis.text.y = element_text(size = 10, angle=0, vjust=1),
        axis.title.x = element_text(size = 12, angle=0),
        axis.title.y = element_text(size = 12, angle=90))

png(filename= "oncas-ind.png", res= 300, height= 1500, width= 3000)
plot(g2)
dev.off()


