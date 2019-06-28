############################################
#Universidade de Sao Paulo
#Instituto de Biociencias
#Topicos Avancados em Ecologia de Animais (BIE0315) - 2019
#Profs. Jose Carlos Motta Jr. & Marco Mello
#Monitores: Silara Batista, Bruno Ferreto & Julia Trevisan
#Pratica de Computador II
#Tema: Reproducao - 20/05/19
#Agradecimentos: Alexandre Palaoro, primeiro autor do artigo, cedou-nos os dados
#Artigo fonte dos dados: http://dx.doi.org/10.1016/j.anbehav.2014.06.014 
############################################


#Defina o diretorio de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Remova todos os objetos previos
rm(list= ls())

#Limpe o console
cat("\014")  

#Carregue os pacotes necessarios
library(ggplot2)
library(lme4)

#Importe os dados e inspecione-os
dados<- read.delim("dados.txt", header=T)
head(dados)
nrow(dados)
ncol(dados)

#Transforme a variavel "status" em numerica e binaria, salvando-a como uma nova variavel
dados$status2 <- ifelse(dados$status == "perdedor", 0, 1)
head(dados)

#Examine de forma rapida as relacoes entre as variaveis
plot(dados$cc~dados$ap)
plot(dados$status2~dados$cc)
plot(dados$status2~dados$ap)


##### TESTE 1 #####


#Plote a relacao entre o status e o comprimento cefalotoracico, depois exporte o grafico como um arquivo PNG
png(filename= "p1.png", res= 300, height= 3000, width= 3000)
p1 = ggplot(dados, aes(x=cc, y=status2)) + 
  geom_point(colour = "#1855FA", size=4, alpha = 0.5) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  labs(x="Comprimento cefalotoracico", y = "Status") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=40, hjust=0.5),
        axis.text.x = element_text(size = 20, angle=0, hjust=1),
        axis.text.y = element_text(size = 20, angle=0, vjust=1),
        axis.title.x = element_text(size = 30, angle=0),
        axis.title.y = element_text(size = 30, angle=90))
p1
dev.off()

#Teste se a relacao eh significativa e examine os valores estatistivos, depois exporte os resultados como um arquivo TXT
fit1 = glm(dados$status2~dados$cc, family=binomial)
summary(fit1)
res1 = anova(fit1, test="Chisq")
res1
capture.output(res1, file = "resultados-cc.txt")

#Voce tambem pode plotar o gráfico usando apenas o pacote base, pegando os resultados do teste que acabou de fazer
plot(dados$status2~dados$cc,
     xlab = "Comprimento do corpo",
     ylab = "Status")
curve (exp(fit1$coefficients[[1]]+fit1$coefficients[[2]]*x)/(1+exp(fit1$coefficients[[1]]+fit1$coefficients[[2]]*x)), add=T)


##### TESTE 2 #####


#Plote a relacao entre o status e a altura do propodo, depois exporte o grafico como um arquivo PNG
png(filename= "p2.png", res= 300, height= 3000, width= 3000)
p2 = ggplot(dados, aes(x=ap, y=status2)) + 
  geom_point(colour = "#1855FA", size=4, alpha = 0.5) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  labs(x="Altura da garra", y = "Status") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=40, hjust=0.5),
        axis.text.x = element_text(size = 20, angle=0, hjust=1),
        axis.text.y = element_text(size = 20, angle=0, vjust=1),
        axis.title.x = element_text(size = 30, angle=0),
        axis.title.y = element_text(size = 30, angle=90))
p2
dev.off()

#Teste se a relacao eh significativa e examine os valores estatistivos, depois exporte os resultados como um arquivo TXT
fit2 = glm(dados$status2~dados$ap, family=binomial)
summary(fit2)
res2 = anova(fit2, test="Chisq")
res2
capture.output(res2, file = "resultados-ap.txt")


##### TESTE 3 #####


#Teste como tanto o comprimento cefalotoracio quanto a altura do propodo determinam o status, depois exporte os resultados como um arquivo TXT
fit3 = glm(dados$status2~dados$ap+dados$cc, family=binomial)
summary(fit3)
res3 = anova(fit3, test="Chisq")
res3
capture.output(res3, file = "resultados-ap-cc.txt")


##### TESTE 4 #####


#Teste como tanto o comprimento cefalotoracio quanto a altura do propodo determinam o status, mas considerando a identidade da dupla como um fator aleatório, depois exporte os resultados como um arquivo TXT
fit4 = glmer(status2 ~ ap + cc + (1|dupla), family=binomial, data=dados)
summary(fit4)
res3 = anova(fit4, test="Chisq")
res3
capture.output(res3, file = "resultados-ap-cc-dupla.txt")
isSingular(fit4, tol = 1e-05)


##### TESTE 5 #####


#Teste a relacao entre os fatores usados nas analises anteriores, salvando os residuos dessa relacao para fazer uma nova analise
fit5 = lm(cc ~ ap, data=dados) 
summary(fit5)
fit5.res = resid(fit5)

#Plote a relacao entre os fatores, depois exporte o grafico como um arquivo PNG
png(filename= "p5.png", res= 300, height= 3000, width= 3000)
p5 = ggplot(dados, aes(x=cc, y=ap), CI = F) +
  geom_smooth(method=lm, colour = "#1855FA") +
  geom_point(colour = "#1855FA", size=4, alpha = 0.5) +
  geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.1, 
              fill = "#1855FA") +
  ggtitle("") +
  labs(x="Comprimento do corpo", y = "Altura da garra") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=40, hjust=0.5),
        axis.text.x = element_text(size = 20, angle=0, hjust=1),
        axis.text.y = element_text(size = 20, angle=0, vjust=1),
        axis.title.x = element_text(size = 30, angle=0),
        axis.title.y = element_text(size = 30, angle=90))
p5
dev.off()

#Plote a relacao entre status e os residuos da relacao entre comprimento cefalotoracico e altura do propodo, depois exporte o grafico como um arquivo PNG
p6 = png(filename= "p6.png", res= 300, height= 3000, width= 3000)
ggplot(dados, aes(x=fit5.res, y=status2)) + 
  geom_point(colour = "#1855FA", size=4, alpha = 0.5) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  labs(x="Residuos corpo-garra", y = "Status") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=40, hjust=0.5),
        axis.text.x = element_text(size = 20, angle=0, hjust=1),
        axis.text.y = element_text(size = 20, angle=0, vjust=1),
        axis.title.x = element_text(size = 30, angle=0),
        axis.title.y = element_text(size = 30, angle=90))
p6
dev.off()

#Teste se a relacao eh significativa e examine os valores estatisticos, depois exporte os resultados como um arquivo TXT
fit6 = glmer(status2 ~ fit5.res + (1|dupla), family=binomial, data=dados)
summary(fit6)
res3 = anova(fit6, test="Chisq")
res3
capture.output(res3, file = "resultados-res-dupla.txt")
isSingular(fit6, tol = 1e-05)