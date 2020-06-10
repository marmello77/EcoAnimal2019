############################################
#Universidade de Sao Paulo
#Instituto de Biociencias
#Topicos Avancados em Ecologia de Animais (BIE0315) - 2019
#Profs. Jose Carlos Motta Jr. & Marco Mello
#Monitores: Silara Batista, Bruno Ferreto & Julia Trevisan
#Pratica de Computador V
#Tema: Forrageio - 10/06/19
#Artigo fonte dos dados: https://doi.org/10.1371/journal.pone.0168062 
############################################

#Limpe o console.
cat("\014")  

#Remova todos os objetos previos.
rm(list= ls())

#Defina o diretorio de trabalho como sendo a pasta onde salvou este script.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Carregue os pacotes necessarios.
library(ggplot2)
library(lme4)
library(glm2)
library(MASS)
library(bbmle)

#DICA: Notou que as linhas dos comandos e comentarios geralmente são mais longas do que a tela de computadores pequenos? Se quiser resolver esse problema, fazendo com que as linhas sejam quebradas automaticamente para facilitar visualizacao, use este comando, clicando em janelas la na barra superior do RStudio:
# Tools>Global Options>Code>marque a opcao "Soft-wrap R source files"


#######################


#Importe o arquivo com os dados. Confira o nome que deu ao arquivo que extraiu da planilha de dados.
dados = read.delim("dados.txt", na.strings = "NA")

#De uma olhada no objeto que acabou de criar para conferir se ele esta como voce esperava.
head(dados)

#Deixe os dados ja carregados na memoria. Isso facilita a sua vida, possibilitando escrever apenas os nomes das colunas nos comandos, sem o nome do objeto.
attach(dados)

#Examine quantos presas de cada tipo foram mortas pelos lobos. Ha varias formas de produzir esse resultado: voce conhece outras?
counts = table(preytype) # 0  = corças, 1 = alces
counts
barplot(counts,
        xlab = "Tipo de presa",
        names=c("Corças","Alces"),
        ylab = "Frequencia")

#Pense com calma sobre quais fatores devem ser mais importantes para determinar a escolha das presas pelos lobos. Não saia pescando resultados a esmo. Lembre-se das correlacoes espurias: https://www.tylervigen.com/spurious-correlations

#Plote as relacoes entre os fatores escolhidos, adaptando este exemplo aos fatores de sua escolha. Da para alterar varias coisas nos parametros que geram as camadas de desenho do ggplot.
p1 = ggplot(dados, aes(x=moosedensity, y=preytype)) + 
  geom_point(colour = "blue", size=4, alpha = 0.3) + 
  stat_smooth(method="glm", method.args=list(family="binomial"),
              se=T, colour = "blue", fill = "blue", alpha = 0.1) +
  labs(x="Densidade de alces por 10 km2", y = "Corças | Alces") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=40, hjust=0.5),
        axis.text.x = element_text(size = 20, angle=0, hjust=1),
        axis.text.y = element_text(size = 20, angle=0, vjust=1),
        axis.title.x = element_text(size = 30, angle=0),
        axis.title.y = element_text(size = 30, angle=90))

#De uma olhada no grafico produzido:
p1 #essa nuvem em torno da reta de tendencia eh o intervalo de confianca

#Quer exportar o grafico como uma imagem de alta resolucao usando comandos, ao inves da janelinha de export? Experimente esta solucao:
png(filename= "p1.png", res= 300, height= 3000, width= 3500)
p1
dev.off()

#E entao? Voce acha que o fator que voce escolheu explica bem a escolha das presas pelos lobos? Teste isso estatisticamente, usando um modelo linear generalizado com distribuicao binomial. Trata-se de uma analise bem parecida com uma regressao logistica, que discutimos em sala.
fit1 = glm(preytype~moosedensity, family=binomial(link="logit")) 

#Salve o resumo dos resultados como um objeto e depois de uma olhada. A quais resultados voce deve prestar mais atencao?
resultados1 = summary(fit1)
resultados1

#Quer exportar os resultados como um arquivo de texto? Use esta solucao, por exemplo.
capture.output(resultados1, file = "res1.txt")

#Outra forma de testar a significancia seria atraves de uma analise de variancia (ANOVA). Experimente essa alternativa.
res1 = anova(fit1, test="Chisq")
res1 #E ai? Mudou alguma coisa?

#Voce pode tambem calcular a odds ratio, que te informa o quanto o Y muda a cada mudanca de 1 unidade do X.
exp(coef(fit1))

#Voce  pode calcular tambemo intervalo de confianca da odds ratio.
exp(cbind(coef(fit1), confint(fit1)))  

#Outra forma de se testar a significancia seria atraves de comparacao de modelos: um contendo o fator de interesse (fit1, criado alguns passos atras) e outro sem ele (nulo). Veja como faze-lo.
nulo = glm(preytype~1, family=binomial(link="logit")) 
anova(nulo, fit1, test="Chisq") #E ai? Mudou alguma coisa?

#Nao faltam formas de se testar a significancia de um GLM. Uma terceira alternativa eh por selecao de modelos, usando o AICc: criterio de informacao de Akaike corrigido. Escolhe-se o modelo que apresenta menor valor de AICc. Considera-se a diferenca (delta) entre dois modelos significativa, apenas se AICc1 - AICc2 > 2.
AICctab(fit1,nulo)

#Caso voce decida que mais de um fator importa neste caso, pode fazer um modelo multifatorial, como neste exemplo. Esses modelos podem incluir a interacao entre os fatores, mas vamos deixar essa complicacao adicional para outra hora.
fit2 = glm(preytype~moosedensity+timesincekill, family=binomial(link="logit"))

#Confira o resultado.
summary(fit2)



#Sugestoes de leitura:
#1. Origem dos dados: http://dx.doi.org/10.5061/dryad.n863q
#2. Regressao logistica na Wikipedia: https://pt.wikipedia.org/wiki/Regressão_logística
#3. GLM na Wikipedia: https://en.wikipedia.org/wiki/Generalized_linear_model 





p2 = ggplot(dados, aes(x=roedensity, y=preytype)) + 
  geom_point(colour = "blue", size=4, alpha = 0.3) + 
  stat_smooth(method="glm", method.args=list(family="binomial"),
              se=T, colour = "blue", fill = "blue", alpha = 0.1) +
  labs(x="Densidade de corsas por 10 km2", y = "Corças | Alces") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=40, hjust=0.5),
        axis.text.x = element_text(size = 20, angle=0, hjust=1),
        axis.text.y = element_text(size = 20, angle=0, vjust=1),
        axis.title.x = element_text(size = 30, angle=0),
        axis.title.y = element_text(size = 30, angle=90))

p2 

png(filename= "p2.png", res= 300, height= 3000, width= 3500)
p2
dev.off()

fit3 = glm(preytype~roedensity, family=binomial(link="logit")) 

resultados2 = summary(fit3)
resultados2

capture.output(resultados2, file = "res2.txt")

