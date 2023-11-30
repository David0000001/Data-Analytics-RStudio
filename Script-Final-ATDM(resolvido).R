#Script Final ATDM 28/12/2022

#David Cabrita 202100320
#João Almeida 202100068
#João Carreira 20191365

#Alteração do nome do ficheiro para facilitar a sua utilização
dados = EscalaBurnoutGrupo2

#Instalação de todos os packages necessários
install.packages("ggplot2")
install.packages("dplyr")
install.packages("pander")
install.packages("plotrix")
install.packages("rstatix")
install.packages('sjPlot')
install.packages('RVAideMemoire')
install.packages("FSA")
install.packages("GGally")
install.packages("lm.beta")
install.packages('Hmisc')


########################Estatística Univariada##########################

#V.a Idade
#Histograma
library(ggplot2)

table<-table(na.omit(dados$Idade))
data<-as.data.frame(table)
colnames(data)<-c("Idade","Numero_Elementos")

ggplot(data, aes(x =Idade, y =Numero_Elementos, fill = Idade)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Numero_Elementos), vjust = 0)

#Diagrama de extremos e quartis
out <- boxplot.stats(dados$Idade)$out
out_ind <- which(dados$Idade %in% c(out))
out
boxplot.stats(dados$Idade)$out
Idade<-na.omit(dados$Idade)
boxplot(Idade,na.rm=TRUE,
main="Diagrama de extremos e quartis para a idade",
cex.main=1,ylab="Diferença de idades",cex.lab=0.8,horizontal=TRUE,col = "orange")
mtext(paste("Outliers: ", paste(out, collapse = ";")))

#Tabela Summary

summary(dados$Idade)

#V.a Sexo
#Gráfico Circular
library(plotrix)
relat_p2<-round(prop.table(table(dados$Sexo)),3)
pie3D(relat_p2, labels = paste0(relat_p2*100 , "%"), cex=0.8,
main="Grafico cricular para a variavel 'Sexo'",cex.main=1,
col=c("blue","deeppink"))
legend(0.1,1, legend = c("Feminino", "Masculino"),
fill = c("deeppink","blue"), cex=0.9)

#V.a Curso que frequenta
#Grafico Circular
library(plotrix)
relat1 = round(prop.table(table(dados$Curso)),3)
pie3D(relat1, labels=paste0(relat1*100, "%"),cex=0.8,
main="Gráfico circular para a variavel Curso que frequenta",cex.main=1,
col=c("cyan", "green", "red"),explode=0.1)
legend(0.3,1, legend = c("CTeSP TLQB", "L.Bioninformatica","L.Biotecnologia"),
fill = c("cyan","green","red"),cex=0.8)

#V.a Ano Curricular
#Grafico de Barras
barplot(table(dados$`Ano Curricular`),
        main = "Barplot para a variavel 'Ano Curricular'",
        xlab = "Ano Curricular",
        ylab = "Frequencia",
        col=c("darkred"),
        border="black")

#Variável Este curso foi 1ªOpção?
#Gráfico Circular
library(plotrix)
graf <- round(prop.table(table(dados$'1 Opcao')),3)
pie3D(graf, labels = paste0(graf*100, "%"), cex=0.8,
      main="Foi a primeira Opcao?", cex.main=1,
      col=c("Red","Green"))
legend(0.3,1, legend = c("Sim", "Não"),
       fill = c("Red","Green"), cex=0.8)

#V.a Fui eu que escolhi este curso?
#Grafico circular
library(plotrix)
relat_p6=round(prop.table(table(dados$Escolhi)),3)
pie(relat_p6, labels=paste0(relat_p6*100,"%"),cex=0.8,
main = "Fui eu que escolhi este curso?",cex.main=1,
col = c("dodgerblue1","ghostwhite"))
legend(0.3,1,legend = c("Sim","Não"),
fill = c("dodgerblue1","ghostwhite"),cex = 0.8)

#Variável Deslocação Casa-Escola
#Histograma
library(ggplot2)
table2<-table(na.omit(dados$TempoDesloca))
data2<-as.data.frame(table2)
colnames(data2)<-c("TempoDeslocação","NºAlunos")
library(ggplot2)
ggplot(data2, aes(x=TempoDeslocação, y=NºAlunos, fill=TempoDeslocação)) +
geom_bar(stat = "identity") +
geom_text(aes(label = NºAlunos), vjust = 0)

#Diagrama de extremos e quartis # ERROO###################
out2 <- boxplot.stats(dados$TempoDesloca)$out
out_ind2 <- which(dados$TempoDesloca %in% c(out))
out2
boxplot.stats(dados$TempoDesloca)$out2
Idade<-na.omit(dados$TempoDesloca)
boxplot(Idade,na.rm=TRUE,
main="Diagrama de extremos e quartis para a variável Tempo de Deslocaçao",
cex.main=1,xlab="Tempo de Deslocação",cex.lab=0.8,horizontal=TRUE,
col = "cyan")mtext(paste("Outliers: ", paste(out2, collapse = ";")))

#Tabela Summary
summary(dados$TempoDesloca)

#V.a Numero de horas de estudo por semana
#Diagrama de extremos e quartis
out3 <- boxplot.stats(dados$HorasEstudo)$out
out_ind3 <- which(dados$HorasEstudo %in% c(out))
out3
boxplot.stats(dados$HorasEstudo)$out2
Idade<-na.omit(dados$HorasEstudo)
boxplot(Idade,na.rm=TRUE,
main="Diagrama de extremos e quartis para a variável Horas de Estudo",
cex.main=1,
xlab="Horas de Estudo",cex.lab=0.8,
horizontal=TRUE,
col = "lightblue")
mtext(paste("Outliers: ", paste(out3, collapse = ";")))

#Identificação dos outliers
library(rstatix)
library(dplyr)
library(pander)
data_p8=dados[,9]
data_p8%>%
  identify_outliers(HorasEstudo) %>% pander(.,split.table=Inf)

#Tabela Summary
summary(dados$HorasEstudo)

#V.a Numero de horas dedicadas ás redes sociais
out4 <- boxplot.stats(dados$HorasRedes)$out
out_ind4 <- which(dados$HorasRedes %in% c(out))
out4
boxplot.stats(dados$HorasRedes)$out2
Idade<-na.omit(dados$HorasRedes)
boxplot(Idade,na.rm=TRUE,
main="Diagrama de extremos e quartis para a v.a Horas dedicadas ás Redes Sociais",
cex.main=1,
xlab="Horas dedicadas as redes sociais",cex.lab=0.8,
horizontal=TRUE,
col = "indianred3")
mtext(paste("Outliers: ", paste(out4, collapse = ";")))

#Identificacao dos outliers
library(rstatix)
library(dplyr)
library(pander)
data_p9=dados[,10]
data_p9%>%
  identify_outliers(HorasRedes) %>% pander(.,split.table=Inf)

#Tabela Summary
summary(dados$HorasRedes)

#V.a Numero de horas dedicadas a ver TV,Netflix,etc
#Gráfico de barras
barplot(table(dados$HorasTV),
main = "Barplot para a variavel 'Horas na TV'",
xlab = "Horas na TV",
ylab = "Frequencia",
col=c("brown"),
border="black")

#Tabela Summary
summary(dados$HorasTV)

#V.a Numero de horas de sono de 2ª a 6ªfeira
#Diagrama de extremos e quartis
out5 <- boxplot.stats(dados$HorasSono)$out
out_ind5 <- which(dados$HorasSono %in% c(out))
out5
boxplot.stats(dados$HorasSono)$out
Idade<-na.omit(dados$HorasSono)
boxplot(Idade,na.rm=TRUE,
main="Diagrama de extremos e quartis para a variável Horas de sono de 2ª a 6ªfeira",
cex.main=1,
xlab="Horas de sono de 2ª a 6ªfeira",cex.lab=0.8,
horizontal=TRUE,
col = "indianred3")
mtext(paste("Outliers: ", paste(out5, collapse = ";")))

#Tabela Summary
summary(dados$HorasSono)

#V.a Programa de Mentoria
library(plotrix)
relat2 = round(prop.table(table(dados$PMentoria)),2)
pie3D(relat2, labels=paste0(relat2*100, "%"),cex=0.5,
main="Gráfico circular par a variável Programa de Mentoria",cex.main=1,
col=c("cyan","blueviolet"),explode=0.1)
legend(0.5,1, legend = c("Não","Sim"),
fill = c("cyan","blueviolet"),cex=0.8)
relat2 = round(prop.table(table(dados$PMentoria)),2)

#########################################Análise Descritiva Bivariada#########################################

#Variável Sexo e Burnout P1
##Mudança de nome de variáveis

###Mudança do sexo
dados$Sexo[dados$Sexo == 1] <- 'Feminino'
dados$Sexo[dados$Sexo == 2] <- 'Masculino'

###Mudança do BurnoutP1
dados$BurnoutP1[dados$BurnoutP1 == 0] <- '0 - Nunca'
dados$BurnoutP1[dados$BurnoutP1 == 1] <- '1 - Quase Nunca'
dados$BurnoutP1[dados$BurnoutP1 == 2] <- '2 - Algumas Vezes'
dados$BurnoutP1[dados$BurnoutP1 == 3] <- '3 - Regularmente'
dados$BurnoutP1[dados$BurnoutP1 == 4] <- '4 - Muitas Vezes'
dados$BurnoutP1[dados$BurnoutP1 == 5] <- '5 - Quase Sempre'
dados$BurnoutP1[dados$BurnoutP1 == 6] <- '6 - Sempre'

#Tabela de Contingencia
library(sjPlot)
sjt.xtab(
  dados$Sexo, dados$BurnoutP1,
  show.row.prc = TRUE, 
  show.summary = TRUE, 
  statistics = "auto",
)

#V.a Sexo e Curso que frequenta

##Mudança do nome das v.a
dados$Curso[dados$Curso == 'L.Bioinformática'] <- 'L. Bioinformatica'

##Tabela de Contingencia
library(sjPlot)
sjt.xtab(
  dados$Sexo,dados$Curso,
  show.row.prc=TRUE,
  show.summary=TRUE,
  statistics="auto",
)

#V.a Ano Curricular e Horas de Sono

##Coeficiente de Correlacao de Pearson
cor(dados$HorasEstudo,dados$HorasSono)


##########################################Estudo Inferencial##################################################

###Ano Curricular e tempo de deslocação Casa-Escola

#Alteracao das v.a para criação de uma tabela mais legível
dados$`Ano Curricular`[dados$`Ano Curricular` == 1] <- 'Primeiro'
dados$`Ano Curricular`[dados$`Ano Curricular` == 2] <- 'Segundo'
dados$`Ano Curricular`[dados$`Ano Curricular` == 3] <- 'Terceiro'

##Tabela de Contingencia
library(rstatix)
library(dplyr)
library(pander)
descritivas1 = dados %>%
  group_by(`Ano Curricular`) %>%
  get_summary_stats(TempoDesloca, type = "full",
                    show=c("n","min","max","q1","median","q3","mean","sd"))
descritivas1

##Tabela de Contingencia formato pander
library(pander)
descritivas1 %>% pander(.,split.table=Inf)

##Identificacao de Outliers

#Criação da tabela apenas com as variáveis em questão
dados2 = dados[,c('Ano Curricular','TempoDesloca')]
dados2

#Identificação dos outliers
library(rstatix)
library(dplyr)
library(pander)
dados2 %>% group_by(`Ano Curricular`) %>%
  identify_outliers(TempoDesloca) %>% pander(.,split.table=Inf)

##Teste á normalidade
library(RVAideMemoire)
ks.test(EscalaBurnoutGrupo2$TempoDesloca,EscalaBurnoutGrupo2$`Ano Curricular`)

##Teste de Kruskal-Wallis
library(RVAideMemoire)
kruskal.test(TempoDesloca ~ dados$`Ano Curricular`,dados)

##Tabela de Dimensão do efeito
kruskal_effsize(TempoDesloca ~ `Ano Curricular`, data=dados,ci=T)%>%
  pander(.,split.table=Inf) #Pode demorar a correr

##Teste de Dunn
library(FSA)
dunnTest(TempoDesloca ~ dados$`Ano Curricular`,dados,
         method="bonferroni")

##Boxplot
boxplot(TempoDesloca ~ dados$`Ano Curricular`,dados,
        ylab = "Tempo de Deslocação (minutos)",
        xlab = "Ano Curricular",
        col="deepskyblue")

###Horas de estudo e 1 opcao
dados$`1 Opcao`[dados$`1 Opcao` == 1] <- 'Sim'
dados$`1 Opcao`[dados$`1 Opcao` == 2] <- 'Não'

##Tabela de Contingencia
library(rstatix)
library(dplyr)
descritivas_3 = dados %>%
  group_by(`1 Opcao`) %>%
  get_summary_stats(HorasEstudo, type = "full",
                    show=c("n","min","max","q1","median","q3","mean","sd"))
descritivas_3

##Tabela de Contingencia em formato pander
library(pander)
descritivas_3 %>% pander(.,split.table=Inf)

##Criacao da tabela com os dados em estudo
dados4 = dados[,c('1 Opcao','HorasEstudo')]

##Identificacao dos outliers
library(rstatix)
descritivas_3 = dados4 %>%
  group_by(`1 Opcao`) %>%
  identify_outliers(HorasEstudo) %>% pander(.,split.table=Inf)

##Teste a normalidade
ks.test(EscalaBurnoutGrupo2$HorasEstudo,EscalaBurnoutGrupo2$`1 Opcao`)

##Teste de Wilcoxon
wilcox.test(HorasEstudo ~ `1 Opcao`, data=dados, mu=0,
            alternative="two.sided",correct="True")

##Valor de Z
library(dplyr)

wilcox.test(HorasEstudo ~ `1 Opcao`, data=dados, mu=0,
            alternative="two.sided",correct="True")%>%
  with(tibble(U=statistic,
              Z=qnorm(p.value/2),
              p=p.value))

##BoxPlot
boxplot(dados$HorasEstudo ~ dados$`1 Opcao`,
        ylab="Horas de Sono",
        xlab="1ª Opcao",
        col='darkred')

###Horas de sono e Curso

##Criacao de nova df com v.a Curso e HorasSono
Dados3 <- dados[,c('Curso','HorasSono')]
Dados3
##Tabela de Contingencia
library(rstatix)
library(dplyr)
descritivas2 = dados %>%
  group_by(Curso) %>%
  get_summary_stats(HorasSono, type = "full",
                    show=c("n","min","max","q1","median","q3","mean","sd"))
descritivas2

##Tabela de Contingencia em formato pander
library(pander)
descritivas2 %>% pander(.,split.table=Inf)

##Identificacao de Outliers
library(rstatix)
Descritivas = Dados3 %>%
  group_by(Curso) %>%
  identify_outliers(HorasSono) %>% pander(.,split.table=Inf)

## Normalidade para CTest (n<30) - Teste de Shapiro-Wilk
shapiro.test(Dados3$HorasSono[Dados3$Curso=="CTeSP TLQB"])

##Normalidade para os outros cursos
library(nortest)
lillie.test(Dados3$HorasSono[Dados3$Curso=="L. Bioinformática"])
lillie.test(Dados3$HorasSono[Dados3$Curso=="L. Biotecnologia"])

## Dimensao do efeito: eta_quadrado ordinal
library(pander)
library(rstatix)
kruskal_effsize(HorasSono ~ Curso, data = Dados3, ci =T) %>% 
  pander(.,split.table=Inf)

kruskal.test(HorasSono ~ Dados3$Curso,Dados3)
#Pode demorar a correr

##BoxPlot
boxplot(HorasSono ~ dados$Curso,dados,
        ylab = "Horas de Sono",
        xlab = "Curso",
        col="darkolivegreen3")

########################Regressao Linear Multipla###############################

###Horas de sono horas de estudo e curso

dados$Curso[dados$Curso == "L. Biotecnologia"] <- "Biotecnologia"
dados$Curso[dados$Curso == "L. Bioinformática"] <- "Bioinformatica"
##GRAFICO DE DISPERSAO (3 VARIAVEIS)
library(ggplot2)
library(GGally)

dados_mrl = data.frame(dados$HorasSono, dados$TempoDesloca,
                       dados$HorasEstudo)
ggpairs(dados_mrl)

##CONSTRUCAO DO MODELO: ##
modelo_2 = lm(HorasSono ~ TempoDesloca + HorasEstudo, dados)
summary(modelo_2)

##VERIFICACAO PRESSUPOSTOS DO MODELO: ##
#REPRESENTACAO GRAFICA: 
par(mfrow=c(2,2))
plot(modelo_2,pch = 19)
par(mfrow=c(1,1))

##OBTENCAO DOS COEFICIENTES PADRONIZADOS
library(lm.beta)

lm.beta(modelo_2) %>% pander(.,split.table=Inf)

##CRITERIOS DE AIC E BIC PARA COMPARAR QUASISQUER MODELOS:
modelo = lm(HorasSono ~ HorasEstudo, dados)
summary(modelo)
AIC(modelo, modelo_2)
BIC(modelo, modelo_2)

##ANOVA PARA COMPARAR MODELOS ANINHADOS
anova(modelo, modelo_2)

#DIAGRAMA DE DISPERSAO:
colvec<-ifelse(dados$Curso == "Biotecnologia", "blue",
               ifelse(dados$Curso == "Bioinformatica","red","green"))
plot(dados$HorasEstudo, dados$HorasSono, col=colvec,pch = 19, ylim=c(0,25),
     xlab="Horas Estudo", ylab="Horas Sono")
legend(3,25, legend=c("L.Biotecnologia","L.Bioinformatica","CTeSP"),cex=0.45,
       fill=c("blue","red","green"))
title(main="Horas Estudo vs Horas Sono")

#Criar a variavel dummy Biotecnologia:
  Biotecnologia = ifelse(dados$Curso == "Biotecnologia",1,0)

#Criar a variavel dummy Bioinformatica:
Bioinformatica = ifelse(dados$Curso == "Bioinformatica",1,0)

##Estimar o modelo:
modelo_hs_1 <- lm(HorasSono ~ HorasEstudo + Biotecnologia + Bioinformatica, data=dados)
summary(modelo_hs_1)

##Pressupostos do modelo de regressão (Representacao grafica):
par(mfrow=c(2,2))
plot(modelo_hs_1,pch = 19)
par(mfrow=c(2,1))

##Representacao grafica do modelo:
plot(dados$HorasEstudo, dados$HorasSono, type ='n')
points(dados$HorasEstudo[dados$Curso=="Biotecnologia"],
       dados$HorasSono[dados$Curso=="Biotecnologia"],col='blue',pch=19)
points(dados$HorasEstudo[dados$Curso=="Bioinformatica"],
       dados$HorasSono[dados$Curso=="Bioinformatica"],col='red',pch=19)
points(dados$HorasEstudo[dados$Curso=="CTeSP TLQB"],
       dados$HorasSono[dados$Curso=="CTeSP TLQB"],col='green',pch=19)
legend(45,8.7, legend=c("L.Biotecnologia","L.Bioinformática", "CTeSP"),cex=0.8,
       fill= c("blue","red","green"))
abline(coef(modelo_hs_1)[1]+coef(modelo_hs_1)[3],coef(modelo_hs_1)[2],
       col='blue')
abline(coef(modelo_hs_1)[1]+coef(modelo_hs_1)[4],
       coef(modelo_hs_1)[2], col='red')
abline(coef(modelo_hs_1)[1], coef(modelo_hs_1)[2], col='green')

##Criterios de AIC e BIC para comparar quaisquer modelos:
AIC(modelo, modelo_hs_1)
BIC(modelo,modelo_hs_1)

##Para comparar modelos aninhados
anova(modelo,modelo_hs_1)

###Horas de sono PMentoria e Tempo de Deslocacao###

Dados4 <- dados[,c('PMentoria','HorasSono','TempoDesloca')]
Dados4

### Representacoes graficas: Diagrama de dispersao:
## Diagrama de dispersão para visualizar os Dados4 dos estudantes 
# do PMentoria 1 (a azul) e do PMentoria 2 (a rosa)
colvec<-ifelse(Dados4$PMentoria=="1","blue","deeppink")
plot(Dados4$HorasSono,Dados4$TempoDesloca,col=colvec,pch = 19,
     xlab="HorasSono",ylab="TempoDesloca")
legend(3,160, legend=c("Sim", "Nao"), cex=0.8,
       fill = c("blue","deeppink"))
title(main="HorasSono vs TempoDesloca")

## Criação duma a variavel dummy "Sim":
Sim = ifelse(Dados4$PMentoria=="1",1,0)

## Estimar modelo
modelo_Sim <-lm(HorasSono ~ TempoDesloca + Sim,data=Dados4)
summary(modelo_Sim)

## Pressupostos do modelo de regressao (Representacao grafica):
par(mfrow=c(2,2))
plot(modelo_Sim,pch = 19)
par(mfrow=c(1,1))

## Representacao grafica do modelo
plot(Dados4$HorasSono,Dados4$TempoDesloca,type='n')
points(Dados4$HorasSono[Dados4$PMentoria=="1"],
       Dados4$TempoDesloca[Dados4$PMentoria=="1"],col='blue',pch=19)
points(Dados4$HorasSono[Dados4$PMentoria=="2"],
       Dados4$TempoDesloca[Dados4$PMentoria=="2"],col='red',pch=19)
legend(3,160, legend=c("Sim", "Nao"), cex=0.6,
       fill = c("blue","red"))
abline(coef(modelo_Sim)[1],coef(modelo_Sim)[2], col='blue')
abline(coef(modelo_Sim)[1]+coef(modelo_Sim)[3],
       coef(modelo_Sim)[2], col='red')

#############################Analise Fatorial###################################

colnames(Dados3)[1] = "P1"
colnames(Dados3)[2] = "P2"
colnames(Dados3)[3] = "P3"
colnames(Dados3)[4] = "P4"
colnames(Dados3)[5] = "P5"
colnames(Dados3)[6] = "P6"
colnames(Dados3)[7] = "P7"
colnames(Dados3)[8] = "P8"
colnames(Dados3)[9] = "P9"
colnames(Dados3)[10] = "P10"
colnames(Dados3)[11] = "P11"
colnames(Dados3)[12] = "P12"
colnames(Dados3)[13] = "P13"
colnames(Dados3)[14] = "P14"
colnames(Dados3)[15] = "P15"

Dados5 <- dados[,c('BurnoutP1','BurnoutP2','BurnoutP3','BurnoutP4','BurnoutP5',
                   'BurnoutP6','BurnoutP7','BurnoutP8','BurnoutP9',
                   'BurnoutP10','BurnoutP11','BurnoutP12','BurnoutP13','BurnoutP14',
                   'BurnoutP15')]
Dados5

##Matriz de Correlacao
library(Hmisc)
mat_corr = rcorr(as.matrix(Dados5), type=c("spearman"))
mat_corr
#Matriz de Correlacao triangular superior
lowerCor(Dados5)

if(!require(psych)) install.packages("psych")
library(psych)
library(Hmisc)
if(!require(corrplot)) install.packages("corrplot")
library(corrplot)

corrplot(mat_corr$r, type = "lower",
         p.mat = mat_corr$r,sig.level = 0.0005, insig = "p-value")

##Teste de esfericidade

cortest.bartlett(mat_corr$r,n=nrow(Dados5))

#Determinação do número de fatores a considerar na solução inicial
#Usando o método de Kaiser
fit=princomp(Dados3,cor=TRUE)
fit
summary(fit)

#Construção da soluçao inicial
fit1=principal(Dados3, nfactors = 3,
               n.obs = nrows(Dados3),rotate = "none", scores = TRUE)
fit1
fit2=principal(Dados3, nfactors = 5,
               n.obs = nrows(Dados3),rotate = "none", scores = TRUE)
fit2

#Valores das comunalidades
fit1$communality

#Matriz de correlacao grafica
corrplot(mat_corr$r,p.mat=mat_corr$P,
         sig.level=0.05,method="number",type="lower")


























