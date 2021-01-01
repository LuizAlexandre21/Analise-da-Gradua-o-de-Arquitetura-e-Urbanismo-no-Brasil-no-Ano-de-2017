#######################Projeto de Estatistica
###################### 1- Pre-tratamento dos dados
###### Alterando o Padrão do arquivo - Usando o arquivo Replace.bash
#### "," -> "."
#### ";" -> ","

####################### 2- Importando para o Mongodb
#####mongoimport --type csv -d db_fb -c Enade --headerline --drop MICRODADOS_ENADE_2017.txt
######### Visualizando o formato da base
####use db_fb
####db.Enade.findOne()


############################# Trabalhando no R #######################################
#source("Projeto_bruto.R")
######importando pacotes
#install,packages("plyr")
#install.packages("mongolite")
#install.packages("tydeverse")
#install.packages("PerformanceAnalitics")
#install.packages("ggplot2")
#install.package(fastDummies)
library(rmarkdown)
library(knitr)
library(mongolite)
library(PerformanceAnalytics)
library(tidyr)
library(plyr)
library(ggplot2)
library(fastDummies)
######Acessando o banco de dados
data=mongo(collection = "Enade",db = "db_fb",url = "mongodb://localhost",verbose = FALSE,options = ssl_options())
######Selecionando os campos e Filtrando os campos
dados<-data$find('{"CO_GRUPO":21}',fields='{"NT_OBJ_FG":1,"CO_GRUPO":1,"CO_REGIAO_CURSO":1, "QE_I02":1, "CO_TURNO_GRADUACAO":1}')
############Rotulando os Campos
############Curso
dados$CO_GRUPO[dados$CO_GRUPO==21]<-"Arquitetura e Urbanismo"
############Região do Curso
dados$CO_REGIAO_CURSO[dados$CO_REGIAO_CURSO==1]<-"Norte"
dados$CO_REGIAO_CURSO[dados$CO_REGIAO_CURSO==2]<-"Nordeste"
dados$CO_REGIAO_CURSO[dados$CO_REGIAO_CURSO==3]<-"Sudeste"
dados$CO_REGIAO_CURSO[dados$CO_REGIAO_CURSO==4]<-"Sul"
dados$CO_REGIAO_CURSO[dados$CO_REGIAO_CURSO==5]<-"Centro-Oeste"
############Cor da Pele
dados$QE_I02[dados$QE_I02=="A"]<-"Branca"
dados$QE_I02[dados$QE_I02=="B"]<-"Preta"
dados$QE_I02[dados$QE_I02=="C"]<-"Amarela"
dados$QE_I02[dados$QE_I02=="D"]<-"Parda"
dados$QE_I02[dados$QE_I02=="E"]<-"Indígenas"
dados$QE_I02[dados$QE_I02=="F"]<-"Não quero declarar"
#############Turno
dados$CO_TURNO_GRADUACAO[dados$CO_TURNO_GRADUACAO==1]<-"Matutino"
dados$CO_TURNO_GRADUACAO[dados$CO_TURNO_GRADUACAO==2]<-"Vespetino"
dados$CO_TURNO_GRADUACAO[dados$CO_TURNO_GRADUACAO==3]<-"Integral"
dados$CO_TURNO_GRADUACAO[dados$CO_TURNO_GRADUACAO==4]<-"Norturno"

###############Analise Descritiva
dados$NT_OBJ_FG<-as.numeric(dados$NT_OBJ_FG) ###########Transformando as notas em numericos
sum(is.na(dados$NT_OBJ_FG)) ###########Numero de dados faltantes das notas gerais
sum(is.na(dados$QE_I02))    ##########Numero de dados faltantes das notas gerais
sum(is.na(dados$CO_REGIAO_CURSO)) ##########Numero de dados faltantes das notas gerais
sum(is.na(dados$CO_TURNO_GRADUACAO)) ##########Numero de dados faltantes das notas gerais
sum(is.na(dados$CO_GRUPO))  ##########Numero de dados faltantes das notas gerais

##############Removendo as linhas faltantes
dados<-dados[!is.na(dados$NT_OBJ_FG),]
dados<-dados[!dados$QE_I02==unique(dados$QE_I02)[6],] ###Existe um campo com vazio diferente de Na


###############Criando Analises Graficas
#####Distribuição Simples dos Alunos
###Alunos de Arquitetura por Região
ggplot(data.frame(dados$CO_REGIAO_CURSO),aes(x=dados$CO_REGIAO_CURSO))+geom_bar(fill=c("darkblue"))
###Notas
ggplot(data.frame(dados$NT_OBJ_FG),aes(x=dados$NT_OBJ_FG))+geom_bar(fill=c("darkblue"))
###Turno
ggplot(data.frame(dados$CO_TURNO_GRADUACAO),aes(x=dados$CO_TURNO_GRADUACAO))+geom_bar(fill=c("darkblue"))
####
ggplot(data.frame(dados$QE_I02),aes(x=dados$QE_I02))+geom_bar(fill=c("darkblue"))

######Analise condicionais
###1- Como a cor da pele influência nos aspectos do curso
##Bases
dados_negros<-dados[dados$QE_I02=="Preta",]
dados_brancos<-dados[dados$QE_I02=="Branca",]
dados_amarelos<-dados[dados$QE_I02=="Amarela",]
dados_parda<-dados[dados$QE_I02=="Parda",]
dados_indigenas<-dados[dados$QE_I02=="Indígenas",]
##Dados Descritivos
table.Stats(dados_negros$NT_OBJ_FG)
table.Stats(dados_brancos$NT_OBJ_FG)
table.Stats(dados_amarelos$NT_OBJ_FG)
table.Stats(dados_indigenas$NT_OBJ_FG)
table.Stats(dados_parda$NT_OBJ_FG)

##Boxplot - Distribuição de Notas
p1 <- ggplot(dados, aes(x=QE_I02, y=NT_OBJ_FG)) + geom_boxplot()

##Histograma - Composição das regiões
p2<-ggplot(dados,aes(factor(CO_REGIAO_CURSO),fill=(QE_I02)))+ geom_histogram(stat="count", position = "dodge") + scale_fill_brewer(palette = "Set1")
##Histograma - Composição dos Turnos
P3<-ggplot(dados,aes(factor(CO_TURNO_GRADUACAO),fill=(QE_I02)))+geom_histogram(stat="count",position="dodge")+scale_fill_brewer(palette="Set1")

####2 Quais as diferenças de notas entre os turnos
##Bases
dados_noturno<-dados[dados$CO_TURNO_GRADUACAO=="Noturno",]
dados_vespertino<-dados[dados$CO_TURNO_GRADUACAO=="Vespetino",]
dados_integral<-dados[dados$CO_TURNO_GRADUACAO=="Integral",]
dados_matutino<-dados[dados$CO_TURNO_GRADUACAO=="Matutino",]
##Dados Descritivos
table.Stats(dados_noturno$NT_OBJ_FG)
table.Stats(dados_vespertino$NT_OBJ_FG)
table.Stats(dados_integral$NT_OBJ_FG)
table.Stats(dados_matutino$NT_OBJ_FG)

## Box plot
p4 <- ggplot(dados, aes(x=CO_TURNO_GRADUACAO, y=NT_OBJ_FG)) + geom_boxplot()
p5<-ggplot(dados,aes(factor(CO_REGIAO_CURSO),fill=(CO_TURNO_GRADUACAO)))+ geom_histogram(stat="count", position = "dodge") + scale_fill_brewer(palette = "Set1")

##3 Quais caracteristicas entre as Regioes
dados_norte<-dados[dados$CO_REGIAO_CURSO=="Norte",]
dados_nordeste<-dados[dados$CO_REGIAO_CURSO=="Nordeste",]
dados_centro<-dados[dados$CO_REGIAO_CURSO=="Centro-Oeste",]
dados_sudeste<-dados[dados$CO_REGIAO_CURSO=="Sudeste",]
dados_sul<-dados[dados$CO_REGIAO_CURSO=="Sul",]

##Dados Descritivos
table.Stats(dados_norte$NT_OBJ_FG)
table.Stats(dados_nordeste$NT_OBJ_FG)
table.Stats(dados_centro$NT_OBJ_FG)
table.Stats(dados_sudeste$NT_OBJ_FG)
table.Stats(dados_sul$NT_OBJ_FG)

p6 <- ggplot(dados, aes(x=CO_REGIAO_CURSO, y=NT_OBJ_FG)) + geom_boxplot()


#####Regressão Linear
####Ajustando o banco de dados
Região<-dummy_cols(dados$CO_REGIAO_CURSO)
Turno<-dummy_cols(dados$CO_TURNO_GRADUACAO)
cor<-dummy_cols(dados$QE_I02)
###Criando os banco de dados
data_lm<-dados$NT_OBJ_FG

##Modelo linear
