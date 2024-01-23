
install.packages("FuzzyR")
install.packages("frbs")
install.packages("BBmisc")
install.packages("caret")

########################MÉTODOS DE CLASSIFICAÇÃO########################### ############# 

#-"FRBCS.W": sistemas de classificação baseados em regras difusas com fator de ponderação baseado no método de Ishibuchi para lidar com tarefas de classificação. 
#-"FRBCS.CHI": sistemas de classificação baseados em regras fuzzy baseados no método de Chi para lidar com tarefas de classificação. 
#-"GFS.GCCL": O método de Ishibuchi baseado na aprendizagem competitiva e cooperativa genética para lidar com tarefas de classificação. 
#-"FH.GBML": O método de Ishibuchi baseado na hibridização da aprendizagem competitiva cooperativa genética e Pittsburgh para lidar com tarefas de classificação.
#-"SLAVE": algoritmo de aprendizagem estrutural em ambiente vago para lidar com tarefas de classificação.

library(FuzzyR)
library(frbs)
library(caret)
library(BBmisc)

help(sum)

#carrega o banco 
load<-read.csv("todas.csv") # carrega arquivos
dt <- read.csv("todas.csv", head=T, sep=",") # cria um objeto com os dados 

#carrega o banco agrupameto
#load<-read.csv("dadosagrupamento.csv") # carrega arquivos
#dadosagrupamento<- read.csv("dadosagrupamento.csv", head=T, sep=";") # cria um objeto com os dados 


#normalização completa dos dados
#dtva<-normalize(dtva, method = "range", range = c(0, 1))

#normalização apenas dos atributos. A classe deve permanecer a mesma e ser numérica
dtva$distance <- with(dtva, (distance - min(distance)) / (max(distance) - min(distance)))
dtva$velocity <- with(dtva, (velocity - min(velocity)) / (max(velocity) - min(velocity)))
dtva$acceleration <- with(dtva, (acceleration - min(acceleration)) / (max(acceleration) - min(acceleration)))


#Essa semente evita que seja encontrado um resultado diferente a cada rodada
set.seed(0451)

#Particionando os dados em 10 folds (10-fold-cross-validation)
inTrain <- createDataPartition(y = dt$risk,
                               p = .9,
                               list = FALSE, times=10)

#Criando um data frame para armazenar o erro de cada fold
error_df <- data.frame(matrix(ncol = 2, nrow = ncol(inTrain)))
colnames(error_df) <- c('test_error', 'fold')

#Criando um data frame para armazenar a acurácia de cada fold
accuracy_df <- data.frame(matrix(ncol = 2, nrow = ncol(inTrain)))
colnames(accuracy_df) <- c('test_accuracy', 'fold')

#Criando um data frame para armazenar a quantidade de regras cada fold
rules_df <- data.frame(matrix(ncol = 2, nrow = ncol(inTrain)))
colnames(rules_df) <- c('#rules', 'fold')

#Executando o gerador de regras para cada partição aleatória
for(i in 1:nrow(error_df)){
  
  #Obtendo os conjuntos de teste e treinamento de cada fold
  data.train <- dt[inTrain[,i],]
  data.tst <- dt[-inTrain[,i],]
  real.dist <- data.tst$risk
  
  #Calculando o intervalo de valores dos atributos, exceto a classe
 range.data.input <- apply(dt[, -ncol(dt)], 2, range)
  
 #range.data.wm <- apply(data.train, 2, range)
  
  
  #Selecionando o tipo de função de pertiência
  type.mf <-"TRIANGLE"
  #type.mf<-"TRAPEZOID"
  #type.mf<-"GAUSSIAN"
  
  #Selecionando a quantidade de termos linguísticos
 #num.labels <- 3
 num.labels <-5
  
  #Selecionando a t-norma
  type.tnorm = "MIN"
  #type.tnorm = "MAX"
  
  #Selecionando a s-norma
  type.snorm = "MIN"
  #type.snorm = "MAX"
  
  #Selecionando o método de implicação
  type.implication.func = "ZADEH"
  
  control <- list(num.class=3, num.labels=num.labels, type.mf=type.mf, type.tnorm = type.tnorm, type.snorm = type.snorm, type.implication.func = type.implication.func, name = "Riscos de Colisao")
  
  #method.type<-"FRBCS.W"
  #method.type <- "FRBCS.CHI"
  method.type <-"GFS.GCCL"
 #method.type <-"FH.GBML"
  #method.type <-"SLAVE"
  
  #method.type <-"DEFINS"
  
  #method.type <-"WM"
  
  
    #Executar o gerador de regras
  obj<-frbs.learn (data.train, range.data.input, method.type=method.type, control)
  pred <- predict(obj, data.tst[,-ncol(data.tst)])
  
  #Plotar as funções de pertinência
  plotMF(obj)
  
  #Verificar os parâmetros e as regras
  summary(obj)
  
  #Encontrando o erro por fold
  err <- mean(ifelse(real.dist != pred, 1, 0))
  error_df[i,'test_error'] <- err
  error_df[i, 'fold'] <- i
  
  #Encontrando a quantidade de regras por fold
  rules_df[i,'#rules'] <- nrow(obj$rule)
  rules_df[i, 'fold'] <- i
  
  #Encontrando a acurácia por fold
  sum(pred != real.dist)
  (pred != real.dist)
  acuracia <- (1-err)*100
 
  accuracy_df[i,'test_accuracy'] <- acuracia
  accuracy_df[i, 'fold'] <- i
 
  acuracia
   
}

#Erro obtido após x folds
mean(error_df[,'test_error'])

#Acurácia obtida após x folds
mean(accuracy_df[i,'test_accuracy'])

#Quantidade média de regras obtidas após x folds
mean(rules_df[i,'#rules'])




#singh


dados<-read.csv("todas.csv") #carrega a base de dados
dado<-dados[0:3]
dado
install.packages("xyplot")
library(lattice)

serietf<-ts(dado)#gera uma série temporal com os dados
serie
xyplot(serie, superpose = TRUE) 

plot(serie) # gera o gráfico da serie
s<-ts(dado)
s
plot(s)
install.packages("AnalyzeTS")
library(AnalyzeTS)

options(max.print = 2000)

fuzzy.ts1(s,n=5,type="Chen",trace=TRUE, plot=TRUE,grid=TRUE)

fuzzy.ts1(s,n=5,type="Singh",trace=TRUE, plot=TRUE,grid=TRUE)

fuzzy.ts1(s,n=5,type="Heuristic",trace=TRUE, plot=TRUE,grid=TRUE)


a<-fuzzy.ts1(s,n=5,type="Chen-Hsu",plot=1)
b<-ChenHsu.bin(a$table1,n.subset=c(1,1,1,1,1))
chenhsu5<-fuzzy.ts1(s,type="Chen-Hsu",bin=b, plot=1,trace=1)
chenhsu5

install.packages("zoo")
library(zoo)
library(XLConnect)
library(lubridate)
library(ggplot2)

