


##Atividade avaliativa 2 - Classificação "Machine Learning"

# 10(a)

install.packages("ISLR")
install.packages("MASS")
library(ISLR)
library(MASS)
attach(Weekly)
#verifique o tamanho do dataframe
dim(Weekly)

#Ver o conjunto de dados
head(Weekly)

#classe de variáveis do conjunto de dadoss
str(Weekly)

#Resumo dos dados
summary(Weekly)

#encontrar correlação
correlation<-cor(Weekly[-9])
correlation

#visualizar correlação
install.packages("corrplot")
library(corrplot)
corrplot(correlation,type="upper")
install.packages("ggplot2")
library(ggplot2)

#Plotar variáveis correlacionadas como dados de séries temporais Volume vs Ano
volumetimeseries <- ts(Volume, frequency=52, start=c(1990,1))
plot.ts(volumetimeseries)

# 10(b)

#modelo completo
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly,family = binomial)
summary(glm.fit)

# 10(c)

#Matriz de confusão para modelo completo

glm.probs<-predict(glm.fit,type="response")
glm.pred=rep("Down",1089)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred,Direction)
mean(glm.pred==Direction)

predicted<-glm.probs>0.5
predicted<-as.numeric(predicted)
table(Direction, predicted, dnn = c("Actual Direction", "Predicted Direction"))

#precisão do modelo
oa<-(54+557)/(length(predicted))
oa

sensitivity<-557/(557+430)
sensitivity

specificity<-(54)/(54+48)
specificity

precision<-557/(557+48)
precision

type1error<-48/(48+54)
type1error

#A precisão geral do modelo é de 56,11%. A sensibilidade é de 56,43 por cento, o que indica que somos capazes de ter um desempenho melhor do que a linha de base. Mas este é o modelo construído em todo o conjunto de dados e precisamos ver os resultados para testar os dados.


# 10(d) Agora, ajuste o modelo de regressão logística usando um período de dados de treinamento de 1990 a 2008, com o Lag2 como o único preditor. Calcule a matriz de confusão e a fração geral de previsões corretas para os dados retidos (isto é, os dados de 2009 e 2010).

#particionamento de dados e previsão para dados de teste
train<-(Year<2009)
Weekly.0910<-Weekly[!train,]
dim(Weekly.0910)


mean<-mean(glm.pred==Direction.0910)
mean

#Usando apenas a variável mais significativa, Lag 2, em nosso modelo de regressão logística, achamos que a precisão absoluta do modelo aumenta para 62,5%.

# 10(e)

#LDA
lda.fit<-lda(Direction~Lag2,data=Weekly,subset=train)
lda.predict=predict(lda.fit,Weekly.0910)
lda.class=lda.predict$class
table(Direction.0910,lda.class,dnn = c("Actual Direction", "Predicted Direction"))

mean<-mean(lda.class==Direction.0910)

#Usando a Análise Linear Discriminante, obtemos uma precisão geral de 62,5%, que é igual à Regressão Logística.

# 10(f)

#QDA
qda.fit<-qda(Direction~Lag2,data=Weekly,subset=train)
qda.predict=predict(qda.fit,Weekly.0910)
qda.class=qda.predict$class
table(Direction.0910,qda.class,dnn = c("Actual Direction", "Predicted Direction"))
mean<-mean(qda.class==Direction.0910)

#Usando a Análise Discriminante Quadrática, obtemos uma precisão geral de 58,65%. A QDA prevê que a direção sempre será

# 10(h)

##Se usarmos a precisão geral do modelo como nosso critério de julgamento, obteremos a Regressão Logística e a LDA dando os melhores resultados.

# 11(a)

attach(Auto)
Auto1<-Auto
Auto1$mpg0<-ifelse(Auto1$mpg>median(Auto1$mpg),1,0)

# 11(b)

library(corrplot)
M<-cor(Auto1[,-9])
corrplot(M, type="upper")

# 11(c) O conjunto de dados é dividido aleatoriamente na proporção 70-30. Dados de treinamento têm 70% de observações, enquanto os dados de teste têm 30%

set.seed(1)
subset <- sample(nrow(Auto1), nrow(Auto1) * 0.7)
autotrain = Auto1[subset, ]
autotest = Auto1[-subset, ]
dim(autotrain)
dim(autotest)

# 11(d)

set.seed(1)
autotrain.lda.fit<-lda(mpg0~cylinders+displacement+horsepower+weight,data=autotrain)
autotest.lda.predict=predict(autotrain.lda.fit,autotest)
autotest.lda.class=autotest.lda.predict$class
table(autotest$mpg0,autotest.lda.class,dnn = c("Actual Mileage", "Predicted Mileage"))
round(mean(autotest.lda.class!=autotest$mpg0),3)

#LDA Erro de teste é 0,076 para o modelo com variáveis cilindros, deslocamento, potência, peso

#11(e)

set.seed(1)
autotrain.qda.fit<-qda(mpg0~cylinders+displacement+horsepower+weight,data=autotrain)
autotest.qda.predict=predict(autotrain.qda.fit,autotest)
autotest.qda.class=autotest.qda.predict$class
table(autotest$mpg0,autotest.qda.class,dnn = c("Actual Mileage", "Predicted Mileage"))
round(mean(autotest.qda.class!=autotest$mpg0),3)

#O erro de teste QDA é 0.102 para o modelo com variáveis cilindros, deslocamento, potência, peso


# 11(f)

set.seed(1)
autotrain.glm.fit<-glm(mpg0~cylinders+displacement+horsepower+weight,data=autotrain,family = binomial)
autotest.glm.predict=predict(autotrain.glm.fit,autotest,type="response")
autotest.glm.class=ifelse(autotest.glm.predict>0.5,1,0)
table(autotest$mpg0,autotest.glm.class,dnn = c("Actual Mileage", "Predicted Mileage"))
round(mean(autotest.glm.class!=autotest$mpg0),3)

#Erro de teste de regressão logística é 0,11 para o modelo com variáveis cilindros, deslocamento, potência, peso

# 13

attach(Boston)
Boston1<-Boston
#Criando crim0. Ele é codificado como 1 se o valor de crim estiver acima da mediana e 0 caso contrário
Boston1$crim0<-ifelse(Boston1$crim>median(Boston1$crim),1,0)
#correlação de visulização
library(corrplot)
M<-cor(Boston1[,])
corrplot.mixed(M)

#dividindo dados
set.seed(1)
subset <- sample(nrow(Boston1), nrow(Boston1) * 0.7)
Bostontrain = Boston1[subset, ]
Bostontest = Boston1[-subset, ]
#modelo com variáveis com correlação maior que 0,6 com medv0
#LDA
Bostontrain.lda.fit<-lda(crim0~indus+nox+age+dis+rad+tax,data=Bostontrain)
Bostontest.lda.predict=predict(Bostontrain.lda.fit,Bostontest)
Bostontest.lda.class=Bostontest.lda.predict$class
table(Bostontest$crim0,Bostontest.lda.class,dnn = c("Actual Mileage", "Predicted Mileage"))
round(mean(Bostontest.lda.class!=Bostontest$crim0),3
      
      #QDA
      Bostontrain.qda.fit<-qda(crim0~indus+nox+age+dis+rad+tax,data=Bostontrain)
      Bostontest.qda.predict=predict(Bostontrain.qda.fit,Bostontest)
      Bostontest.qda.class=Bostontest.qda.predict$class
      table(Bostontest$crim0,Bostontest.qda.class,dnn = c("Actual Mileage", "Predicted Mileage"))
      round(mean(Bostontest.qda.class!=Bostontest$crim0),3)
      
      #Logistic regression
      Bostontrain.glm.fit<-glm(crim0~indus+nox+age+dis+rad+tax,data=Bostontrain,family = binomial)
      Bostontest.glm.predict=predict(Bostontrain.glm.fit,Bostontest,type="response")
      Bostontest.glm.class=ifelse(Bostontest.glm.predict>0.5,1,0)
      table(Bostontest$crim0,Bostontest.glm.class,dnn = c("Actual Mileage", "Predicted Mileage"))
      round(mean(Bostontest.glm.class!=Bostontest$crim0),3)