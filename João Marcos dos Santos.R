# João Marcos dos Santos Nepomuceno
# Neylson
# Exercicio avaliativo

# Ex 8

if (! "ISLR" %in% installed.packages()) install.packages("ISLR")
if (! "MASS" %in% installed.packages()) install.packages("MASS")
if (! "dplyr" %in% installed.packages()) install.packages("dplyr")
if (! "ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (! "readr" %in% installed.packages()) install.packages("readr")
if (! "texreg" %in% installed.packages()) install.packages("texreg")
library(readr)
library(dplyr)
library(texreg)
library(ggplot2)
library(ISLR)
library(MASS)

data("Auto")
head(Auto) # os primeiros casos
tail(Auto) # os ultimos casos
summary(Auto)

lm.fit <- lm(formula = "mpg ~ horsepower", data = Auto)
summary(lm.fit)
plot(lm.fit) 

# I- Os valores p para os coeficientes de regressão são quase zero. Isso implica em significância estatística,
# o que, por sua vez, significa que existe um relacionamento.
# II - O valor R ^ {2} indica que cerca de 61% da variação na variável de resposta (mpg) é devida à variável preditor (força por cavalos).
# III - coeficiente de regressão para "cavalos-força" é negativo. Portanto, o relacionamento é negativo.
# IV - O intervalo de confiança de 95%

# 8-B

lm.fit <- lm(formula = "mpg ~ horsepower", data = Auto)
plot(Auto$mpg ~ Auto$horsepower)
abline(lm.fit)

# ou

attach(Auto)
plot(mpg~horsepower, main =" MPG vs Horsepower", xlab = " Horsepower", ylab ="MPG")
abline(coef = coef(lm.fit), col ="red")


# 8-C

par(mfrow=c(2,2)) #4 figuras dispostas em 2 linhas e 2 colunas
plot(lm.fit)

# 9-A

pairs(Auto)

# 9-B

cor(Auto[, names(Auto) !="name"]) 
# Você pode usar a função cor () para produzir correlações

# 9-C 

model = lm(mpg ~. -name, data = Auto)
summary(model)

# jeito manual
reg1 = lm(mpg ~ horsepower, data = Auto)
reg2 =  lm(mpg ~ cylinders, data =  Auto)
reg3 = lm(mpg ~ weight, data = Auto)
reg4 = lm(mpg ~ acceleration, data = Auto)
reg5 = lm(mpg ~ displacement, data = Auto)
reg6 = lm(mpg ~ year, data = Auto)
reg7 = lm(mpg ~ origin, data = Auto)
reg_completa = lm(mpg ~ horsepower + cylinders + weight + acceleration+ displacement + year + orogin, data = Auto)
summary(reg1)
summary(reg2)
summary(reg3)
summary(reg4)
summary(reg5)
summary(reg6)
summary(reg7)
summary(reg_completa)
screenreg(list(reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg_completa))
# # Conversão da saída de regressão R
par(mfrow = c(2,2)) # divide a tela de plotagem em 2 colunas e 2 linhas
plot(reg_completa)
par(mfrow = c(1,1)) # mostra um grafico por vez
plot(reg_completa)

# I - Sim existe. No entanto, alguns preditores não têm um efeito estatisticamente significativo na resposta.
# O  valor de R-quadrado implica que 82% das mudanças na resposta podem ser explicadas pelos preditores neste modelo de regressão.

# II - displacement, weight, year, origin .

# III - O que sugere o coeficiente da variável ano? Quando cada outro preditor mantido constante,
# o valor de mpg aumenta a cada ano que passa. Especificamente, o mpg aumenta em 1,43 a cada ano.

# 9-D

par(mfrow = c(2,2))
plot(model)

# O primeiro gráfico mostra um padrão (em forma de U) entre os resíduos e os valores ajustados.
# Isso indica um relacionamento não linear entre as variáveis preditor e resposta.
# O segundo gráfico mostra que os resíduos são normalmente distribuídos.
# O terceiro gráfico mostra que a variância dos erros é constante. Finalmente,
# o quarto gráfico indica que não há pontos de alavancagem nos dados.

# 10-A

?Carseats
head(Carseats)
str(Carseats)
lm.fit = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit)

# 10-B

# I - Quando o preço aumenta em US $ 1.000 e outros preditores são mantidos constantes, as vendas diminuem em 54.459 unidades vendidas.
# Em outras palavras, quando o preço aumenta em $ 1000, o número de carros vendidos diminui em 54.459.
# II - A venda de uma loja não é afetada por estar ou não em uma área urbana.
# III - Uma loja nas vendas dos EUA 1200 mais carros (em média) do que uma loja que está no exterior.

# 10-C

# 10-D

#O preditor "Urbano". Seu valor de p não é estatisticamente significativo com um valor de 0,936.

# 10-E

lm.fit2 = lm(Sales ~ Price + US , data= Carseats)
summary(lm.fit2)

# 10-F

# Com base em seus respectivos valores de R-quadrado (em tabelas de resumo),
# esses dois modelos são medíocres (apenas 24% de mudança na resposta explicada).

# 10-G

confint(lm.fit2)

# 10-H

par(mfrow=c(2,2))
plot(lm.fit2)

# 13-A

X = rnorm(100, mean = 0, sd = 1)
X

# 13-B

eps = rnorm(100, mean = 0, sd = 0.25)
eps

# 13-C

y = -1 + 0.5 * X + eps
y
length(y)

# 13-D

plot(X ~ y)

# 13-E

lm.fit1 = lm(X ~ y)
summary(lm.fit1)

# β ^ o e β ^ 1 são praticamente iguais a βo e β1. Como esperado, β ^ o e β ^ 1 são estatisticamente signficantes
# e R-quadrado = 0,84 mostram que o modelo se ajusta muito bem aos dados.

# 13-F

plot(y ~ X); abline(lm.fit1, col="red")
legend("bottomright", c("Regression Line"), lwd=1, col = "red", bty = "n")

# 13-G

lm.fit1 = lm(y~poly(X,2))
summary(lm.fit1)

# o coeficiente de regressão do termo quadrático não é estatisticamente significativo; 
# Portanto, não há evidências de que o termo quadrático melhore o modelo.

# 13-H

X = rnorm(100, mean= 0, sd =1)
eps = rnorm(100, mean =0, sd = 0.10)
y = -1+0.5*X+eps
lm.fit2 = lm(y~X)
summary(lm.fit2)

plot(y~X); abline(lm.fit2, col ="red") 
legend("bottomright", c("Regression line"), lwd=1, col="red",bty ="n")

lm.fit2 = lm(y~poly(X,2))
summary(lm.fit2)

# R-quadrado = 0,95 mostra que este modelo se ajusta melhor aos dados.
# A observação em (1) é evidenciada graficamente por quão bem a linha de regressão se ajusta aos pontos de dados.
# O termo quadrático é, novamente, estatisticamente insignificante

# 13-I

X = rnorm(100, mean= 0, sd =1)
eps = rnorm(100, mean =0, sd = 0.50)
y = -1+0.5*X+eps
lm.fit3 = lm(y~X)
summary(lm.fit3)

plot(y~X); abline(lm.fit3, col ="red") 
legend("bottomright", c("Regression line"), lwd=1, col="red",bty ="n")

lm.fit3 = lm(y~poly(X,2))
summary(lm.fit3)

# R-quadrado = 0,4482 é muito pobre relativamente
# Graficamente, podemos ver que a linha de regressão não está ajustando muito bem os pontos de dados.
# A quadrática ainda é estatisticamente insignificante.

# 13-J

lm.fit1 = lm(y~X); lm.fit2 = lm(y~X); lm.fit3 = lm(y~X)
confint(lm.fit1);confint(lm.fit2); confint(lm.fit3)

# Obtemos os mesmos intervalos de confiança para os parâmetros βo e β1 de cada modelo.

# 15-A

# age, black, chas, crim, dis, indus, lstat, medv, nox, ptratio, rad, rm, tax, zn

library(MASS)
attach(Boston)

data(Boston)
head(Boston)
tail(Boston)
summary(Boston)
?Boston                  

reg1 <- lm(crim ~ age)                  
summary(reg1)                  
reg2 <- lm(crim ~ black)                  
summary(reg2)              
reg3 <- lm(crim ~ chas)
summary(reg3)
reg4 <- lm(crim ~ dis)
summary(reg4)
reg5 <- lm(crim ~ indus)
summary(reg5)
reg6 <- lm(crim ~ lstat)
summary(reg6)
reg7 <- lm(crim ~ medv)
summary(reg7)
reg8 <- lm(crim ~nox)
summary(reg8)
reg9 <- (crim ~ ptratio)
summary(reg9)
reg10 <- lm(crim ~ rad)
summary(reg10)
reg11 <- lm(crim ~rm)
summary(reg11)
reg12 <- lm(crim ~ tax)
summary(reg12)
reg13 <- lm(crim ~ zn)
summary(reg13)

pairs(Boston)

# n consegui fazer desse jeito 
# cor(Boston[, names(Boston) !="name"])
# model1 = lm(crim ~. -name, data = Boston)
# summary(model1)

# Pode-se notar a partir de todos os modelos lineares acima que não há evidências de que o chas e a idade 
# estejam associados ao crim, e há evidências de associação entre os preditores remanescentes e o crim.

# 15-B

reg_completa1 <- lm(crim ~ age + black + chas + dis + indus + lstat + medv + nox + ptratio + rad + rm + tax + zn, data = Boston)
summary(reg_completa1)

# outro jeito

lm.all <- lm(crim~.,data=Boston)
summary(lm.all)

# No nível de confiança de 5%, a hipótese nula pode ser rejeitada para os preditores; zn, dis, rad, black e medv.

# 15-C

# n consegui

xs <- c(coef(lm.indus)[2],coef(lm.zn)[2],coef(lm.chas)[2],coef(lm.nox)[2],coef(lm.rm)[2],coef(lm.age)[2],coef(lm.dis)[2],coef(lm.rad)[2],coef(lm.tax)[2],coef(lm.ptratio)[2],coef(lm.black)[2],coef(lm.lstat)[2],coef(lm.medv)[2])
ys <- coef(lm.all)[-1]

data.frame(xs,ys)
plot(xs, ys)

# feito

univcof <- lm(crim ~ zn, data = Boston)$coefficients[2]
univcof <- append(univcof, lm(crim ~ indus, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ chas, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ nox, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ rm, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ age, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ dis, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ rad, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ tax, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ ptratio, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ black, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ lstat, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ medv, data = Boston)$coefficients[2])
fooBoston <- (lm(crim ~ . - crim, data = Boston))
fooBoston$coefficients[2:14]
plot(univcof, fooBoston$coefficients[2:14], main = "Univariate vs. Multiple Regression Coefficients", 
     xlab = "Univariate", ylab = "Multiple")

# 15-D

lm.zn3 <- lm(crim~poly(zn,3))
summary(lm.zn3) # No evidence for cubic term.

lm.indus3 <- lm(crim~poly(indus,3))
summary(lm.indus3) # Evidence of non linearity.

# chas é um preditor qualitativo, o polinômio não faz sentido nesse caso.