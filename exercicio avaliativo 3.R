# João Marcos 
# Atividade avaliativa 3 - Resampling


# 5 - A

install.packages("ISLR")
library(ISLR)
attach(Default)
set.seed(1)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)

#5(b)i

train <- sample(dim(Default)[1], dim(Default)[1] / 2)

#5(b)ii

fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)

summary(fit.glm)

#5(b)iii

probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")

pred.glm <- rep("No", length(probs))

pred.glm[probs > 0.5] <- "Yes"

mean(pred.glm != Default[-train, ]$default)


#Temos uma taxa de erro de teste de 2,86% com a abordagem do conjunto de validação.

#5(c)

train <- sample(dim(Default)[1], dim(Default)[1] / 2)

fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)

probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")

pred.glm <- rep("No", length(probs))

pred.glm[probs > 0.5] <- "Yes"

mean(pred.glm != Default[-train, ]$default)

# 0.0246

train <- sample(dim(Default)[1], dim(Default)[1] / 2)

fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)

probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")

pred.glm <- rep("No", length(probs))

pred.glm[probs > 0.5] <- "Yes"

mean(pred.glm != Default[-train, ]$default)

# 0.0276

train <- sample(dim(Default)[1], dim(Default)[1] / 2)

fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)

probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")

pred.glm <- rep("No", length(probs))

pred.glm[probs > 0.5] <- "Yes"

mean(pred.glm != Default[-train, ]$default)

# 0.0264

#Vemos que a estimativa de validação da taxa de erro de teste pode ser variável,
#dependendo precisamente de quais observações 
#são incluídas no conjunto de treinamento e quais observações são incluídas no conjunto de validação.

#5(d)

train <- sample(dim(Default)[1], dim(Default)[1] / 2)

fit.glm <- glm(default ~ income + balance + student, data = Default, family = "binomial", subset = train)

pred.glm <- rep("No", length(probs))

probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")

pred.glm[probs > 0.5] <- "Yes"

mean(pred.glm != Default[-train, ]$default)

## 0.0264

#Não parece que adicionar a variável dummy "estudante" conduza a
#uma redução na estimativa do conjunto de validação da taxa de erro de teste.

#6(a)

fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial")

summary(fit.glm)

#6(b)

library(boot)

boot.fn = function(data,index){
  coef(glm(default~income+balance,data=data,subset=index,family='binomial'))
}

set.seed(1)
boot.fn(Default,sample(1000,500,replace = T))

#6(c)

boot(Default,boot.fn,R=1000)

#Os erros padrão estimados obtidos pelos dois métodos são bem próximos.

#8(a)

set.seed(1)

y <- rnorm(100)

x <- rnorm(100)

y <- x - 2 * x^2 + rnorm(100)


#8(b)

plot(x, y)

#8(c)

library(boot)

set.seed(1)

Data <- data.frame(x, y)

fit.glm.1 <- glm(y ~ x)

cv.glm(Data, fit.glm.1)$delta[1]

## 5.890979

fit.glm.2 <- glm(y ~ poly(x, 2))

cv.glm(Data, fit.glm.2)$delta[1]

## 1.086596

fit.glm.3 <- glm(y ~ poly(x, 3))

cv.glm(Data, fit.glm.3)$delta[1]

## 1.102585

fit.glm.4 <- glm(y ~ poly(x, 4))

cv.glm(Data, fit.glm.4)$delta[1]

##  1.114772

#8(d)

set.seed(10)

fit.glm.1 <- glm(y ~ x)

cv.glm(Data, fit.glm.1)$delta[1]

## 5.890979

fit.glm.2 <- glm(y ~ poly(x, 2))

cv.glm(Data, fit.glm.2)$delta[1]

##  1.086596

fit.glm.3 <- glm(y ~ poly(x, 3))

cv.glm(Data, fit.glm.3)$delta[1]

##  1.102585

fit.glm.4 <- glm(y ~ poly(x, 4))

cv.glm(Data, fit.glm.4)$delta[1]

##  1.114772


#8(e)

## Podemos ver que a estimativa do LOOCV para o teste MSE é mínima para “fit.glm.2”, isto não é surpreendente, uma vez que vimos claramente em (b) que a relação entre “x” e “y” é quadrática.

#8(f)

summary(fit.glm.4)


#9(a)

library(MASS)

attach(Boston)

mu.hat <- mean(medv)

mu.hat

##22.53281

#9(b)

se.hat <- sd(medv) / sqrt(dim(Boston)[1])

se.hat

##0.4088611

#9(c)

set.seed(1)

boot.fn <- function(data, index) {
  
  mu <- mean(data[index])
  
  return (mu)
  
}

boot(medv, boot.fn, 1000)

#9(d)

t.test(medv)

CI.mu.hat <- c(22.53 - 2 * 0.4119, 22.53 + 2 * 0.4119)

CI.mu.hat

#9(e)

med.hat <- median(medv)

med.hat

#9(f)

boot.fn <- function(data, index) {
  
  mu <- median(data[index])
  
  return (mu)
  
}

boot(medv, boot.fn, 1000)

#9(g)

percent.hat <- quantile(medv, c(0.1))

percent.hat

#9(h)

boot.fn <- function(data, index) {
  
  mu <- quantile(data[index], c(0.1))
  
  return (mu)
  
}

boot(medv, boot.fn, 1000)

##Obtemos um valor percentil estimado de 12,75, que é novamente igual ao valor obtido em (g), 
#com um erro padrão de 0,5113, que é relativamente pequeno comparado ao valor percentual.



