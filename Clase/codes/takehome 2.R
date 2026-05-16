
#Modelos Lineales
#Examen

#Ejercicio 1-------------------------------------------------
#inciso a
library(MASS)
library(matlib)
#Inversa generalizada
X<-matrix(c(1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1),8,4)
X
XtX<-t(X)%*%X
XtX
invXtX<-Ginv(XtX);invXtX

#inciso b
R(X) 

c1<-cbind(0,1,-1,0)
Xc1<-rbind(X,c1)
R(Xc1)
R(X) 
#Es comprobable

c2<-c(0,1,-2,3)
Xc2<-rbind(X,c2)
R(Xc2)
R(X)
#NO es comprobable

c3<-c(0,0,0,1)
Xc3<-rbind(X,c3)
R(Xc3) 
R(X)
#NO es comprobable

c4<-c(1,0,0,0)
Xc4<-rbind(X,c4)
R(Xc4)
R(X)
#NO es comprobable

c5<-matrix(c(0,1,0,-1,0,1,-2,1),2,4,byrow = T)
Xc5<-rbind(X,c5)
R(Xc5) 
R(X)
#SI es comprobable

c6<-matrix(c(0,1,-1,0,0,1,0,-1,0,2,-1,-1),3,4,byrow = T)
Xc6<-rbind(X,c6)
R(Xc6) 
R(X)
#SI es comprobable

#inciso c
R<-matrix(c(0,1,0,-1,0,1,-2,1),2,4,T);R
R(rbind(X,R))
#inciso b1
A<-diag(8) - X%*%invXtX%*%t(X)
A
R(A)
#simetrica
t(A) 
simA<-all.equal(A,t(A))
simA
#Prueba de idempotencia
A22<-A%*%A 
Idem_A2 = all.equal(A, A22)
Idem_A2


#inciso b2

R <- matrix(c(0,1,0,-1,0,1,-2,1),2,4,T);R
A <- diag(8) - X%*%invXtX%*%t(X)
B <- X%*%invXtX%*%t(R)%*%solve(R%*%invXtX%*%t(R))%*%R%*%invXtX%*%t(X)
B%*%A

solve(R%*%invXtX%*%t(R))


#inciso b3

R <- matrix(c(0,1,0,-1,0,1,-2,1),2,4,T);R
R(X)
R(rbind(X,R))


A1 <- solve(R%*%invXtX%*%t(R))
R(A1)


#inciso b4
t(R)%*%solve(R%*%invXtX%*%t(R))%*%R

#inciso e
W<-matrix(c(1,1,1,1,1,1,1,1,0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1),8,3)
W
#relación de beta con theta
solve(t(W)%*%W)%*%t(W)%*%X

c<-matrix(c(0,1,0,0,0,1),2,3,byrow = T);c
R(W) 
R(rbind(W,c)) 
#Si es comprobable







############## Problema 2 Regresión lineal segmentada------------

#La idea básica detrás de la regresión lineal por partes es que si los datos siguen diferentes tendencias lineales en diferentes regiones de los datos, entonces debemos modelar la función de regresión en “partes”. Las partes pueden estar conectadas o no conectadas. Aquí, tenemos un modelo en el que las piezas están conectadas.

#Una empresa de electrónica importa periódicamente envíos de una determinada pieza importante que utiliza como componente en varios de sus productos. El tamaño del envío varía según los programas de producción. Para el manejo y distribución a plantas de ensamblaje, los envíos de 250 mil piezas o menos se envían al almacén A; Los envíos más grandes se envían al almacén B, ya que este almacén cuenta con equipo especializado que proporciona mayores economías de escala para envíos grandes.

#El conjunto de datos contiene información sobre el costo $y$ de manejar el envío en el almacén (en miles de dólares) y el $x_1$ tamaño del envío (en miles de partes).







## Inciso a)

#Grafique los datos ¿parece razonable que haya dos funciones de regresión diferentes (pero conectadas): una cuando y otra cuando? tenemos un punto de cambio en $x_1 < 250$?.


datos=data.frame(y=c(11.95,14.13,8.93,10.98,10.03,10.13,13.75,13.3,15,7.97),
                 x1=c(225,350,150,200,175,180,325,290,400,125))
plot(datos$x,datos$y,xlab = "tamaño de envío", ylab="costos")
abline(v=250,col="blue",lwd=1,lty=2)


#Sí parece razonable que haya dos funciones de regresión diferentes (pero conectadas): una cuando el tamaño de envío es menor que 250 piezas $(x_1<250)$ y otra cuando el tamaño de envío es mayor que 250 piezas $(x_1>250)$. Las pendientes son diferentes en ambas regiones; la pendiente es menos pronunciada cuando el tamaño de envío es mayor que 250 piezas, indicando que el costo de envío por unidad (en miles de partes) empieza a disminuir.





## Inciso b)

#Usando una variable ficticia o dummy y un término de interacción para ayudar a definir el modelo de regresión lineal por partes. Específicamente, el modelo que ajustaremos es: $$y_i = \beta_0 + \beta_1 x_{i1} + \beta_2 (x_{i1} − 250) x_{i2} + \epsilon_i$$ Dónde $x_{i1}$ es el tamaño del envío y $x_{i2} = 0$ si $x_{i1} < 250$ y $x_{i2} = 1$ si $x_{i1} > 250$. Si suponemos que los datos siguen este modelo, ¿cuál es la función de respuesta media para envíos cuyo tamaño es menor que 250? ¿Y cuál es la función de respuesta media para envíos cuyo tamaño es mayor que 250? ¿Las dos funciones de respuesta media tienen pendientes diferentes y se conectan cuando $x_{i1} = 250$. $$\mu_Y = \beta_0 + \beta_1x_1 + \beta_2(x_1 − 250)x_2$$
  

  
#  Función de respuesta media para envíos cuyo tamaño es menor que 250: $$ \mu_Y \mid (x_1<250)= \beta_0 + \beta_1x_1 + \beta_2(x_1 − 250)(0)=\beta_0 + \beta_1x_1$$
  
#  Función de respuesta media para envíos cuyo tamaño es mayor que 250: $$ \mu_Y \mid (x_1>250)= \beta_0 + \beta_1x_1 + \beta_2(x_1 − 250)(1)=(\beta_0-250\beta_2) + (\beta_1+\beta_2)x_1$$
  
#  Como puede observarse, las dos funciones de respuesta media tienen pendientes diferentes:
  
#-   La pendiente de la función de respuesta media para envíos cuyo tamaño es menor que 250 es: $\beta_1$.

#-   La pendiente de la función de respuesta media para envíos cuyo tamaño es mayor que 250 es: $\beta_1+\beta_2$.

#Como puede observarse, las dos funciones de respuesta media se conectan cuando $x_{1} = 250$:
  
#-   De la primera función se tiene: $\mu_Y \mid (x_1=250)=\beta_0 + \beta_1x_1=\beta_0 + 250\beta_1$.

#-   De la segunda función se tiene: $\mu_Y \mid (x_1=250)=(\beta_0-250\beta_2) + (\beta_1+\beta_2)x_1=(\beta_0-250\beta_2) + (\beta_1+\beta_2)250=\beta_0 +250 \beta_1$.









## Inciso c)

#Obtenga la matrix diseño y encuentre el estimador de MCO.

#Se calcula la variable ficticia ($x_2$) y un término de interacción ($x_2^*$).

datos$x2=ifelse(datos$x1<250,0,1)
datos$x2s=(datos$x1-250)*datos$x2
datos

library(matlib)
#Matriz diseño:
library(mvtnorm)
library(sasLM)
X1=ModelMatrix(y~x1+x2s,datos)
X<-X1$X
X

#Ajuste del modelo:
m1=lm(y~x1+x2s,datos)
summary(m1)


# # Vector de observaciones
# Y <- datos$y
# # Vector de parámetros estimados usando inversa generalizada de X'X
# beta1 <- Ginv(t(X)%*%X)%*%t(X)%*%Y
# Yhat1 <- X%*%beta1
# # Suma de cuadrado del error
# SCE <- t(Y - X%*%beta1)%*%(Y - X%*%beta1)
# # Grados de libertad
# gle <- nrow(X) - R(X)
# # Estimación de sigma cuadrada
# CME <- SCE/gle






## Inciso d) 

#Según su función de regresión estimada, ¿cuál es el costo previsto para un envío con un tamaño de 125? un tamaño de 250? con un tamaño de 400? Convénzase de que obtendrá la misma predicción para tamaño = 250 independientemente de la función de regresión estimada que utilice. 

x1=c(125,250,400)
x2s=ifelse(x1-250>0,x1-250,0)
X=cbind(1,x1,x2s)
yhat=X%*%m1$coefficients
cbind(x1,yhat)

#Función de respuesta media para envíos cuyo tamaño es menor que 250: $$ \mu_Y \mid (x_1<250)= \beta_0 + \beta_1x_1 + \beta_2(x_1 − 250)(0)=\beta_0 + \beta_1x_1=3.21393+0.03846*x_1$$
  
#Función de respuesta media para envíos cuyo tamaño es mayor que 250: $$ \mu_Y \mid (x_1>250)= \beta_0 + \beta_1x_1 + \beta_2(x_1 − 250)(1)=(\beta_0-250\beta_2) + (\beta_1+\beta_2)x_1=9.40643+0.01369x_1$$
  
#-   $\hat{y} \mid (x_1=125)=3.21393 +0.03846*125=8.0214$.

#-   usando la primera función se tiene: $\hat{y} \mid  (x_1=250)=3.21393 +0.03846*250=12.8289$.

#-   usando la segunda función se tiene: $\hat{y} \mid (x_1=250)=9.40643+0.01369*250=12.8289$.

#- $\hat{y} \mid (x_1=400)=9.40643+0.01369*400=14.8824$.








## Inciso e). 

# Grafique sus ecuaciones estimadas sobre la gráfica de dispersión de datos y anote sus predicciones en d.

plot(y~x1,datos,xlab = "tamaño de envío", ylab="costos")
x1=c(min(datos$x1),250,max(datos$x1))
x2s=ifelse(x1-250>0,x1-250,0)
X<-cbind(1,x1,x2s)
lines(x1,as.vector(X%*%m1$coefficients),col="red",lwd=2)
abline(v=250,col="blue",lwd=1,lty=2)
points(x=125, y=8.0214,type="p", pch=20, col="green")
points(x=250, y=12.8289,type="p", pch=20, col="green")
points(x=400, y=14.8824,type="p", pch=20, col="green")


## inciso f) intervalo del 95% de credibilidad para B_2
library(MCMCpack)
library(tidyverse)
library(brms)
library("tidymodels")
library(broom.mixed)
library(broom)

# costos.mcmcpack = MCMCregress(y~x1+x2s, data = datos)
# summary(costos.mcmcpack)
# 
# 
# knitr::kable(tidyMCMC(costos.mcmcpack, conf.int = TRUE, conf.method = "HPDinterval"))
# raftery.diag(costos.mcmcpack)


library(brms)
library(bayestestR)


m2 <- brm(y~x1+x2s, data = datos)
print(m2, digits = 6)
print(describe_posterior(m2, centrality = "mean")[1:8], digits = 6)
plot(m2)

# Obtenemos las muestras de la distribución posterior
posterior_samp <- posterior_samples(m2)
# Calcular beta_2:
posterior_samp$indice <- posterior_samp$b_x2s

#Intervalo de credibilidad del 95%:
cred_interval <- quantile(posterior_samp$indice, c(0.025, 0.975))
# Resultados
knitr::kable(c(mean = mean(posterior_samp$indice), cred_interval))
plot(cred_interval)


#----------problema 3---------------

library(matlib)
library(MatrixModels)
library(readxl)
library(dplyr)
library(mvtnorm)

path <- 'C:/Users/Lenovo/Desktop/PSEI-ESTADISTICA Y CD/2DO CUATRIMESTRE/MODELOS LINEALES/CODIGOS CLASE/datos_3.csv'

datos3 <- read.csv(path, header = TRUE)

glimpse(datos3)
datos3$tallos<-as.numeric(datos3$tallos)
datos3$sitios<-as.factor(datos3$sitios)
datos3$trat<-as.factor(datos3$trat)
datos3$espacio<-as.numeric(datos3$espacio)
datos3$pendiente<-as.factor(datos3$pendiente)
datos3$drenaje<-as.factor(datos3$drenaje)
datos3$dosel<-as.numeric(datos3$dosel)
unique(datos3$dosel)
unique(datos3$espacio)
library(leaps)
library(MASS)
library(sasLM)
#MODELO CON TODAS LAS VARIABLES (prueba stepAIC 
#para elegir el mejor modelo)
model_completo<-lm(tallos~trat*sitios*espacio*pendiente*drenaje, data = datos3)
elecc<-stepAIC(model_completo)

#De metodo de seleccion se obtiene
tallos ~ trat + sitios + pendiente + trat:sitios + trat:pendiente
Step:  AIC=338.6

#Se corre el codigo 
modelo1<- lm(tallos ~ trat + sitios + pendiente + trat:sitios + trat:pendiente, data = datos3)
anova(modelo1)
summary(modelo1)
#en el anova se observa que la pendiente y la interaccion tratamiento pendiente 
#no son significativas y se quitan de modelo

#Se analiza mejor modelo
mf<-lm(formula = tallos ~ trat *sitios, data = datos3)
anova(mf)
summary(mf)

m1<- ModelMatrix(tallos ~ trat *sitios, datos3)
X<- as.matrix(m1$X)
X
dim(X)
Re<- datos3$tallos
Re
dim(Re)
#Vector de betha
Beta1<-Ginv(t(X)%*%X)%*%t(X)%*%Re
Beta1
#Se pruebas los efectos 
summary(mf)
anova(mf,)
library(agricolae)
pruebatukey<- HSD.test(mf, c("sitios", "trat"), group = T)
pruebatukey
pruebatukey$groups

plot(mf)
bar.group(x = pruebatukey$groups, 
          ylim=c(0,120),
          main="Grupos de comportamiento",
          xlab="Combinaciones",
          ylab="Tallos regenerados",
          col="blue")
















###############################################
modelolineal <- lm(tallos ~ trat*sitios*pendiente, data = datos3)
summary(modelolineal)
anova(modelolineal)
AIC(modelolineal)

modelo<-model.Matrix(tallos~sitios+trat+espacio+pendiente+drenaje+dosel, data = datos3)
modelo

plot(modelo,)

#Modelo con interacciones
modelolineal <- lm(tallos~sitios*trat, data = datos3)
summary(modelolineal)
anova(modelolineal)
AIC(modelolineal)

ml6=lm(formula = tallos ~ trat + sitios + pendiente + trat:sitios + 
         trat:pendiente, data = datos3)
AIC(ml6)
summary(ml6)
anova(ml6)

modelo<-model.Matrix(tallos ~ trat + sitios + pendiente + trat:sitios + 
                       trat:pendiente, data = Dataset)
modelo
anova(modelo)


library(agricolae)
prueba <- HSD.test(modelolineal, c("sitios", "trat"), group = T)
prueba


dcar<-aov(peso~Tmt,data=rhizobium)
TukeyHSD(modelolineal, "Tmt", ordered = TRUE)
HSD <- HSD.test(datos3, "Tmt", group = TRUE)
with(rhizobium, HSD)


