path <- 'C:/Users/Lenovo/Desktop/PSEI-ESTADISTICA Y CD/2DO CUATRIMESTRE/MODELOS LINEALES/CODIGOS CLASE/ejerciciot3.csv'
Datos <- read.csv(path, header = TRUE)
Datos

# Cambiamos a factores las variables que lo sean
Datos$fertilizante <- as.factor(Datos$fertilizante)
Datos$variedad <- as.factor(Datos$variedad)

# Importamos las librerías para encontrar la matriz diseño original
# y la biblioteca para operaciones matriciales
library(mvtnorm)
library(sasLM)
library(matlib)

# Matriz diseño sin reparametrización
#------2) Matriz diseño
X1 <- ModelMatrix(rendimiento ~ fertilizante + variedad + fertilizante*variedad, Datos)
X <- X1$X
X

#----------3 Ecuanciones normales-----
#t(X)XB=t(X)Y
X_t<-t(X)%*%X
Y <- Datos$rendimiento
ec_2<-t(X)%*%Y
#-----4----Resuelva la ecuación normal usando inversa generalizada-------
# Vector de parámetros estimados usando inversa generalizada de X'X
beta1 <- Ginv(t(X)%*%X)%*%t(X)%*%Y
beta1
Yhat1 <- X%*%beta1
Yhat1

#-5-Reparametrizar el modelo y Escriba W y encuentra theta
# Modelo de regresión lineal con interacción
lm1 <- lm(rendimiento ~ fertilizante + variedad + fertilizante*variedad, data = Datos)
summary(lm1)
# MAtrix con las variables reparametrizadas (Variables Dummy)
W <- as.matrix(model.matrix(lm1))
dim(W)
# parámetros usando la matriz reparametrizada
theta <- solve(t(W)%*%W)%*%t(W)%*%Y
theta

Yhat2 <- W%*%theta
Yhat2



#-------------6-Encuentre otra reparametrización

lm2 <- lm(rendimiento ~ 0+ fertilizante + variedad + fertilizante*variedad, data = Datos)
summary(lm2)
# MAtrix con las variables reparametrizadas (Variables Dummy)
W2 <- as.matrix(model.matrix(lm2))
W2

# parámetros usando la matriz reparametrizada
theta2 <- solve(t(W2)%*%W2)%*%t(W2)%*%Y
theta2

Yhat3 <- W2%*%theta2
Yhat3

#---7--Para reparametrización con dummy indicar la relación 
#de β con θ
# Ver relación entre Beta y Theta
L<-round(solve(t(W)%*%W)%*%t(W)%*%X, 1)

#---8---Para cada solución, realice una predicción y compare----


estimados<-cbind(Yhat1,Yhat2,Yhat3)
