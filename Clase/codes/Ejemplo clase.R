# Importamos los datos 
path <- 'C:/Users/Lenovo/Desktop/PSEI-ESTADISTICA Y CD/2DO CUATRIMESTRE/MODELOS LINEALES/CODIGOS CLASE/Datos.csv'
Datos <- read.csv(path, header = TRUE)
Datos
# Cambiamos a factores las variables que lo sean
Datos$NP <- as.factor(Datos$NP)
Datos$FP <- as.factor(Datos$FP)

# Importamos las librerías para encontrar la matriz diseño original
# y la biblioteca para operaciones matriciales
library(mvtnorm)
library(sasLM)
library(matlib)

# Matriz diseño sin reparametrización

X1 <- ModelMatrix(GP ~ NP + FP + NP*FP, Datos)
X <- X1$X

dim(X)
R(X)

# Vector de observaciones
Y <- Datos$GP

# Vector de parámetros estimados usando inversa generalizada de X'X
beta1 <- Ginv(t(X)%*%X)%*%t(X)%*%Y

Yhat1 <- X%*%beta1

# Modelo de regresión lineal con interacción
lm1 <- lm(GP ~ FP + NP + NP*FP, data = Datos)
summary(lm1)

# MAtrix con las variables reparametrizadas (Variables Dummy)
W <- as.matrix(model.matrix(lm1))
dim(W)

# parámetros usando la matriz reparametrizada
theta <- solve(t(W)%*%W)%*%t(W)%*%Y
theta

# Valores predichos 
Yhat2 <- W%*%theta
Yhat2

# Comparación de valores predichos con ambos métodos
cbind(Yhat1, Yhat2)

# Ver relación entre Beta y Theta

round(solve(t(W)%*%W)%*%t(W)%*%X, 1)

# Si conocemos el rango de W, se le pueden eliminar
# columnas a X hasta tener un rango igual para tener 
# una nueva matriz W
Wnueva <- X[,-c(3,6,9,10,11,12)]
R(Wnueva)

round(solve(t(Wnueva)%*%Wnueva)%*%t(Wnueva)%*%X, 1) 

# Restricción 






