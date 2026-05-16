#---------------EJERCICI1-------------
install.packages('mvtnorm')
library(mvtnorm)
library(sasLM)
library(matlib)

#Se indica la direccion del conjunto de datos

path <- 'C:/Users/Lenovo/Desktop/PSEI-ESTADISTICA Y CD/2DO CUATRIMESTRE/MODELOS LINEALES/CODIGOS CLASE/tarea3_3.csv'
datos <- read.csv(path, header = TRUE)



datos$suplement<-as.factor(datos$suplement)
#Matriz X
model=ModelMatrix(peso~suplement,datos)
X=as.matrix(model$X)

dim(X) 
#Rango de la matriz X
qr(X)$rank
#Se construyen las lambas
lambda1<-c(0,3,-2,-2,-2,3)
lambda2<-c(0,0,1,-2,1,0)
lambda3<-c(0,0,1,0,-1,0)
lambda4<-c(0,1,0,0,0,-1)
#matrices aumentadas y rango
X1=rbind(X,t(lambda1))
qr(X1)$rank

X2=rbind(X,t(lambda2))
qr(X2)$rank

X3=rbind(X,t(lambda3))
qr(X3)$rank

X4=rbind(X,t(lambda4))
qr(X4)$rank
#Dado que el rango de la matriz es siempre 5, entonces es estimable.


#Se observa que el rango de la matriz X es igual a 5, y que en todas las CL, el rango de la nueva matriz (matriz aumentada), se mantiene igual, por lo que todas las CL, son estimables. Se procede a calcular la estimación de cada CL.		

#-----------
Y=as.vector(datos$peso)
#Calcular estimadores usando inversa generalizada
beta_est=Ginv(t(X)%*%X)%*%t(X)%*%Y
beta_est
efecto_cuadratico_est_1=lambda1%*%beta_est
efecto_cuadratico_est_1

efecto_cuadratico_est_2=lambda2%*%beta_est
efecto_cuadratico_est_2

efecto_cuadratico_est_3=lambda3%*%beta_est
efecto_cuadratico_est_3

efecto_cuadratico_est_4=lambda4%*%beta_est
efecto_cuadratico_est_4

#----------------------EJERCICIO1------------------------------------------

# Importamos las librerías para encontrar la matriz diseño original
# y la biblioteca para operaciones matriciales
library(mvtnorm)
library(sasLM)
library(matlib)

path <- 'C:/Users/Lenovo/Desktop/PSEI-ESTADISTICA Y CD/2DO CUATRIMESTRE/MODELOS LINEALES/CODIGOS CLASE/ejerciciot1.csv'
Datos <- read.csv(path, header = TRUE)
Datos

#a

# Cambiamos a factores las variables que lo sean
Datos$A <- as.factor(Datos$A)
Datos$B <- as.factor(Datos$B)

# Matriz diseño sin reparametrización
#- Matriz diseño
X1<-ModelMatrix(Respuesta~A+B,Datos)
#X<-as.matrix(X1$X)
X <- X1$X
X
dim(X)
R(X)


#---b Ecuanciones normales-----
#t(X)XB=t(X)Y
X_t<-t(X)%*%X
Y<-Datos$Respuesta
ec_2<-t(X)%*%Y

#c
#beta1<-Ginv(X_t)%*%ec_2 
#beta1
#-----Resuelva la ecuación normal usando inversa generalizada-------
# Vector de parámetros estimados usando inversa generalizada de X'X
beta1 <- Ginv(t(X)%*%X)%*%t(X)%*%Y
beta1
#Yhat1 <- X%*%beta1
#Yhat1



#d

r1<-c(0,1,1,1,0,0,0,0)
r2<-c(0,0,0,0,1,1,1,1)
X2<-rbind(X,r1,r2)
X2
R(X2)

Xnueva<-cbind(rbind(t(X)%*%X,r1,r2),c(r1,0,0),c(r2,0,0))
Xnueva
ec_3<-rbind(t(X)%*%Y,0,0)
ec_3
beta2<-solve(Xnueva)%*%ec_3
beta2


#-e---Reparametrizar el modelo y Escriba W, R(W) y encuentra theta
#identifique relación de tetha con beta
# Modelo de regresión lineal
lm1 <- lm(Respuesta ~ A+B, data = Datos)
# MAtrix con las variables reparametrizadas (Variables Dummy)
W <- as.matrix(model.matrix(lm1))
dim(W)
R(W)
# parámetros usando la matriz reparametrizada
theta <- solve(t(W)%*%W)%*%t(W)%*%Y
theta

#Yhat2 <- W%*%theta
#Yhat2

theta_Rel<-round(solve(t(W)%*%W)%*%t(W)%*%X,1)


#f
echelon(X)

#g

lambda<-c(1,1,0,0,0,1,0,0)
X_lam<-rbind(X,lambda)
R(X)
R(X_lam)

#h

lambda1<-c(1,1,0,0,1/4,1/4,1/4,1/4)
X_lam1<-rbind(X,lambda1)
R(X_lam1)
#Es estimable
lambda2<-c(1,0,1,0,1/4,1/4,1/4,1/4)
X_lam2<-rbind(X,lambda2)
R(X_lam2)
#Es estimable
lambda3<-c(1,0,0,1,1/4,1/4,1/4,1/4)
X_lam3<-rbind(X,lambda3)
R(X_lam3)
#Es estimable

#Estimadores
est1<-lambda1%*%beta1
est1

est2<-lambda2%*%beta1
est2

est3<-lambda3%*%beta1
est3










