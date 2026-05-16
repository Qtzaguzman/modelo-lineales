library(mvtnorm)
library(mnormt)
x <- seq(-8, 8, 0.25)
y <- seq(-8, 8, 0.25)
mu=c(1,0)
sigma=matrix(c(8,-2,-2,7),ncol=2,byrow=T)
f <- function(x, y) dmnorm(cbind(x,y),mu,sigma)
z <- outer(x, y, f)
persp(x,y,z,theta=-30,phi=25,shade=0.75,col="purple",expand=0.5, r=2,ltheta=25)

muy=1
mux=matrix(c(0,-1),ncol=1,byrow = T)
sigmayy=8
sigmayx=matrix(c(-2,1),ncol=2,byrow = T)
sigmaxy=matrix(c(-2,1),ncol=1,byrow = T)
sigmaxx=matrix(c(7,3,3,5),ncol=2,byrow = T)
sigmayx%*%solve(sigmaxx)
cov=sigmayy-sigmayx%*%solve(sigmaxx)%*%sigmaxy
cov

a=matrix(c(1,1,2),ncol=1)
mu=matrix(c(1,0,-1),ncol=1)
sigma=matrix(c(8,-2,1,-2,7,3,1,3,5),ncol=3,byrow=T)
t(a)%*%mu
#Varianza 
t(a)%*%sigma%*%a

pnorm(1.5, mean = -1, sd = sqrt(47))
a=matrix ( c(3 , -2 , -2 ,0) , nrow = 2 , ncol = 2 )
e=eigen(A)
e
# Definir la funcion cuadratica Q(x, y)
Q <- function(x, y) {3 * x^2 - 4 * x * y}
x <- seq(-10, 10, length.out = 100)
y <- seq(-10, 10, length.out = 100)
z <- outer(x, y, Q)
persp(x,y,z,theta = 30, phi=30, col="pink",expand=0.5, r=2,ltheta=25)


library(matlib)


#1-Matriz diseño
res<- c(2,1,0,7,9,3)
trat<-c(1,1,1,2,2,3)
datos<-data.frame(res,trat)
datos$trat<-as.factor(datos$trat)
modelo<-lm(res~0+trat, datos)
W<-as.matrix(model.matrix(modelo))
R(W)

#2-Matriz proyectora
Pw <- W%*%solve(t(W)%*%W)%*%t(W)
Pw

#3-Proyección ortogonal
y <- datos$res
y_est <- Pw%*%y
y_est

#4 Proyector M
I <- diag(x=1, 6, 6)
Mw <- I - Pw
Mw
#vector ortogonal y
e_est <- round(Mw%*%y, 1)
e_est

#5-Normas
norma_2y <- t(y)%*%y
norma_2y
norma_2y_est <- t(y_est)%*%y_est
norma_2y_est
norma_2e_est <- t(e_est)%*%e_est
norma_2e_est

#6
summary(modelo)

