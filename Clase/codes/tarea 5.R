#---------------2---------------
library(matlib)
# EJERCICIO 2 
# Definimos las matrices B1, B2, B3 y B4:
B1 <-(1/6)*matrix(data = c(2, 2, -1, -1, -1, -1, 2, 2, -1, -1, -1, -1,
                           -1, -1, 2, 2, -1, -1, -1, -1, 2, 2, -1, -1,
                           -1, -1, -1, -1, 2, 2, -1, -1, -1, -1, 2, 2), nrow = 6, byrow = T)
B2 = (1/6)*matrix(c(1, -1, 1, -1, 1, -1, -1, 1, -1, 1, -1, 1, 1, -1, 1,
                    -1, 1, -1, -1, 1, -1, 1, -1, 1, 1, -1, 1, -1, 1, -1,
                    -1, 1, -1, 1, -1, 1), nrow = 6, byrow = T)
B3 = (1/6)*matrix(c(2, -2, -1, 1, -1, 1, -2, 2, 1, -1, 1, -1, -1, 1, 2,
                    -2, -1, 1, 1, -1, -2, 2, 1, -1, -1, 1, -1, 1, 2, -2,
                    1, -1, 1, -1, -2, 2), nrow = 6, byrow = T)
B4 = (1/6)*matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                    1, 1), nrow = 6, byrow = T)
I6 = matrix(c(1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,
              0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1), ncol = 6, byrow = T)

#Para probar simetria A= AT
B1t = t(B1)
B1t
B1
B2t = t(B2)
B2t
B2
B3t = t(B3)
B3t
B3
B4t = t(B4)
B4t
B4

#Idempotencia
B12=B1%*%B1
Idem_B1 = all.equal(B1, B12)
Idem_B1

B22=B2%*%B2
Idem_B2 = all.equal(B2, B22)
Idem_B2

B32=B3%*%B3
Idem_B3 = all.equal(B3, B32)
Idem_B3
B42=B4%*%B4
Idem_B4 = all.equal(B4, B42)
Idem_B4


R(B1)
R(B2)
R(B3)
R(B4)
B=B1+B2+B3+B4
R(B)


RB=qr(B)$rank
RB

# EJERCICIO 3_1 ------------------------
library(mvtnorm)
library(sasLM)
library(matlib)

path <- 'C:/Users/Lenovo/Desktop/PSEI-ESTADISTICA Y CD/2DO CUATRIMESTRE/MODELOS LINEALES/CODIGOS CLASE/datos31_5.csv'
datos31 <- read.csv(path, header = TRUE)
datos31$Fuente <- as.factor(datos31$Fuente)
# Obtener la matriz diseño

modelo31 = ModelMatrix(Peso ~ Fuente, datos31)
X = as.matrix(modelo31$X)
y = as.matrix(datos31$Peso)

# Inciso a) 

C1 <- matrix( c(0, 1, -1,  0,  0,  0,
                0, 1,  0, -1,  0,  0,
                0, 1,  0,  0, -1,  0,
                0, 1,  0,  0,  0, -1), nrow=4, byrow=TRUE)

R(X)
X_C=rbind(X,C1)
R(X_C)

# La hipótesis es comprobable

# Cálculo de esadistico de prueba y valor de tablas (Metodo 1)

beta <- Ginv(t(X)%*%X)%*%t(X)%*%y
SCH <- t(C1%*%beta)%*%solve(C1%*%Ginv(t(X)%*%X)%*%t(C1))%*%(C1%*%beta)
glh <- R(C1%*%Ginv(t(X)%*%X)%*%t(C1))
SCE <- t(y)%*%(diag(nrow(X))-X%*%Ginv(t(X)%*%X)%*%t(X))%*%y
gle <- R(diag(nrow(X)))-R(X%*%Ginv(t(X)%*%X)%*%t(X))
FC <- (SCH/glh)/(SCE/gle)
FC                                         # F calculada

alfa=0.05
qf((1-alfa), df1=glh, df2=gle)             ## valor de tablas F

pf(FC, df1=glh, df2=gle, lower.tail = F)   ## Pvalue

# Prueba de hipotesis con ANOVA (Metodo 2)

library(carData)
library(car)
GLM(Peso ~ Fuente, datos31)$`Type III`

# Inciso b) 

C2 <- matrix( c(0, 3, -2, -2, -2,  3,
                0, 0,  1, -2, -1,  0,
                0, 0, -1,  0,  1,  0,
                0, 1,  0,  0,  0, -1), nrow=4, byrow=TRUE)

R(X)
X_C2=rbind(X,C2)
R(X_C2)

# La hipótesis no es comprobable

# Cálculo de esadistico de prueba y valor de tablas 

beta <- Ginv(t(X)%*%X)%*%t(X)%*%y
SCH2 <- t(C2%*%beta)%*%solve(C2%*%Ginv(t(X)%*%X)%*%t(C2))%*%(C2%*%beta)
glh2 <- R(C2%*%Ginv(t(X)%*%X)%*%t(C2))
SCE2 <- t(y)%*%(diag(nrow(X))-X%*%Ginv(t(X)%*%X)%*%t(X))%*%y
gle2 <- R(diag(nrow(X)))-R(X%*%Ginv(t(X)%*%X)%*%t(X))
FC2 <- (SCH2/glh2)/(SCE2/gle2)
# F calculada
FC2                                        

alfa=0.05
## valor de tablas F
qf((1-alfa), df1=glh2, df2=gle2)             
## Pvalue
pf(FC2, df1=glh2, df2=gle2, lower.tail = F)   

# Inciso c)

C <- matrix(c(0,2,2,2,-3,-3))

R(X)
X_C3=rbind(X,t(C))
R(X_C3)             
# La hipotesis es comprobable
# Prueba de hipotesis con la prueba F

beta <- Ginv(t(X)%*%X)%*%t(X)%*%y
SCH3 <- t(t(C)%*%beta)%*%solve(t(C)%*%Ginv(t(X)%*%X)%*%C)%*%(t(C)%*%beta)
glh3 <- R(t(C)%*%Ginv(t(X)%*%X)%*%C)
SCE3 <- t(y)%*%(diag(nrow(X))-X%*%Ginv(t(X)%*%X)%*%t(X))%*%y
gle3 <- R(diag(nrow(X)))-R(X%*%Ginv(t(X)%*%X)%*%t(X))
FC3 <- (SCH3/glh3)/(SCE3/gle3)
# F calculada
FC3                                        

alfa=0.05
# valor de tablas F
qf((1-alfa), df1=glh3, df2=gle3)             
# Pvalue
pf(FC3, df1=glh3, df2=gle3, lower.tail = F)   



#----------ejercicio3_2----------------
path <- 'C:/Users/Lenovo/Desktop/PSEI-ESTADISTICA Y CD/2DO CUATRIMESTRE/MODELOS LINEALES/CODIGOS CLASE/datos3_5.csv'
datos3 <- read.csv(path, header = TRUE)

library(mvtnorm)
library(sasLM)
library(matlib)

datos3$A=as.factor(datos3$A)
datos3$B=as.factor(datos3$B)
y=as.matrix(datos3$Respuesta)
X=ModelMatrix(Respuesta~A+B,datos3)$X


#Es comprobable
R(X)
C1<-c(0,1,-1,0,0,0,0,0)
C2<-c(0,1,0,-1,0,0,0,0)
C3<-c(0,1,0,0,-1,0,0,0)
C<-rbind(C1,C2,C3)
# C=matrix(c(0,1,-1,0,0,0,0,0,
#            0,1,0,-1,0,0,0,0,
#            0,1,0,0,-1,0,0,0),nrow=3,byrow=T)
X_R=rbind(X,C)
R(X_R)



beta=Ginv(t(X)%*%X)%*%t(X)%*%y
#C%*%beta
SCH=t(C%*%beta)%*%solve(C%*%Ginv(t(X)%*%X)%*%t(C))%*%(C%*%beta)
glh=R(C%*%Ginv(t(X)%*%X)%*%t(C))
SCE=t(y)%*%(diag(nrow(X))-X%*%Ginv(t(X)%*%X)%*%t(X))%*%y
gle=R(diag(nrow(X)))-R(X%*%Ginv(t(X)%*%X)%*%t(X))
# F calculada
FC=(SCH/glh)/(SCE/gle)
FC 
# valor de tablas F
alfa=0.05
qf((1-alfa), df1=glh, df2=gle)
# Pvalue
pf(FC, df1=glh, df2=gle, lower.tail = F)




## b). Usando una prueba de F general pruebe que $H_0 : \beta_1 = \beta_2 = \beta_3$ es comprobable y pruebe $H_0$ con un nivel de significancia del 5%.

R(X)
C1<-c(0,0,0,0,0,1,-1,0)
C2<-c(0,0,0,0,0,1,0,-1)
C<-rbind(C1,C2)
# C=matrix(c(0,0,0,0,0,1,-1,0,
#            0,0,0,0,0,1,0,-1),nrow=2,byrow=T)
X_R=rbind(X,C)
R(X_R)




beta=Ginv(t(X)%*%X)%*%t(X)%*%y
#C%*%beta
SCH=t(C%*%beta)%*%solve(C%*%Ginv(t(X)%*%X)%*%t(C))%*%(C%*%beta)
glh=R(C%*%Ginv(t(X)%*%X)%*%t(C))
SCE=t(y)%*%(diag(nrow(X))-X%*%Ginv(t(X)%*%X)%*%t(X))%*%y
gle=R(diag(nrow(X)))-R(X%*%Ginv(t(X)%*%X)%*%t(X))
# F calculada
FC=(SCH/glh)/(SCE/gle)
FC 
# valor de tablas F
alfa=0.05
qf((1-alfa), df1=glh, df2=gle)
# Pvalue
pf(FC, df1=glh, df2=gle, lower.tail = F)






## e). Usando modelo completo $y_{ijk} = \mu + \alpha_i + \epsilon_{ijk}$ y reducido $y_{ijk} = \mu + \epsilon_{ijk}$ Pruebe la hipótesis $H_0 : \alpha_1 = \alpha_2 = \alpha_3 = \alpha_4$ usando la f parcial, compare con el inciso a.

#Partición
Xc = X[, 1:5] 
X1 = X[, 1] 

# Suma de cuadrados de Xc ajustada por X1:
SCX_cX1 = t(y)%*%(Xc%*%Ginv(t(Xc)%*%Xc)%*%t(Xc)-X1%*%Ginv(t(X1)%*%X1)%*%t(X1))%*%y
glX_cX1 = qr(Xc)$rank-qr(X1)$rank

# Suma de cuadrados del error del modelo completo
SCE = t(y)%*%(diag(nrow(Xc)) - Xc%*%Ginv(t(Xc)%*%Xc)%*%t(Xc))%*%y
glE = nrow(Xc)-qr(Xc)$rank

# Cuadrados Medios:
CMX_cX1 = SCX_cX1/glX_cX1
CME = SCE/glE

# F calculada
FC=CMX_cX1 / CME
FC
# F de tablas 
alpha = 0.05
qf(1-alpha, glX_cX1, glE)
# Pvalue
pf(FC, df1=glX_cX1, df2=glE, lower.tail = F)





## f). Obtenga la sumas de cuadrados tipo II y Tipo III para los factores A y B, e interprete las pruebas de F.
library(car)
GLM(Respuesta~ A+ B, datos3)$`Type II`
GLM(Respuesta~ A+ B, datos3)$`Type III`





#--------Ejericio4-------------------------
path <- 'C:/Users/Lenovo/Desktop/PSEI-ESTADISTICA Y CD/2DO CUATRIMESTRE/MODELOS LINEALES/CODIGOS CLASE/datos_5_ej5.csv'
datos <- read.csv(path, header = TRUE)
modeloej4=lm(y~x1+x2+x3+x4+x5,data =datos)
summary(modeloej4)


anova(modeloej4)


make.X=function(I, J) {
  X=NULL
  for(i in 1:I) {
    tmp=matrix(0, J, I)
    tmp[, i]=1
    X=rbind(X, cbind(tmp, diag(J)))
  }
  X=cbind(1, X)
  X
}
X=make.X(3, 2)
X
qr(X)$rank
## [1] 4


library(matlib)
library(sasLM)
model4=ModelMatrix(y~x1+x2+x3+x4+x5,datos)
X=as.matrix(model4$X)
dim(X) #dimension de la matriz
R(X)
C1<-c(0,1,-1,0,0,0)
C2<-c(0,1,0,-1,0,0)
C3<-c(0,1,0,0,-1,0)
C4<-c(0,1,0,0,0,-1)
C<-rbind(C1,C2,C3,C4)
X_n=rbind(X,C)
R(X)
R(X_n)

#Modelo completo
datos
model4_c=lm(y~x1+x2+x3+x4+x5,data =datos)
summary(model4_c)
anova(model4_c)
#Modelo reducido
model4_red=lm(y~x3+x4+x5,data =datos)
summary(model4_red)
anova(model4_red)
anova(model4_c,model4_red)


#Prueba de verosimilitud

logm1=logLik(model4_c)
logm1
logm2=logLik(model4_red)
logm2
glh=7-5
logLik(model4_c)-logLik(model4_red)
Tc=2*(logLik(model4_c)-logLik(model4_red))
Tc
pvalor=pchisq(T,df=glh,lower.tail = F)
pvalor





#----------ejercicio5----------------
# Cargar datos y bibliotecas

library(sasLM)
library(matlib)
path <- 'C:/Users/Lenovo/Desktop/PSEI-ESTADISTICA Y CD/2DO CUATRIMESTRE/MODELOS LINEALES/CODIGOS CLASE/datos_t5_5.csv'
datos5 <- read.csv(path, header = TRUE)
# Crear las variables y matrices de interés

y <- log(datos5$VALUEADD)
x1 <- rep(1, 27)
x2 <- log(datos5$LABOR)
x3 <- log(datos5$CAPITAL)
x4 <- (1/2)*x2^2
x5 <- (1/2)*x3^2
x6 <- x2*x3

X_1 <- cbind(x1,x2,x3,x4,x5,x6)  # Matriz diseño del modelo 1
X_2 <- cbind(x1,x2,x3)           # Matriz diseño del modelo 2
y <- as.matrix(y)

# Inciso a) ________________________________________________________________ 

C <- matrix( c(0, 0, 0, 1, 0, 0,
                0, 0, 0, 0, 1, 0,
                0, 0, 0, 0, 0, 1), nrow=3, byrow=TRUE)

beta1 <- Ginv(t(X_1)%*%X_1)%*%t(X_1)%*%y
SCH <- t(C%*%beta1)%*%solve(C%*%Ginv(t(X_1)%*%X_1)%*%t(C))%*%(C%*%beta1)
glh <- R(C%*%Ginv(t(X_1)%*%X_1)%*%t(C))
SCE <- t(y - X_1%*%beta1)%*%(y - X_1%*%beta1)
n <- nrow(X_1)
lambda1 <- (SCE/(SCE+SCH))^(n/2)
Tc <- -2*log(lambda1); Tc

alfa=0.05
qchisq((1-alfa), df=(glh)) ## valor de tablas F

pchisq(Tc, df=(glh), lower.tail = F) ## Pvalue


# Otra forma de probar la hipótesis (usando lmtest)

D <- data.frame(y, x2, x3, x4, x5, x6)

m1 <- lm(y ~ x2 + x3 + x4 + x5 + x6, D)
m2 <- lm(y ~ x2 + x3, D)

install.packages("lmtest")
library(lmtest) ## Usando librería lmtest

lrtest(m1,m2)

# Inciso b) ________________________________________________________________ 

C_L <- matrix( c(0, 1, 1, 0, 0, 0,
                0, 0, 0, 1, 1, 2), nrow=2, byrow=TRUE)

m2 <- as.matrix(c(1,0))

beta1=Ginv(t(X_1)%*%X_1)%*%t(X_1)%*%y
beta_L=beta1-Ginv(t(X_1)%*%X_1)%*%t(C_L)%*%solve(C_L%*%Ginv(t(X_1)%*%X_1)%*%t(C_L)) %*%(C_L%*%beta1 - m2)
n=nrow(X_1)
sigma2_r=t(y-X_1%*%beta_L)%*%(y-X_1%*%beta_L)/n
LM=(t(C_L%*%beta1 - m2)%*%solve(C_L%*%Ginv(t(X_1)%*%X_1)%*%t(C_L))%*%(C_L%*%beta1 - m2))/sigma2_r
gl=R(C_L%*%Ginv(t(X_1)%*%X_1)%*%t(C_L))
gl
LM
alfa=0.05
#valor de tablas F
qchisq((1-alfa), df=gl) 
## Pvalue
pchisq(LM, df=gl, lower.tail = F) 

# Inciso c) ________________________________________________________________ 
# Matriz diseño del m2
X_2 <- cbind(x1,x2,x3)           
C3 <- matrix( c(0, 1, 1), nrow=1, byrow=TRUE)

m3 <- 1

beta2=Ginv(t(X_2)%*%X_2)%*%t(X_2)%*%y
beta_L2=beta2-Ginv(t(X_2)%*%X_2)%*%t(C3)%*%solve(C3%*%Ginv(t(X_2)%*%X_2)%*%t(C3)) %*%(C3%*%beta2 - m3)
n=nrow(X_2)
sigma2_r2=t(y-X_2%*%beta_L2)%*%(y-X_2%*%beta_L2)/n
LM2=(t(C3%*%beta2 - m3)%*%solve(C3%*%Ginv(t(X_2)%*%X_2)%*%t(C3))%*%(C3%*%beta2 - m3))/sigma2_r2
gl2=R(C3%*%Ginv(t(X_2)%*%X_2)%*%t(C3))
gl2
LM2
alfa_2=0.05; 
# valor de tablas F
qchisq((1-alfa_2), df=gl2) 

#pchisq(LM2, df=gl2, lower.tail = F) ## Pvalue


#p-------------PROBLEMA 6------
url = "http://www.stern.nyu.edu/~wgreene/Text/Edition7/TableF5-2.txt"
datos = read.table(url, header = T)
#####a------------ a
#regresion con glm
#columna realcons t-1
realconst_1 = c(NA, datos$realcons[-nrow(datos)])
#modelo
model1<-glm(log(realcons)~log(realdpi)+log(realconst_1), data = datos)
#guardamos los betas
beta_hat <- matrix(coef(model1), ncol=1,
                        dimnames=list(names(coef(model1)), NULL))
beta_hat
#G(theta) matriz de derivadas
G_derv<-t(matrix(c(1/(1-beta_hat[3]),beta_hat[2]/((1-beta_hat[3])^2))))
G_derv
#I_theta estimado
Vest<-vcov(model1)[2:3,2:3]
Vest
#Matriz Vestimado de beta de G I t(G)
V_est_g_beta<-G_derv%*%Vest%*%t(G_derv)
V_est_g_beta

#h_0: g(\beta)
h0<-beta_hat[2]/(1-beta_hat[3])
#Estadistico W
Wn<-t(h0-1)%*%solve(V_est_g_beta)%*%(h0-1)
alfa=0.05
chi_tab<-qchisq((1-alfa), df=1) ## valor de tablas 
p_value<-pchisq(Wn, df=1, lower.tail = F) ## Pvalue
data.frame(Wn,chi_tab,p_value)

              
               
#Ho:betas[2]/(1-betas[3])=1 y Ha:betas[2]/(1-betas[3]) \neq 1
#Dado que el valor del p_value es mayor a \aplha =0.05, no 
#hay evidencia suficiente para rechazar Ho.



#----------b----------------
#modelo
model1<-glm(log(realcons)~log(realdpi)+log(realconst_1), data = datos)

#guardamos los betas
beta_hat <- matrix(coef(model1), ncol=1,
                   dimnames=list(names(coef(model1)), NULL))
beta_hat

#G(theta) matriz de derivadas
G_derv<-t(matrix(c(1/(1-beta_hat[3]),beta_hat[2]/((1-beta_hat[3])^2))))
G_derv
#I_theta estimado (Varianzas y covarianzas)
Vest<-vcov(model1)[2:3,2:3]
Vest

#Matriz Vestimado (Varianza)
Varianza<-G_derv%*%Vest%*%t(G_derv)

#h_0: g(\beta)=mu
mu<-beta_hat[2]/(1-beta_hat[3])
data.frame(mu,Varianza)





#--------c-------------------------
# Simulación Boostrap:
beta1<-c()
beta2<-c()
beta3<-c()
nr=nrow(datos)
NBOOT <- 1000 ; ind <- NULL ; set.seed(1234)
for (i in 1:NBOOT){
  datam<-datos[sample(nrow(datos),size=nr,replace=TRUE),]
  datos_new = data.frame(
    realconst = datam$realcons,
    realdpi = datam$realdpi,
    realconst_1 = c(NA, datam$realcons[-nrow(datam)])
  )
  regre<-lm(log(realconst) ~ log(realdpi) + log(realconst_1), data=datos_new)
  beta1<-append(beta1,regre$coefficients[1])
  beta2<-append(beta2,regre$coefficients[2])
  beta3<-append(beta3,regre$coefficients[3])
}
# índice a estudiar:
ind= beta2/(1- beta3)
# Intervalos:
li=sort(ind)[25]
ls=sort(ind)[975]
data.frame(li,ls)



## Simulación MonteCarlo:
logrealconst = log(datos_new$realconst)
logrealdpi = log(datos_new$realdpi)
logrealconst_1 = log(datos_new$realconst_1)
log(realconst_1)
# Modelo:
modelo = lm(logrealconst ~ logrealdpi + logrealconst_1, datos_new)
# Coeficientes:
b1 = modelo$coefficients[1]
b2 = modelo$coefficients[2]
b3 = modelo$coefficients[3]
raiz_cme = summary(modelo)$sigma
# Simulaciones
pred <- data.frame(cbind(logrealdpi, logrealconst_1))
nr = nrow(pred)
#nr = nrow(datos_new)
beta1 <- c()
beta2 <- c()
beta3 <- c()
NREP <- 1000 ; ind <- NULL ; #set.seed(10000)

for (i in 1:NREP){
  y = b1 + b2*logrealdpi + b3*logrealconst_1 + rnorm(nr, -0.032591, raiz_cme)
  regre <- lm(y ~ logrealdpi + logrealconst_1)
  beta1 <- append(beta1, regre$coefficients[1])
  beta2 <- append(beta2, regre$coefficients[2])
  beta3 <- append(beta3, regre$coefficients[3])
}
# Indice a estudiar:
ind=beta2/(1-beta3)
# Intervalos:
lii=sort(ind)[25]
lss=sort(ind)[975]
data.frame(lii,lss)



#----------8--------------------------
path <- 'C:/Users/Lenovo/Desktop/PSEI-ESTADISTICA Y CD/2DO CUATRIMESTRE/MODELOS LINEALES/CODIGOS CLASE/datos8.csv'
datos8 <- read.csv(path, header = TRUE)
datos8$Site<- as.factor(datos8$Site)
datos8$habitat<- as.factor(datos8$habitat)
datos8$Latitude<- as.numeric(datos8$Latitude)
datos8$Elevation<- as.numeric(datos8$Elevation)

#Modelo de regresión simple
model_reg= lm(log(Species.richness) ~ Latitude + Elevation 
              + habitat,data=datos8)
summary(model_reg)

#Modelo Bayesiano
# Ajustamos el modelo:
library(INLA)
library(brm)
library(mlbench)
library(rstanarm)
library(bayestestR)
library(bayesplot)
library(insight)
library(broom)

library(brms)
library(broom)
library(broom.mixed)
library(coda)
#brm
model_bayes <- brm(log(Species.richness) ~ Latitude + Elevation 
                   + habitat, data = datos8)
print(model_bayes$Regr, digits = 6)

#inla
library(INLA)
library(gridExtra)
library(ggplot2)
inlamod <- inla(log(Species.richness) ~ Latitude + Elevation 
                + habitat, data = datos8, family="gaussian")
inlamod$summary.fixed[,1:2]

#STAN
model_bayes<- stan_glm(log(Species.richness) ~ Latitude + Elevation 
                       + habitat, data = datos8, seed=111)
print(model_bayes, digits = 3)









#----------7--------------------------


#---EJERCICIO 7 
url = "http://www.stern.nyu.edu/~wgreene/Text/Edition7/TableF5-2.txt"
datos = read.table(url, header = T)
head(datos)

datos_new = data.frame(
  realconst = datos$realcons,
  realdpi = datos$realdpi,
  realconst_1 = c(NA, datos$realcons[-nrow(datos)])
)

head(datos_new)



# Modelo Cl?sico:
modelo = lm(log(realconst)~log(realdpi) + log(realconst_1), datos_new)
summary(modelo)



# Modelo Bayesiano con INLA
library(INLA)
bayes1 = inla(log(realconst)~log(realdpi) + log(realconst_1), 
              family = "Gaussian", data = datos_new)
summary(bayes1)

#Con Stan
library(brms)
library(rstanarm)
bayes2 = stan_glm(log(realconst)~log(realdpi) + log(realconst_1),
                  data=datos_new)

print(bayes2, digits= 4)

# Intervalos de credibilidad
posterior_interval(bayes2)

# distribución posterior
library(bayestestR)
describe_posterior(bayes2, centrality = "mean")
library(bayesplot)
mcmc_dens(bayes2)
posterior_samples(bayes2)


#Prueba de hipotesis 
library(MCMCpack)
library(tidymodels)

#comparacion B2/(1-B3)
#H0:B2/(1-B3)
resultados_mod=as.matrix(bayes2)

hipotesis=resultados_mod[,2]/(1-resultados_mod[,3])
plot(density(hipotesis),xlim=c(0,1.5))

#funcion estimar
FUNCION=(resultados_mod[,2]/(1-resultados_mod[,3]))-1
plot(density(FUNCION),xlim=c(-0.5,0.5))

#Limites de credibilidad 
summary(hipotesis, c(0.025, 0.975))

c(mean = mean(posterior_samp$indice), cred_interval)


########
# Obtenemos las muestras de la distribución posterior de los coeficientes:
posterior_sample <- posterior_samples(bayes2)
# Calcular beta_2 / (1 - beta_3):
posterior_samp$indice <- posterior_samp$b_logrealdpi/(1-posterior_samp$b_logrealconst_1)
# Obtener intervalo de credibilidad del 95%:
cred_interval <- quantile(posterior_samp$indice, c(0.025, 0.975))
# Resultados
c(mean = mean(posterior_samp$indice), cred_interval)
