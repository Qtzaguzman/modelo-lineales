# Clase 2024/02/15 Modelos Lineales

getwd()
setwd('C:/Users/User/Downloads')

Data <- read.csv('fert_dosis.csv')

Data$Dosis <- as.factor(Data$Dosis)

head(Data)

# Librería para graficas para publicar
library(ggpubr)

# Gráfica del rendimiento medio por dosis
# Usar jitter para agregar los datos
ggerrorplot(data = Data, x = 'Dosis', y = 'Rend',
            add = 'jitter')

library(sasLM)
library(matlib)
# Obtenemos la matriz diseño
Xm <- ModelMatrix(Rend ~ Dosis, Data)

X <- as.matrix(Xm$X)

lambda2 <- c(0,1,-1,-1,1)

# Probamos si es estimable

# Método del rango
R(X)

Xnueva <- rbind(X,t(lambda2))

R(Xnueva)

# Como no cambia entonces lambda2 pertenece al
# espacio de renglones de X y es estimable

# Encontrar el estimador de beta utilizando 
# inversa generalizada
Y <- as.vector(Data$Rend)

beta_est <- Ginv(t(X)%*%X)%*%t(X)%*%Y

# Estimamos lambdat beta
# Este estimador es MELI
efecto_cuadratico_est <- lambda2%*%beta_est

# Suma de cuadrado del error
SCE <- t(Y - X%*%beta_est)%*%(Y - X%*%beta_est)
# Grados de libertad
gle <- nrow(X) - R(X)

# Estimación de sigma cuadrada

CME <- SCE/gle

# Varianza de combinación lineal

Vcl <- t(lambda2)%*%Ginv(t(X)%*%X)%*%lambda2%*%CME

# Valor de tablas de t-student

ttab <- qt(1 - 0.05/2,gle)

# Intervalo de confianza al 95% para la comb lineal

li <- efecto_cuadratico_est - ttab * sqrt(Vcl)
ls <- efecto_cuadratico_est + ttab * sqrt(Vcl)

cbind(li,ls)

# Como incluye cero no es estadísticamente significante

# 
library(emmeans)

anova_m <- lm(Rend ~ Dosis, Data)
summary(anova_m)

anova(anova_m)


# Intervalo de confianza usando libreía emmeans

medias <- emmeans(anova_m, 'Dosis')

plot(medias, horiz = FALSE)

# Intervalo de confianza de la combinación lineal

lam <- contrast(medias, method = list(c(1,-1,-1,1)))

confint(lam)












