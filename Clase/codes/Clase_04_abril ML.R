## Ejemplo boostrap regresion
T =c(15.1,13.3,15.3,13.3,14.6,15.6,13.1,13.1,15.0,11.7,15.3,14.4,14.4,12.7,
     11.7,11.9,15.9,13.4,14.0,13.9,12.9,15.1,13.0)
P= c(67,52,88,61,32,36,72,43,92,32,86,28,57,55,66,26,28,96,48,90,86,78,87)
Y=c( 2.55, 1.85,2.05,2.88,3.13,2.21,2.43,2.69,2.55,2.84,2.47,
     2.69,2.52,2.31,2.07,2.35,2.98,1.98,2.53,2.21,2.62,1.78,2.30)
datosex=data.frame(cbind(T,P,Y))
head(datosex)

# Modelo de Regresión.
modelo = lm(Y~T+P, datosex)
summary(modelo)

# Containers for the coefficients
coef_int_rem <- NULL
coef_T_rem <- NULL
coef_P_rem <- NULL
B=1000
set.seed(1234)
for (i in 1:B) {
  #Creating a resampled dataset from the sample data
  remues = datosex[sample(1:nrow(datosex), nrow(datosex), replace = TRUE), ]
  
  #Ejecutando la regresion
  modelo_bootstrap <- lm(Y ~ T+P, data = remues)
  
  #Guardando los coeficientes
  coef_int_rem<-
    c(coef_int_rem, modelo_bootstrap$coefficients[1])
  coef_T_rem <-
    c(coef_T_rem, modelo_bootstrap$coefficients[2])
  
  coef_P_rem <-
    c(coef_P_rem, modelo_bootstrap$coefficients[3])
}
# Diemnsión de los vectores de los coeficientes estimados.
length(coef_T_rem)
length(coef_P_rem)

# Intervalo de confianza del 95% para Beta1 (temperatura):
li=sort(coef_T_rem)[25]
ls=sort(coef_T_rem)[975]
ic95=c(li,ls)
ic95
# El cero queda incluido. No se rechaza H0.

# Intervalo de confianza del 95% para Beta2 (precipitación):
li=sort(coef_P_rem)[25]
ls=sort(coef_P_rem)[975]
ic95=c(li,ls)
ic95
# El cero no está incluido. Se rechaza H0.


# Probar una hipótesis no lineal:
#H_0: B_1/B_2=0.5 vs H_a: B_1/B_2\neq 0.5 
hipotesis = coef_T_rem /coef_P_rem
li=sort(hipotesis)[25]
ls=sort(hipotesis)[975]
ic95=c(li,ls)
ic95
# El 0.5 esta incluido. No se rechaza H0.

# Histogramas
hist(coef_P_rem)
hist(coef_T_rem)
hist(hipotesis)











