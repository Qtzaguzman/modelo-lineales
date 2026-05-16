
library(readr)
yuca <- data.frame(read_csv("Modelos Lineales/yuca.csv"))

# convertir a factores
yuca$Genotipos=as.factor(yuca$Genotipos)

# Modelo
# yij = \mu + Tau_{i} + \epsilon_{ij}
# produccion  del j-ésimo tratamiento de la i-ésima variedad
# \Tau_{i} es el efecto de la variedad i.
# i = 1, ..., 9.
# j = r_i, las réplicas no son las mismas para todas las variedades.
library(sasLM)
M_X=ModelMatrix(Rendimiento~Genotipos,yuca)

# Matriz diseño
X=as.matrix(M_X$X)
X

# Respuesta:
Y = yuca[, 1]

# Rango de X
qr(X)$rank

# Una solución es:
library(matlib)
Bhat1 = Ginv(t(X)%*%X)%*%t(X)%*%Y

# Probar si la yuca 1 = yuca 2
# Tau_1 = Tau_2
# Lambda  = c(mu, tau_1, tau_2, tau_3, tau_4, tau_5, tau_6, tau_7, tau_8. tau_9)
lambda = c(0, 1, -1, 0, 0, 0, 0, 0, 0, 0)


#¿lambda está en C(X) (espacio de renglones)?

# Método del Rango 

# 1.- Matriz Aumentada con lambda
Xnueva = rbind(X, t(lambda))
# rango
qr(Xnueva)$rank
# Si no cambia el rango, lambda sí pertenece al espacio, y por tanto es estimable


# 2.- Uso de inversa generalizada
Xg = Ginv(t(X)%*%X)
# Teorema 1:
t(lambda)%*%Xg%*%t(X)%*%X
# Si es igual a lambda^t, entonces, lambda es estimable.


# 3.- Espacios Nulos
I = diag(1, 10)
round(t(lambda)%*%(I - Xg%*%t(X)%*%X), 0)
# Si es igual al vector nulo, entonces es estimable.



# Usando librerías
library(estimability)
# Base no estimable (no redondear):
base1 = nonest.basis(X)
# Verifica si nuestra lambda es estimable:
is.estble(lambda, base1)


# Encontrar un set de combinaciones lineales independientes y estimables:

# 1.- Matriz Escalonada de X
X_esc = echelon(X)

# 2.- Usando Dummys
modelo = lm(Rendimiento~Genotipos,yuca)
# Matriz reparametrizada
W =as.matrix(model.matrix(modelo))
# Estimar coeficientes
theta = solve(t(W)%*%W)%*%t(W)%*%Y
# Combinaciones Linealmente Independientes (estimables)
round(solve(t(W)%*%W)%*%t(W)%*%X)
# Nota: La diferencia entre el genotipo1 y el 2 es la estimación en theta_2


# Qué pasa si no es estimable:
# Para que sea contraste debe igualarse a cero
# mu + 5 tau_1 + tau_2 + tau_3
# Lambda  = c(mu, tau_1, tau_2, tau_3, tau_4, tau_5, tau_6, tau_7, tau_8. tau_9)
lambda2 = c(1, 5, 1, 1, 0, 0, 0, 0, 0, 0)

# ¿Es estimable lambda2?
# Usando Librerías
is.estble(lambda2, base1)
# Otro no estimable:
lambda3 = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# ¿Es estimable lambda3?
is.estble(lambda3, base1)
# Usando la matriz aumentada
Xnueva2 = rbind(X, t(lambda3))
# rango
qr(Xnueva2)$rank







# =============== EJEMPLO CLASE: ===============
cliente = c(1, 1, 2, 2, 3, 4, 4)
Pelicula = c(1, 2, 2, 3, 3, 1, 2)
Calif = c(4, 1, 3, 5, 3, 3, 1)
datos = data.frame(cbind(cliente, Pelicula, Calif))
datos$cliente = as.factor(cliente)
datos$Pelicula = as.factor(Pelicula)
Mod = ModelMatrix(Calif ~ cliente + Pelicula, datos)
# Matriz Diseño
X = as.matrix(Mod$X)
# Rango de X
qr(X)$rank

# Lambda
# Comparar película 2 con 3, m2 = m3 -> m2-m3=0
lambda = c(0, 0, 0, 0, 0, 0, 1, -1)
Xnueva = rbind(X, t(lambda))
qr(Xnueva)$rank
# El rango no cambia, por tanto, sí es estimable

# ¿Y es estimable para m1 y m3?
lambda = c(0, 0, 0, 0, 0, 1, 0, -1)
Xnueva = rbind(X, t(lambda))
qr(Xnueva)$rank
# También lo es.

