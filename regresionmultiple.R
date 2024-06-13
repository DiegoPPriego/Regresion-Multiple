# Cargar la librería necesaria para leer datos
library(readr)
library(randtests)
# Leer el archivo BGSboys.txt
# datosboys contiene las mediciones para niños
datosboys <- read.table("C:/Users/yeyoe/Documents/Diego/Sexto/regresion lineal/BGSboys.txt", header = TRUE)

# Leer el archivo BGSgirls.txt
# datosgirls contiene las mediciones para niñas
datosgirls <- read.table("C:/Users/yeyoe/Documents/Diego/Sexto/regresion lineal/BGSgirls.txt", header = TRUE)
# Aquí va la descripción de las variables en BGSgirls.txt:
# Sex 0 para hombres, 1 para mujeres.
# WT2 peso en kg a edad 2 años.
# HT2 altura en cm a edad 2 años.
# WT9 peso en kg a edad 9 años.
# HT9 altura en cm a edad 9 años.
# LG9 circunferencia de la pierna en cm a edad 9 años.
# ST9 fuerza en kg a edad 9 años.
# WT18 peso en kg a edad 18 años.
# HT18 altura en cm a edad 18 años.
# LG18 circunferencia de la pierna en cm a edad 18 años.
# ST18 fuerza medida en kg a edad 18 años.
# Soma Escala de tipo corporal donde 1 significa muy delgad@ y 7 obes@.

# Utilizar los datos de todas las variables de dos años y Soma para niñas
x_g = datosgirls

# Ajustar los márgenes de los gráficos
par(mex = 0.5)

# Crear una matriz de gráficos de dispersión para las variables a los 2 años y Soma
pairs(x_g[, c("HT2", "WT2", "Soma")], gap = 0, cex.labels = 0.9)

# Calcular y redondear la matriz de correlación
round(cor(x_g), 2)

# Crear una matriz de gráficos de dispersión para las variables a los 9 años y Soma
par(mex = 0.5)
pairs(x_g[, c("WT9", "HT9", "LG9", "ST9", "Soma")], gap = 0, cex.labels = 0.9)

# Calcular y redondear la matriz de correlación
round(cor(x_g), 2)

attach(x_g)

# Ajustar un modelo de regresión múltiple con Soma como variable dependiente y HT2, WT2, HT9, WT9, ST9 como independientes
modelo_g = lm(Soma ~ HT2 + WT2 + HT9 + WT9 + ST9)

# Mostrar el resumen del modelo
summary(modelo_g)

# Realizar un análisis de varianza (ANOVA) para el modelo
anova(modelo_g)

# Calcular el intervalo de confianza al 90% para los coeficientes del modelo_n
confint(modelo_g, level = 0.90)

# Continuar con los datos de BGSboys.txt y repetir el proceso para niños
#datos de dos años
x_b = datosboys
par(mex = 0.5)
pairs(x_b[, c("HT2", "WT2", "Soma")], gap = 0, cex.labels = 0.9)
round(cor(x_b), 2)
#los datos de nueve años

par(mex = 0.5)
pairs(x_b[, c("WT9", "HT9", "LG9", "ST9", "Soma")], gap = 0, cex.labels = 0.9)
round(cor(x_b), 2)


attach(x_b)
# Ajustar un modelo de regresión múltiple para los datos de niños
modelo_b = lm(Soma ~ HT2 + WT2 + HT9 + WT9 + ST9)
summary(modelo_b)
anova(modelo_b)

# Calcular el intervalo de confianza al 90% para los coeficientes del modelo_b
confint(modelo_b, level = 0.90)

#### Inciso d) i)

### PARA GIRLS
#extraer la informacion de residuales de nuestro modelo
residuales_g <- residuals(modelo_g)

print(residuales_g)

# Realizamos la prueba de Wilcoxon que nos sirve para comparar medias
# ho: la media de los residuales es igual a 0
# La media de los residuales es distinta a 0
wilcox.test(residuales_g, mu = 0)
# regla de desicion si pvalue > alfa se acepta
#                   si pvalue < alfa se rechaza 

#con un alfa= 0.05 y p-value = 0.6778



###PARA BOYS

#extraer la informacion de residuales de nuestro modelo
residuales_b <- residuals(modelo_b)

print(residuales_b)

# Realizamos la prueba de Wilcoxon que nos sirve para comparar medias
# ho: la media de los residuales es igual a 0
# La media de los residuales es distinta a 0
wilcox.test(residuales_b, mu = 0)
# regla de desicion si pvalue > alfa se acepta
#                   si pvalue < alfa se rechaza 

#con un alfa= 0.05 y p-value = 0.6827



##### INCISO d) ii)

#ho: Los datos NO TIENEN TENDENCIA
#H1: LOS DATOS TIENEN TENDENCIA

cox.stuart.test = function(x) {
  method = "Cox-Stuart test for trend analysis"
  leng = length(x)
  apross = round(leng)%%2
  if (apross == 1) {
    delete = (length(x) + 1)/2
    x = x[-delete]
  }
  half = length(x)/2
  x1 = x[1:half]
  x2 = x[(half + 1):(length(x))]
  difference = x1 - x2
  signs = sign(difference)
  signcorr = signs[signs != 0]
  pos = signs[signs > 0]
  neg = signs[signs < 0]
  if (length(pos) < length(neg)) {
    prop = pbinom(length(pos), length(signcorr), 0.5)
    names(prop) = "Increasing trend, p-value"
    rval <- list(method = method, statistic = prop)
    class(rval) = "htest"
    return(rval)
  } else {
    prop = pbinom(length(neg), length(signcorr), 0.5)
    names(prop) = "Decreasing trend, p-value"
    rval <- list(method = method, statistic = prop)
    class(rval) = "htest"
    return(rval)
  }
}

cox.stuart.test(residuales_b)
cox.stuart.test(residuales_g)

# regla de desicion si pvalue > alfa se acepta
#                   si pvalue < alfa se rechaza 

#con un alfa= 0.05 y p-value = 0.24343



#### INCISO  d)  iii)
#niñas
residuales=(residuals(modelo_g))
print(residuales)
sigma= (sigma(modelo_g))
print(sigma)
ks_result <- ks.test(residuales, "pnorm", mean = 0, sd=sigma)
print(ks_result)
#niños
residuales=(residuals(modelo_b))
print(residuales)
sigma= (sigma(modelo_b))
print(sigma)
ks_result <- ks.test(residuales, "pnorm", mean = 0, sd=sigma)
print(ks_result)










###El modelo explica aproximadamente el 33,55% de la varianza de Soma. WT2, WT9 y ST9 son predictores significativos, siendo WT9 el que tiene el mayor efecto positivo. HT2 y HT9 no son predictores significativos en este modelo.
###Error estándar residual: 1,181, que indica la distancia media entre los valores observados y los predichos.
Grados de libertad (DF): 60, que indica el número de observaciones menos el número de predictores.
R-cuadrado múltiple (0,3866): Proporción de la varianza de la variable dependiente explicada por las variables independientes.
R-cuadrado ajustado (0,3355): Ajustado al número de predictores del modelo.
Estadístico F (7,564): Indica la significación global del modelo.
Valor p (1,459e-05): Indica que el modelo es estadísticamente significativo.
###



