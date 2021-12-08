#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#                     Enunciado 4, desarrollo de sala 2

#           Integrantes:                        RUT:              
#        -Carlos Castillo                   20.201.274-4
#        -Francisco Moreno                  19.892.183-1
#        -Gianfranco Piccini                20.237.081-0
#        -Joaquín Torres                    19.091.702-9
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
library ( ggpubr )
library ( dplyr )
#-------------------------------------------------------------------------------
# PREGUNTA 1

# El artículo "Engineering Properties of Soil" (Soil Science 1998) puso a
# prueba la idea generalizada de que al menos el 3% del suelo corresponde a
# materia orgánica. Para esto, los autores obtuvieron una muestra aleatoria de
# especímenes de suelo, determinando que el porcentaje de materia orgánica
# presente en cada espécimen era (usando punto en vez de coma decimal):

tierra <- "3.10 5.09 2.97 1.59 4.60 3.32 0.55 1.45 0.14 4.47
           0.80 3.50 5.02 4.67 5.22 2.69 3.98 3.17 3.03 2.21
           2.69 4.47 3.31 1.17 2.76 1.17 1.57 2.62 1.66 2.05"

# ¿Qué conclusión sugeriría a los autores?

cat("--------------------------------------------------------------\n")
cat("Pregunta 1\n Resultados de la evaluación:\n")

file_tierra <- textConnection(tierra)
datos_tierra <- scan(file_tierra)

#HIPÓTESIS (u: media de materia orgánica poblacional en porcentaje)
#H0: u >= 3 
#HA: u <  3

nulo_tierra <- 3

# Verificar si la distribución se acerca a la normal.
g_tierra <- ggqqplot(data = datos_tierra,
                     datos_tierra = "porcentaje",
                     color = "steelblue",
                     xlab = "Teórico",
                     ylab = "Muestra",
                     title = "Grafico Q-Q muestra v/s distribucion normal")

print(g_tierra)

# La muestra se asemeja a distribución normal por no encontrarse valores 
# atípicos que se alejen de la región aceptable.
# 
# Los datos son independientes.
# 
# Entonces, como la pregunta no es mencionada la varianza de la población,
# y solo nos provee 30 datos de la muestra, se puede utilizar la prueba t student.

# Nivel de significación elegido.
alfa <- 0.01

# Aplicar la prueba t de Student.
prueba <- t.test(datos_tierra,
                 alternative = "less",
                 mu = nulo_tierra,
                 conf.level = 1-alfa)
print(prueba)

# Los resultados para esta prueba son:
#   Estadístico de prueba T:  t   = -0.63195
#   Grados de libertad:       df  = 29
#   Valor p obtenido:         p = 0.2662
#   Intervalo de confianza: [-Inf, 3.47879]
#   Media de la muestra:      _x   = 2.834667

# En este caso la media de la muestra esta dentro del intervalo de confianza:
# valor p es mayor que el nivel de significancia, por lo que
# se falla al rechazar la hipótesis nula. 

# En consecuencia si no se rechaza la hipótesis nula se puede afirmar con 99% de 
# confidencia que el suelo tiene a lo menos un 3% de materia orgánica en general.

#-------------------------------------------------------------------------------
# PREGUNTA 2

#Se sabe que la lactancia estimula una pérdida de masa ósea para proporcionar 
#cantidades de calcio adecuadas para la producción de leche. Un estudio intentó 
#determinar si madres adolescentes podían recuperar niveles más normales a pesar
#de no consumir suplementos (Amer. J. Clinical Nutr., 2004; 1322-1326).
#El estudio obtuvo las siguientes medidas del contenido total de minerales en
#los huesos del cuerpo (en gramos) para una muestra de madres adolescentes tanto
#durante la lactancia (6-24 semanas post parto) y posterior a ella (12-30 semana
#post parto): 

#¿Sugieren los datos que el contenido total de minerales en los huesos del
#cuerpo durante el post destete excede el de la etapa de lactancia por más de 25 g? 

cat("--------------------------------------------------------------\n")
cat("Pregunta 2\n Resultados de la evaluación:\n")

#HIPÓTESIS (u: excedencia de contenido total de minerales entre la etapa de 
# y post destete)
#H0: u =< 25 
#HA: u >  25

# Ingresar los datos.
instancia <- seq (1 , 10 , 1)
valor_nulo <- 25

lactancia<- "1928 2549 2825 1924 1628 2175 2114 2621 1843 2541"

texto <- (lactancia)
file_lactancia<- textConnection(texto)
datos_lactancia<- scan(file_lactancia)


posdestete<- "2126 2885 2895 1942 1750 2184 2164 2626 2006 2627"

texto2<-(posdestete)
file_posdestete <- textConnection(texto2)
datos_posdestete <- scan(file_posdestete)

diferencia <- datos_posdestete - datos_lactancia

# Verificar si la distribución se acerca a la normal .
normalidad <- shapiro.test(diferencia)
print(normalidad)

# Dado que W es cercano a 1, la distribucion se puede considerar normal.
# Los datos son independientes.
# Por lo tanto se puede aplicar la prueba t student

# Fijar un nivel de significación.
alfa <- 0.01

# Aplicar la prueba t de Student a la diferencia de medias .
prueba_1<- t.test( diferencia ,
                   alternative = "greater",
                   mu = valor_nulo,
                   conf.level = 1 - alfa )

print(prueba_1)
# Los resultados para esta prueba son:
#   Estadístico de prueba T:  t   = 2.4575
#   Grados de libertad:       df  = 9
#   Valor p obtenido:         p = 0.0.1815
#   Intervalo de confianza: [13.0477, Inf]
#   Media de la diferencia:      _d   = 105.7

# En este caso la media de la diferencia de las muestras esta dentro del 
# intervalo de confianza: valor p es mayor que el nivel de significancia, 
# por lo que se falla al rechazar la hipótesis nula. 

# En consecuencia si no se rechaza la hipótesis nula se puede afirmar con 99% de 
# que no se excede el contenido total de minerales de calcio en el cuerpo de la 
# madre por mas de 25g entre la etapa post y pre destete.

#-------------------------------------------------------------------------------
# PREGUNTA 3

# La avicultura de carne es un negocio muy lucrativo, y cualquier método que 
# ayude al rápido crecimiento de los pollitos es beneficiosos, tanto para las 
# avícolas como para los consumidores. En el paquete datasets de R estan los 
# datos (chickwts) de un experimento hecho para medir la efectividad de varios 
# suplementos alimenticios en la tasa de crecimiento de las aves. Pollitos recién 
# nacidos se separaron aleatoriamente en 6 grupos, y a cada grupo se le dio un 
# suplemento distinto. Para productores de la 7ma región, es especialmente 
# importante saber si existe diferencia en la efectividad entre el suplemento 
# basado en linaza (linseed) y el basado en soya (soybean).

cat("--------------------------------------------------------------\n")
cat("Pregunta 3\n Resultados de la evaluación:\n")

#Obtención de datos interesados en el conjunto de datos chickwts
load_data <- chickwts

linseed_data <- subset((load_data %>% filter(feed == "linseed")), select = -feed)

soybean_data <- subset((load_data %>% filter(feed == "soybean")), select = -feed)

# HIPOTESIS: no hay diferencia en la media poblacional del peso de los pollos 
# sea alimentado con soya o linaza.

# uL = media poblacional del peso de los pollos alimentados con linaza.
# uS = media poblacional del peso de los pollos alimentados con soya.

# H0: uL  =   uS    ó     uL - uS  =  0.
# HA: uL =/=  uS    ó     uL - uS =/= 0.

# Nivel de significancia elegido
alpha <- 0.05
null_value <- 0

# Verificar si la distribución se acerca a la normal.
normalzy_linseed <- shapiro.test(linseed_data$weight)
normalzy_soybean <- shapiro.test(soybean_data$weight)
print(normalzy_linseed)
print(normalzy_soybean)

# Valor p peso pollos con linaza = 0.9035
# Valor p peso pollos con soya   = 0.5064
# El valor p de ambas distribuciones es mayor al valor de significancia 0.05 
# elegido por lo tanto se puede aceptar la hipótesis de que están distribuidas
# normalmente.

# Los datos son independientes
# Los datos están distribuidos de forma normal

# Se puede aplicar la prueba t para dos muestras independientes.
mean_L <- mean(linseed_data$weight)
mean_S <- mean(soybean_data$weight)
mean_diff <- mean_L - mean_S

prueba <- t.test(x = linseed_data,
                 y = soybean_data,
                 paired = FALSE,
                 alternative = "two.sided",
                 mu = null_value,
                 conf.level = 1- alpha)

print(prueba)
cat("mean of x minus mean of y\n", mean_diff)

# Los resultados para esta prueba son:
#   Estadístico de prueba T:  t   = -1.3246
#   Grados de libertad:       df  = 23.63
#   Valor p obtenido:         p   = 0.198
#   Intervalo de confianza: [-70.84262,  15.48547]
#   Diferencia de las medias de la muestra:
#                         xL - xS = -27.67857

# En este caso la media de la diferencia de las muestras esta dentro del 
# intervalo de confianza: el valor p es mayor que el nivel de 
# significancia (0.05), por lo que se falla al rechazar la hipótesis nula. 

# En consecuencia si no se rechaza la hipótesis nula se puede afirmar con 95% de 
# confidencia que en promedio el suplemento a base de linaza es igual de
# efectivo que el suplemento a base de soya para el efecto de engordar pollos.

