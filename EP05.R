# ENUNCIADO 5

#Integrantes:
# Carlos Castillo      Rut: 20.201.274-4
# Francisco Moreno     Rut: 19.892.183-1
# Gianfranco Piccinini Rut: 20.237.081-0
# Joaqu�n Torres       Rut: 19.091.702-9



# Se sabe que el proceso de fabricaci�n de barras de acero para concreto 
# reforzado producen barras con medidas de dureza que siguen una distribuci�n 
# normal con desviaci�n est�ndar de 10 kilogramos de fuerza por mil�metro
# cuadrado. 
# Usando una muestra aleatoria de tama�o 25, un ingeniero quiere averiguar si una
# l�nea de producci�n est� generando barras con dureza media de 170 [kgf mm-2] 

#-------------------------------------------------------------------------------
#             PREGUNTA 1
#-------------------------------------------------------------------------------
# Si el ingeniero est� seguro que la verdadera dureza media no puede ser menor a 
# los 170 [kgf mm-2] y piensa rechazar la hip�tesis nula cuando 
# la muestra presente una media mayor a 173 [kgf mm-2], �cu�l es la probabilidad 
# de que cometa un error de tipo 1?


library ( ggpubr )
library(pwr)


# Respuesta: 
# Entonces, la media equivalente a 170, se refiere a la media nula, y la media
# mayor a 173 se refiere al valor de la media al rechazar la hip�tesis nula:
media_nula <- 170
media_rechazo_h0 <- 173
n <- 25
desv_est <- 10

# Entonces, se obtiene el valor de z, mediante la formula de distribuci�n z.
error_est_n <- desv_est/sqrt(n)
z <- (media_rechazo_h0 - media_nula)/error_est_n

# Finalmente, se obtiene el valor de alfa, siendo la probabilidad de que se cometa
# un error tipo 1
alfa <- pnorm(z, lower.tail = FALSE)

cat("----------------------------------------------------------------------\n")
cat("PREGUNTA 1\nLa probabilidad de que se cometa un error tipo 1 es: ",alfa)





#-------------------------------------------------------------------------------
#             PREGUNTA 2
#-------------------------------------------------------------------------------
# Si la verdadera dureza media de la linea de producci�n fuera 172 [kgf mm^-2]
# �cu�l ser�a la probabilidad de que el ingeniero, que obviamente no conoce
# este dato, cometa un error de tipo 2?


# Se calcula el valor del poder de la muestra con el la diferencia entre el real
# y te�rico y significancia dada para cada valor real de la media, asumiendo 
# una prueba bilateral para una sola muestra .
poder <- power.t.test ( n = 25,
                        delta = 2,
                        sd = 10,
                        sig.level = 0.05,
                        power = NULL,
                        type = "one.sample",
                        alternative = "two.sided")$power

poder
prob_error_2 <- 1-poder
prob_error_2

# El poder obtenido si la verdadera dureza media fuera 172 luego de hacer una
# prueba para una media de 170 es de 0.1587619, que es la posibilidad de NO
# cometer un error de tipo 2.
# 
# Por lo tanto la probabilidad de cometer un error de tipo 2 es 1-poder:
# 0.8412381



library ( TeachingDemos )
library ( tidyr )

#-------------------------------------------------------------------------------
#             PREGUNTA 3
#-------------------------------------------------------------------------------
# Como no se conoce la verdadera dureza media, genere un gr�fico del poder
# estad�stico con las condiciones anteriores, pero suponiendo que las verdaderas
# durezas medias podr�an variar de 170 a 178 [kgf mm^-2].
desv <- 10
muestra <- 25
u0 <- 170

alfa <- 0.05
efecto <- seq(-8,8,0.01)

prueba <- power.t.test(n = muestra,
                       delta = efecto,
                       sd = desv,
                       sig.level = alfa,
                       type = "one.sample",
                       alternative = "two.sided")$power


#DATA FRAME
datos <- data.frame(efecto,prueba)
datos <- datos %>% tidyr::pivot_longer(!"efecto",
                                       names_to = "fuente",
                                       values_to = "poder")

#GR�FICO
g <- ggplot ( datos , aes ( efecto , poder , colour = factor( fuente ) ) )

g <- g + geom_line ()
g <- g + labs ( colour = "")
g <- g + ylab (" Poder estad�stico ")
g <- g + xlab (" Tama�o del efecto ")
g <- g + theme_pubr ()
g <- g + ggtitle (" Curva de poder para prueba t bilateral ")


print(g)

cat("----------------------------------------------------------------------\n")

#-------------------------------------------------------------------------------
#             PREGUNTA 4
#-------------------------------------------------------------------------------
# �Cu�ntas barras deber�an revisarse para conseguir un poder estad�stico de 0,85
# y un nivel de significaci�n de 0,05?


# En espec�fico para la dureza media real fuera 172
n_real <- power.t.test ( n = NULL,
                         delta = 2,
                         sd = 10,
                         sig.level = 0.05,
                         power = 0.85,
                         type = "one.sample",
                         alternative = "two.sided")$n
n_real
# La cantidad de barras que deber�an revisarse para conseguir un poder 
# estad�stico de 0.85 y un nivel de significaci�n de 0.05 es de 227.

cat("----------------------------------------------------------------------\n")

#-------------------------------------------------------------------------------
#             PREGUNTA 5
#-------------------------------------------------------------------------------
# �Y si se quisiera ser bien exigente y bajar la probabilidad de cometer un
# error de tipo 1 a un 1% solamente?


# En espec�fico para la dureza media real fuera 172
n_real <- power.t.test ( n = NULL,
                         delta = 2,
                         sd = 10,
                         sig.level = 0.01,
                         power = 0.85,
                         type = "one.sample",
                         alternative = "two.sided")$n
n_real
# La cantidad de barras que deber�an revisarse para conseguir un poder 
# estad�stico de 0.85 y un nivel de significaci�n de 0.01 es de 330.