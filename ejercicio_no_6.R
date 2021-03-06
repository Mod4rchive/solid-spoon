#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#                     Enunciado 4, desarrollo de sala 2

#           Integrantes:                        RUT:              
#        -Carlos Castillo                   20.201.274-4
#        -Francisco Moreno                  19.892.183-1
#        -Gianfranco Piccini                20.237.081-0
#        -Joaqu�n Torres                    19.091.702-9
#-------------------------------------------------------------------------------

# Los siguientes datos vienen de un estudio hecho por Rothman & Keller (1972)
# (Journal of chronic diseases, 25(12), 711-716) sobre la incidencia de la
# cantidad de alcohol y de tabaco que se consume en el riesgo de padecer c�ncer
# oral. Las tablas mue consumiendo una cierta cantidad de alcohol o de tabaco,
# mostrada en cada fila, desarrollaron o no desarrollaron (controles) la
# enfermedad durante su vida.

#-------------------------------------------------------------------------------
library ( ggpubr )
library ( dplyr )
library ( pwr )
#-------------------------------------------------------------------------------

cancer_alcohol <- c(43, 89, 109, 242)
control_alcohol <- c(108, 141, 91, 107)

cancer_tabaco <- c(26, 66, 248, 143)
control_tabaco <- c(85, 97, 197, 68)

# Alcohol (ml por d�a)
# 0
# 1-9
# 10-44
# 45 o m�s
alcohol <- data.frame(casos = cancer_alcohol, controles = control_alcohol,
                      total = cancer_alcohol + control_alcohol)

# Tabaco (cigarrillos por d�a)
# 0
# 1-19
# 20-39
# 40 o m�s
tabaco <- data.frame(casos = cancer_tabaco, controles = control_tabaco, 
                     total = cancer_tabaco + control_tabaco)


#-------------------------------------------------------------------------------
# PREGUNTA 1
#
# Estudios previos hab�an determinado que la incidencia de c�ncer oral en la
# poblaci�n general que bebe regularmente entre 1 y 9 ml de alcohol era de 35%.
# �Respaldan estos datos tal estimaci�n?
# 
# H0: p = 0.35
# HA: p =/= 0.35
# 
# Nivel de significancia = 95%
# Caso de �xito: caso de c�ncer

exito <- alcohol[2,]$casos
n <- alcohol[2,]$total


alfa <- 0.05
prop <- prop.test(exito, n = n, p = 0.35, alternative = "two.sided", 
                  conf.level = 1-alfa)
prop

# Con un nivel de significancia del 99%, la proporci�n obtenida con la prueba se
# encuentra dentro del intervalo de confianza con p-valor mayor a la 
# significancia por lo tanto los datos respaldan la estimaci�n.

alfa <- 0.01
prop <- prop.test(exito, n = n, p = 0.35, alternative = "two.sided", 
                  conf.level = 1-alfa)
prop
# Con un nivel de significancia del 99%, la proporci�n obtenida con la prueba se
# encuentra dentro del intervalo de confianza con p-valor mayor a la 
# significancia por lo tanto los datos respaldan la estimaci�n.

#-------------------------------------------------------------------------------
# PREGUNTA 2
# 
# Seg�n estos datos, �da lo mismo beber de 1 y 9 ml de alcohol diariamente que
# hacerlo de 10 a 44 ml?
# 
# H0: p1 =   p2   �    p1 - p2 = 0
# HA: p1 =/= p2   �    p1 - p2 =/= 0
# p1: proporci�n de casos de c�ncer v/s tama�o de la muestra de 1-9ml de alcohol
# p2: proporci�n de casos de c�ncer v/s tama�o de la muestra de 10-44ml de alcohol
# Caso de ex�to: caso de cancer

exito <- c(alcohol[2,]$casos, alcohol[3,]$casos)
n <- c(alcohol[2,]$total, alcohol[3,]$total)

alfa <- 0.05
prop <- prop.test(exito, n = n, alternative = "two.sided", 
                  conf.level = 1-alfa)
prop

# Con un nivel de significancia del 95%, el valor p es mucho menor al nivel
# de significancia por lo que se rechaza la hip�tesis nula.
# Se puede estimar que la proporci�n es distinta entre 1-9ml y 10-44 ml de alcohol.
#-------------------------------------------------------------------------------
# PREGUNTA 3
# Suponiendo que la diferencia en la proporci�n de personas que desarrollan la
# enfermedad entre quienes beben de 1 a 9 ml de alcohol por d�a y aquellos que
# beben de 10 a 44 ml al d�a es de 0.10. �Cu�nta gente deber�amos monitorear
# para obtener un intervalo de confianza del 95% y poder estad�stico de 80%? si
# se intenta mantener aproximadamente la misma proporci�n de gente estudiada
# en cada caso.
#
diferencia <- 0.1
alfa <- 0.05
poder <- 0.8
prop <- pwr.2p.test(diferencia, NULL, alfa, poder, alternative="two.sided")
prop

# Con un nivel de significancia del 95%, el valor n para una diferencia de 0.1
# en las dos proporciones es de 1569.772.
# Se puede estimar que la el numero de personas a monitorear manteniendo mas o
# menos similares las personas estudiadas en cada caso es de 1570.