#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#                     Enunciado 7, desarrollo de sala 2

#           Integrantes:                        RUT:              
#        -Carlos Castillo                   20.201.274-4
#        -Francisco Moreno                  19.892.183-1
#        -Gianfranco Piccini                20.237.081-0
#        -Joaqu�n Torres                    19.091.702-9
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                           PREGUNTA 1
#-------------------------------------------------------------------------------
#Una de las primeras preguntas a responder por el �ltimo estudio nacional sobre 
#obesidad infantil fue si exist�an diferencias en la prevalencia de esta 
#enfermedad entre ni�as y ni�os o si, por el contrario, la obesidad infantil 
#no var�a entre sexos. De forma muy preliminar, en una muestra reducida, se ha
#encontrado 1 ni�a obesa en 5 reclutadas, mientras que 7 ni�os de un total 
#de 9 resultaron obesos.
#�Parece haber tener el sexo incidencia en la prevalencia de obesidad infantil?

#Respuesta:
#De acuerdo a la pregunta, se plante� las siguiente hip�tesis para contrastarlo:
#H0: El sexo y la obesidad infantil son independientes.
#HA: El se�o y la obesidad infantil estan relacionados.

#Entonces, lo que se quiere verificar es que si existe incidencia el sexo en 
# la prevalencia de obesidad infantil, o bien, si existe una independencia
#entre la obesidad infantil y ambos sexos. 
#Para ello, se har� el uso de la prueba exacta de fisher para 
#determinar la hip�tesis definitiva. 

#Carga de los datos.
obesidad <- c(rep("Obeso/a", 8) , rep("No obeso/a", 6) )
infantil <- c(rep ("ni�o", 9) , rep ("ni�a", 5) )
datos <- data.frame ( infantil , obesidad )
tabla <- xtabs (~. , datos )
print ( tabla )

#Aplicaci�n prueba exacta de Fisher .
alfa <- 0.05
prueba <- fisher.test ( tabla , 1 - alfa )
print ( prueba )

#De acuerdo al resultado obtenido de p, siendo menos al nivel de significancia, 
#es decir, p0<alfa = 0.002997<0.05 . Entonces, se pude concluir que se rechaza 
#la hipotesis nula a favor de la alternativa. Por lo tanto, con un 95% de 
#confianza, se concluye que la obesidad infantil entre ambos sexos estan
#relacionados, o bien, el sexo tiene incidencia en la prevalencia de la obesidad
#infantil.
  
#-------------------------------------------------------------------------------
#                           PREGUNTA 2
#-------------------------------------------------------------------------------
#Uno de los estudios esenciales en la determinaci�n de la relaci�n entre fumar 
#y el c�ncer de pulm�n reclut� a 100 pares de gemelos monocigotos (todos varones) 
#en que solo uno de los hermanos desarroll� la enfermedad mientras que el otro no. 
#Se encuest� a cada uno de los gemelos para determinar, entre otras cosas, 
#si fumaban tabaco. La muestra registr� 16 pares de gemelos en que ambos fumaban 
#y 59 pares en que ambos no fumaban. En 21 pares solo el gemelo con c�ncer 
#fumaba, mientras que en 4 pares solo fumaba el gemelo sano. 

#Respuesta:
#De acuerdo a la pregunta, se plante� las siguiente hip�tesis para contrastarlo:
#H0: No hay cambios significativos en la respuesta.
#HA: Hay cambios significativo en la respuesta.

#Entonces, lo que se quiere verificar es que si ocurre o no un cambio 
#significativo entre las mediciones de los 100 pares de gemelos sanos y con cancer, 
#viendo si existe una diferencia en fumar para ambos hermanos. Para ello, 
#se har� el uso de la prueba exacta de McNemar para determinar la hip�tesis 
#definitiva. 

# Se obtienen los datos mencionados, construyendo la tabla a evaluar.
pares_gemelos <- seq (1:100)
sano <- c(rep("Fuma", 16) , rep("No fuma", 59), rep("No fuma", 21), rep("Fuma", 4))
cancer <- c(rep("Fuma", 16) , rep("No fuma", 59) , rep("Fuma", 21), rep("No fuma", 4))
datos <- data.frame ( pares_gemelos , sano, cancer)
tabla <- table ( sano , cancer)
print ( tabla )

# Se aplica la prueba de McNemar .
prueba <- mcnemar.test ( tabla )
print ( prueba )

#De acuerdo al resultado obtenido de p, siendo menor al nivel de significancia, 
#es decir, p0<alfa = 0.002997<0.05, siendo 0.05 el nivel de significancia que 
#viene por defecto en la funcion mcnemar.test. Entonces, se pude concluir que 
#se rechaza la hipotesis nula a favor de la alternativa. Por lo tanto, 
#con un 95% de confianza, se concluye que hay un cambio significativo en la 
#medicion entre los pares de gemelos sanos y con cancer que fuman o no.

#-------------------------------------------------------------------------------
#                           PREGUNTA 3
#-------------------------------------------------------------------------------
#Estudios sobre las creencias de los estadounidenses acerca del origen y 
#desarrollo de los seres humanos se llevan haciendo desde hace d�cadas. 
#En la �ltima encuesta, se presentaron las siguientes opciones:
  
#  (a) Human beings have developed over millions of years from less advanced 
#      forms of life, but God guided this process
#  (b) Human beings have developed over millions of years from less advanced 
#      forms of life, but God had no part in this process
#  (c) God created human beings pretty much in their present form at one time 
#      within the last 10,000 years or so 
      
#1.019 personas fueron consultadas sobre cu�l de las opciones anteriores 
#representaba mejor su punto de vista. 387 personas se inclinaron por la 
#opci�n 1, 171 por la opci�n 2, 400 por la opci�n 3 y 61 no supieron o no 
#quisieron responder.

#En el a�o 2007, esta misma encuesta registr� las siguientes proporciones: 
#38% opci�n 1, 14% opci�n 2, 43% opci�n 3. �Han cambiado las creencias de 
#los estadounidenses acerca del origen y desarrollo de los seres humanos desde 
#2007?


# Respuesta:
# Sea P1 el vector que representa las probabilidades de alternativas entregadas 
# por una encuesta realizada sobre una poblaci�n representativa el a�o 2017; y
# P2 el vector que representa las probabilidades de alternativas entregadas 
# por una encuesta realizada sobre una poblaci�n representativa el a�o 2021.
#
# H0: La proporci�n de alternativas entre las poblaciones de 2017 y 2021 son las
# mismas (P1 - P2 = 0).
# HA: La proporci�n de alternativas entre las poblaciones de 2017 y 2021 es
# distinta. (P1 - P2 =/= 0)
#
# Se considera que los sujetos encuestados son distintos pero el mismo numero de
# observaciones; esto es puesto que al tomar muestras representativas lo que
# importa es el numero y zona de encuestados, no que sea la misma persona a
# responder (son independientes entre s�).
# Hay a lo menos 5 observaciones en cada grupo (Hay alo menos mil)
# Como se cumplen estas dos observaciones podemos aplicar la prueba chi cuadrado
# de homogeneidad, para asegurar si dos poblaciones presentan la misma 
# proporci�n en las respuestas.
# Se consideran alrededor de 1000 encuestados en cada muestra.

encuesta_nueva <- c(387/1019*100, 171/1019*100, 400/1019*100)
encuesta_vieja <- c(38, 14, 43)

tabla_encuestas <- as.table(rbind(encuesta_vieja, encuesta_nueva))

dimnames(tabla_encuestas) <- list(fecha = c("2017", "2021"),
                                  alternativas = c("1","2","3"))

print(tabla_encuestas)

# Hacer prueba chi-cuadrado de homogeneidad
chi_test <- chisq.test(tabla_encuestas)
print(chi_test)

# El valor p obtenido de la prueba chi-cuadrado es de 0.8119. Suponiendo un nivel
# de significaci�n de 5%, p > 0.05, por lo que se falla al rechazar la hip�tesis
# nula; es decir, no hay evidencia suficiente con 95% de confianza para aseverar
# que la proporci�n de la respuesta a la encuesta es distinta entre la poblaci�n
# de los a�os 2017 y 2021, en otras palabras las creencias de los estadounidenses
# no han cambiado entre estos dos a�os sobre el origen y desarrollo de los seres
# humanos.
  
#-------------------------------------------------------------------------------
#                           PREGUNTA 4
#-------------------------------------------------------------------------------
#Enuncie un ejemplo novedoso (no discutido en clase ni en las lecturas) 
#relacionado con la opini�n del alumnado sobre la vuelta a clases presenciales 
#durante la segunda mitad del semestre 2/2021 que requiera utilizar una prueba Q 
#de Cochran. Identifique las variables involucradas y las hip�tesis a contrastar.