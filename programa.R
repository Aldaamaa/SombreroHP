## Acceder al repositorio donde se encuentran los archivos csv
setwd("C:/Users/Mara/Desktop/EjerciciosIA/Proyecto/SombreroHP/recursos")

## Leer archivos csv
datos_pre = read.csv("preguntas.csv")
datos_res = read.csv("respuestas.csv")


##Para hacer las preguntas
p = 10 #preguntas a realizar
preguntas = sort(sample(1:nrow(datos_pre),10)) #Almacenar numeros de preguntas
respuestas = vector("character",10) #Creamos vector para despues almacenar respuestas
for(i in 1:p){
  pregunta = levels(drop.levels(datos_pre[preguntas[i],2])) #Accedemos a la descripción de la pregunta
  respuestas[i] = readline(pregunta) #Almacenamos las respuestas del usuario
}

# Tabla con los datos de los personajes en base a las respuestas del usuario
datos_tratar = datos_res[,1:2]
datos_tratar = cbind(datos_tratar, datos_res[, (preguntas+2)])

