## Acceder al repositorio donde se encuentran los archivos csv
setwd("C:/Users/Mara/Desktop/EjerciciosIA/Proyecto/SombreroHP/recursos")

## Leer archivos csv
datos_pre = read.csv("preguntas.csv")
datos_res = read.csv("respuestas.csv")


##Para hacer las preguntas
p = 10 #preguntas a realizar
preguntas = sample(1:nrow(datos_pre),10) #Almacenar numeros de preguntas
respuestas = vector("character",10) #Creamos vector para despues almacenar respuestas
for(i in 1:p){
  pregunta = levels(drop.levels(datos_pre[preguntas[i],2])) #Accedemos a la descripción de la pregunta
  respuestas[i] = readline(pregunta) #Almacenamos las respuestas del usuario
}