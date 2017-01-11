## Acceder al repositorio donde se encuentran los archivos csv
setwd("C:/Users/Ekaitz/Desktop/Dropbox/InteligenciaArtificial/SombreroHP/recursos")
library(e1071)

## Leer archivos csv
datos_pre = read.csv("preguntas.csv")
datos_res = read.csv("respuestas.csv")


##Para hacer las preguntas
p = 10 #preguntas a realizar
preguntas = sort(sample(1:nrow(datos_pre),p)) #Almacenar numeros de preguntas
respuestas = vector("character",p) #Creamos vector para despues almacenar respuestas
i = 1;
while(i<p){
  pregunta = levels(droplevels(datos_pre[preguntas[i],2])) #Accedemos a la descripción de la pregunta
  respuesta = readline(pregunta); #Almacenamos las respuestas del usuario
  if(respuesta == "SI" || respuesta == "NO"){
    respuestas[i] = respuesta;
    i = i + 1;
  } else {
    print("Debes responder SI o NO a la pregunta");
  }
}

# Tabla con los datos de los personajes en base a las respuestas del usuario
datos_tratar = datos_res[,1:2]
datos_tratar = cbind(datos_tratar, datos_res[, (preguntas+2)])

model=naiveBayes(datos_tratar[,3:12], datos_tratar[,2], laplace = 1);
summary(model)
respuestas =as.data.frame(t(respuestas))
out = predict(model, respuestas, type="raw");
out