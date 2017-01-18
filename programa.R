## Acceder al repositorio donde se encuentran los archivos csv
setwd("C:/Users/Ekaitz/Desktop/Dropbox/InteligenciaArtificial/SombreroHP/recursos")
#setwd("C:/Users/Mara/Desktop/EjerciciosIA/Proyecto/SombreroHP/recursos")
library(e1071)

## Leer archivos csv
datos_pre = read.csv("preguntas.csv")
datos_res = read.csv("respuestas.csv")

casaPersonaje =  function(casa){
  if(casa=="Griffindor"){
    return (1)
  }else if(casa=="Hufflepuff"){
    return (2)
  }else if(casa=="Ravenclaw"){
    return (3)
  }else if(casa=="Slytherin"){
    return (4)
  }
}

# Antes de empezar con el tratamiento de datos,
# se preparan las respuestas que los personajes de Harry Potter han dado a las preguntas

for(i in 1:nrow(datos_res)){
  # Comprobamos si hay filas con resultados en blanco (NA)
  if(anyNA(datos_res[i,]) == TRUE){
    listNA <- which(is.na(datos_res[i,]))
    # Si hay algun valor NA miramos lo que han respondido sus otros compañeros de casa
    for(j in 1:length(listNA)){
      numNO <- unname(table(datos_res[,listNA[j]],datos_res$casa)[,casaPersonaje(datos_res[i,]$casa)][1])
      numSI <- unname(table(datos_res[,listNA[j]],datos_res$casa)[,casaPersonaje(datos_res[i,]$casa)][2])
      # Le asinamos a ese personaje la respuesta mayoritaria de la casa
      # Si hay empate que escoja SI o NO al azar
      if(numNO<numSI){
        datos_res[i,listNA[j]] = "SI";
      }else if(numNO>numSI){
        datos_res[[i,listNA[j]]] = "NO";
      }else if(numNO==numSI){
        sample(c("SI","NO"),1)
      }
    }
  }
}



##Para hacer las preguntas
p = 10 #preguntas a realizar
preguntas = sort(sample(1:nrow(datos_pre),p)) #Almacenar numeros de preguntas
#respuestas = vector("character",dim(datos_pre)[1]) #Creamos vector para despues almacenar respuestas (Proceso 1)
respuestas = vector("character",p)  #(Proceso 2)
#respuestas[1:dim(datos_pre)[1]]=NA #(Proceso 1)
i = 1;
while(i<=p){
  pregunta = levels(droplevels(datos_pre[preguntas[i],2])) #Accedemos a la descripciÃ³n de la pregunta
  respuesta = readline(pregunta); #Almacenamos las respuestas del usuario
  if(respuesta == "SI" || respuesta == "NO"){
    #respuestas[preguntas[i]] = respuesta;  #(Proceso 1)
    respuestas[i] = respuesta;  #(Proceso 2)
    i = i + 1;
  } else {
    print("Debes responder SI o NO a la pregunta");
  }
}

# - Proceso 1 -

#respuestas = t(replicate(4, respuestas))

#Rellenamos los huecos vacíos con la ocurrencia mayoritaria en su casa
#for(x in 1:dim(datos_pre)[1]){
#  if(is.na(respuestas[[1,x]])){
#    countCasas = table(datos_res[[x+2]],datos_res$casa);
#    for(y in 1:4){
#      if(countCasas[2*y-1] > countCasas[2*y]){
#        respuestas[y,x] = "NO";
#      } else {
#        respuestas[y,x] = "SI";
#      }
#    }
#    summary(datos_res$casa)
#  }
#}

#respuestas = as.data.frame((respuestas), stringsAsFactors=TRUE);



#model=naiveBayes(datos_res[,3:dim(datos_res)[2]], datos_res[,2], laplace = 1);
#summary(model)
#respuestas =as.data.frame((respuestas))
#colnames(respuestas)=colnames(datos_res)[3:dim(datos_res)[2]]
#out = predict(model, respuestas, type="raw");
#out

#mediaCasa1 = mean(out[1:4]);
#mediaCasa2 = mean(out[5:8]);
#mediaCasa3 = mean(out[9:12]);
#mediaCasa4 = mean(out[13:16]);

# - Proceso 2 -

# Tabla con los datos de los personajes en base a las respuestas del usuario
datos_tratar = datos_res[,1:2]
datos_tratar = cbind(datos_tratar, datos_res[, (preguntas+2)])

model=naiveBayes(datos_tratar[,3:12], datos_tratar[,2], laplace = 1);
summary(model)
respuestas =as.data.frame(t(respuestas));
colnames(respuestas)=colnames(datos_tratar)[3:12]
out = predict(model, respuestas, type="raw");
out 



#Para el proceso final, revisar la opcion mayoritaria de cada casa en la tabla
#Si esa es diferente entre las dos opciones, esa será la pregunta a realizar




