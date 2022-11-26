library(dplyr)

# Cargamos la base de datos del IGM con los puntos kilométricos
km <- read.csv( "puntosKilometricosSpain.csv")
# Cargamos la base de datos con nuestros nombres de carreteras y kilómetros
puntos <- read.csv("toyExample.csv")

# Cargamos la función
pkm2xy <- function(events, database){
  # Almacenaremos todas las carreteras que no se encuentren en nuestra base de datos
  noMatch <- events[!(events[,1] %in% database$Nombre),]
  # Filtramos nuestros datos
  events <- events[events[,1] %in% database$Nombre,]
  # Creamos una serie de columas para almacenar las coordenadas y otra info
  events$X <- NA
  events$Y <- NA
  events$error <- NA
  events$RoadAssigned <- NA
  events$PkmAssigned <- NA
  # Mediante un bucle iremos obteniendo nuestras coordenadas
  for(i in 1:nrow(events)){
    # Seleccionamos de nuestra base de datos aquel punto kilométrico que se
    # encuentre más cerca de nuestro kilómetro
    sel <- database %>% filter(Nombre ==  events[i,1]) %>% filter(abs(numero - events[i,2]) == min(abs(numero - events[i,2])))
    # Almacenamos las coordenadas
    events[i,3:4] <- sel[1,1:2]
    # Calculamos el error entre el punto kilométrico obtenido y el deseado
    events[i,5] <- abs(sel$numero[1] - events[i,2])
    # Almacenamos el nombre de la carretera y el punto kilométrico que hemos 
    # seleccionado en nuestra base de datos del IGN 
    events[i,6] <- sel$Nombre[1]
    events[i,7] <- sel$numero[1]
    print(i)
  }
  # Retornamos una lista con dos elementos:
  # Aquellos puntos para los cuales se han conseguido las coordenadas
  # Aquellos puntos para los que no se ha encontrado la carretera
  return(list(events,noMatch))
}

# Corremos nuestra función con el fichero de ejemplo
pkm2xy(puntos,km)



