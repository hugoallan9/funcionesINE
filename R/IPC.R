#'Función encargada de establecer la lista donde estarán alojadados los datos
#'para el IPC

setListIpc <- function(lista){
  pkg.env$ipc <- lista
}


#'Función para obtener de nuevo la lista de datos para el ipc

getListIpc <- function(){
  return(pkg.env$ipc)
}

#'Funcion para establecer la ruta en la cual se exportarán los csv
#'@param ruta Es la ruta dentro del disco duro donde se almacenaran los csv
setPath <- function(ruta){
  pkg.env$rutaIPC <- ruta
}

#'Funcion para obtener la ruta en la cual se exportan los csv

getPath <- function(){
  return(pkg.env$rutaIPC)
}