#'Función encargada de establecer la lista donde estarán alojadados los datos
#'para el IPC

setListIpc <- function(lista){
  pkg.env$ipc <- lista
}


#'Función para obtener de nuevo la lista de datos para el ipc

getListIpc <- function(){
  return(pkg.env$ipc)
}