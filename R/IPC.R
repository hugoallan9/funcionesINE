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

#'Funcion para hacer las graficas del capitulo 1
capitulo1 <- function(){
  anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  g1<- graficaLinea(getListIpc()$"1_01")
  exportarLatex(paste(getPath() , "0_01.tex", sep = "/"),g1)
  g2<- graficaLinea(getListIpc()$"1_02")
  exportarLatex(paste(getPath() , "0_02.tex", sep = "/"),g2)
  g3<- graficaLinea(getListIpc()$"1_03")
  exportarLatex( paste(getPath() , "0_03.tex", sep="/"),g3)
  g4<- graficaLinea(getListIpc()$"1_04")
  exportarLatex( paste(getPath() , "0_04.tex", sep = "/"),g4)
  g5<- graficaLinea(getListIpc()$"1_05")
  exportarLatex( paste(getPath() , "0_05.tex", sep = "/"),g5)
  g6<- graficaLinea(getListIpc()$"1_06")
  exportarLatex(paste(getPath() , "0_06.tex", sep = "/"),g6)
}


#'Fucnion para hacer el capitulo 2

capitulo2 <- function(){
  anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  g7<- graficaLinea(getListIpc()$"1_01")
  exportarLatex(paste(getPath(),"2_01.tex",sep="/"),g7, preambulo = T)
  compilar(paste(getPath(),"2_01.tex",sep="/"))
}