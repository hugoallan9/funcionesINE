#'Función encargada de establecer la lista donde estarán alojadados los datos
#'para la cb

setListCB <- function(lista){
  pkg.env$cb <- lista
}


#'Función para obtener de nuevo la lista de datos para la cb

getListCB <- function(){
  return(pkg.env$cb)
}

#'Funcion para establecer la ruta en la cual se exportarán los csv
#'@param ruta Es la ruta dentro del disco duro donde se almacenaran los csv
setPath <- function(ruta){
  pkg.env$rutaCB <- ruta
}

#'Funcion para obtener la ruta en la cual se exportan los csv

getPath <- function(){
  return(pkg.env$rutaCB)
}

#'Funcion para hacer las graficas del capitulo 1
capitulo1CB <- function(){
  anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  cuatroEtiquetas()
  g1<- graficaLinea(getListIpc()$"1_01")
  exportarLatex(paste(getPath() , "1_01.tex", sep = "/"),g1)
  g2<- graficaLinea(getListIpc()$"1_02", precision = 2)
  exportarLatex(paste(getPath() , "1_02.tex", sep = "/"),g2)
}


#'Fucnion para hacer el capitulo 2

capitulo2CB <- function(){
  trimestral()
  cuatroEtiquetas()
  print("Este es el capitulo2")
  print(getListCB()$"2_03")
  print(getListCB())
  
  g<- graficaLinea(getListCB()$"2_01")
  exportarLatex(paste(getPath(),"2_01Alt.tex",sep="/"),g, preambulo = T )
  

}
