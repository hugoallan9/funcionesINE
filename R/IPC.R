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
  exportarLatex(paste(getPath() , "1_01.tex", sep = "/"),g1)
  g2<- graficaLinea(getListIpc()$"1_02")
  exportarLatex(paste(getPath() , "1_02.tex", sep = "/"),g2)
  g3<- graficaLinea(getListIpc()$"1_03")
  exportarLatex( paste(getPath() , "1_03.tex", sep="/"),g3)
  g4<- graficaLinea(getListIpc()$"1_04")
  exportarLatex( paste(getPath() , "1_04.tex", sep = "/"),g4)
  g5<- graficaLinea(getListIpc()$"1_05")
  exportarLatex( paste(getPath() , "1_05.tex", sep = "/"),g5)
  g6<- graficaLinea(getListIpc()$"1_06")
  exportarLatex(paste(getPath() , "1_06.tex", sep = "/"),g6)
}


#'Fucnion para hacer el capitulo 2

capitulo2 <- function(){
  print("Este es el capitulo2")
  cuatroEtiquetas()
  anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  g7<- graficaLinea(getListIpc()$"2_02")
  exportarLatex(paste(getPath(),"2_02.tex",sep="/"),g7)
  
  g7777<- graficaBar(getListIpc()$"2_03")
  g7777 <- etiquetasBarras(g7777)
  exportarLatex(paste( getPath(), "2_03.tex", sep = "/"),g7777)
  
  g9<- graficaLinea(getListIpc()$"2_06")
  exportarLatex(paste( getPath(), "2_06.tex", sep = "/"),g9)
  
  g12<- graficaBar(getListIpc()$"2_07")
  g12<- etiquetasBarras(g12)
  exportarLatex(paste( getPath(), "2_07.tex", sep = "/"),g12)
  
  g18<- graficaBar(getListIpc()$"2_08")
  g18 <- etiquetasBarras(g18)
  exportarLatex(paste( getPath(), "2_08.tex", sep = "/"),g18)
  
  g19<- graficaBar(getListIpc()$"2_09")
  g19 <- etiquetasBarras(g19)
  exportarLatex(paste( getPath(), "2_09.tex", sep = "/"),g19)
  
  g200<- graficaLinea(getListIpc()$"2_10")
  exportarLatex(paste( getPath(), "2_10.tex", sep = "/"),g200)
  
  g202<- graficaLinea(getListIpc()$"2_11")
  exportarLatex(paste( getPath(), "2_11.tex", sep = "/"),g202)
  
  g203<- graficaLinea(getListIpc()$"2_12")
  exportarLatex(paste( getPath(), "2_12.tex", sep = "/"),g203)
  
  g204<- graficaLinea(getListIpc()$"2_13")
  exportarLatex(paste( getPath(), "2_13.tex", sep = "/"),g204)
  
  g205<- graficaLinea(getListIpc()$"2_14")
  exportarLatex(paste( getPath(), "2_14.tex", sep = "/"),g205)
  
  g206<- graficaLinea(getListIpc()$"2_15")
  exportarLatex(paste( getPath(), "2_15.tex", sep = "/"),g206)
  
  g207<- graficaLinea(getListIpc()$"2_16")
  exportarLatex(paste( getPath(), "2_16.tex", sep = "/"),g207)
  
  g208<- graficaLinea(getListIpc()$"2_17")
  exportarLatex(paste( getPath(), "2_17.tex", sep = "/"),g208)
  
  g209<- graficaLinea(getListIpc()$"2_18")
  exportarLatex(paste( getPath(), "2_18.tex", sep = "/"),g209)
  
  g110<- graficaLinea(getListIpc()$"2_19")
  exportarLatex(paste( getPath(), "2_19.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"2_20")
  exportarLatex(paste( getPath(), "2_20.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"2_21")
  exportarLatex(paste( getPath(), "2_21.tex", sep = "/"),g112)
  
  g301<- graficaBar(getListIpc()$"2_23")
  g301<- etiquetasBarras(g301)
  exportarLatex(paste( getPath(), "2_23.tex", sep = "/"),g301)
  
  g17<- graficaBar(getListIpc()$"2_24")
  g17 <- etiquetasBarras(g17)
  exportarLatex(paste( getPath(), "2_24.tex", sep = "/"),g17)
  

  g13<- graficaLinea(getListIpc()$"2_27")
  exportarLatex(paste( getPath(), "2_27.tex", sep = "/"),g13)
  
  g16<- graficaBar(getListIpc()$"2_28")
  g16 <- etiquetasBarras(g16)
  exportarLatex(paste( getPath(), "2_28.tex", sep = "/"),g16)
  
  g22<- graficaBar(getListIpc()$"2_29")
  g22 <- etiquetasBarras(g22)
  exportarLatex(paste( getPath(), "2_29.tex", sep = "/"),g22)
  
  g23<- graficaBar(getListIpc()$"2_30")
  g23 <- etiquetasBarras(g23)
  exportarLatex(paste( getPath(), "2_30.tex", sep = "/"),g23)
  
  g100<- graficaLinea(getListIpc()$"2_31")
  exportarLatex(paste( getPath(), "2_31.tex", sep = "/"),g100)
  
  g102<- graficaLinea(getListIpc()$"2_32")
  exportarLatex(paste( getPath(), "2_32.tex", sep = "/"),g102)
  
  g103<- graficaLinea(getListIpc()$"2_33")
  exportarLatex(paste( getPath(), "2_33.tex", sep = "/"),g103)
  
  g104<- graficaLinea(getListIpc()$"2_34")
  exportarLatex(paste( getPath(), "2_34.tex", sep = "/"),g104)
  
  g105<- graficaLinea(getListIpc()$"2_35")
  exportarLatex(paste( getPath(), "2_36.tex", sep = "/"),g105)
  
  g106<- graficaLinea(getListIpc()$"2_36")
  exportarLatex(paste( getPath(), "2_36.tex", sep = "/"),g106)
  
  g107<- graficaLinea(getListIpc()$"2_37")
  exportarLatex(paste( getPath(), "2_37.tex", sep = "/"),g107)
  
  g108<- graficaLinea(getListIpc()$"2_38")
  exportarLatex(paste( getPath(), "2_38.tex", sep = "/"),g108)
  
  g109<- graficaLinea(getListIpc()$"2_33")
  exportarLatex(paste( getPath(), "2_33.tex", sep = "/"),g109)
  
  g110<- graficaLinea(getListIpc()$"2_39")
  exportarLatex(paste( getPath(), "2_39.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"2_40")
  exportarLatex(paste( getPath(), "2_40.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"2_41")
  exportarLatex(paste( getPath(), "2_41.tex", sep = "/"),g112)
  
  g302<- graficaBar(getListIpc()$"2_42")
  g302<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "2_42.tex", sep = "/"),g302)
  
  g303<- graficaBar(getListIpc()$"2_44")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "2_44.tex", sep = "/"),g303)
  
  g303<- graficaBar(getListIpc()$"2_45")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "2_45.tex", sep = "/"),g303)
  
  g111<- graficaLinea(getListIpc()$"2_47")
  exportarLatex(paste( getPath(), "2_47.tex", sep = "/"),g111)
  
}