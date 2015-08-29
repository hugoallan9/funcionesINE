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
  
  g9<- graficaLinea(getListIpc()$"2_06", precision = 2)
  exportarLatex(paste( getPath(), "2_06.tex", sep = "/"),g9)
  
  g12<- graficaBar(getListIpc()$"2_07")
  g12<- etiquetasBarras(g12, precision = 2)
  exportarLatex(paste( getPath(), "2_07.tex", sep = "/"),g12)
  
  g18<- graficaBar(getListIpc()$"2_08")
  g18 <- etiquetasBarras(g18)
  exportarLatex(paste( getPath(), "2_08.tex", sep = "/"),g18)
  
  g19<- graficaBar(getListIpc()$"2_09")
  g19 <- etiquetasBarras(g19)
  exportarLatex(paste( getPath(), "2_09.tex", sep = "/"),g19)
  
  g200<- graficaLinea(getListIpc()$"2_10", precision = 2)
  exportarLatex(paste( getPath(), "2_10.tex", sep = "/"),g200)
  
  g202<- graficaLinea(getListIpc()$"2_11", precision = 2)
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
  exportarLatex(paste( getPath(), "2_35.tex", sep = "/"),g105)
  
  g106<- graficaLinea(getListIpc()$"2_36")
  exportarLatex(paste( getPath(), "2_36.tex", sep = "/"),g106)
  
  g107<- graficaLinea(getListIpc()$"2_37")
  exportarLatex(paste( getPath(), "2_37.tex", sep = "/"),g107)
  
  g108<- graficaLinea(getListIpc()$"2_38")
  exportarLatex(paste( getPath(), "2_38.tex", sep = "/"),g108)
  
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


#'Fucnion para hacer el capitulo 3

capitulo3 <- function(){
  print("Este es el capitulo3")
  cuatroEtiquetas()
  anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  g7<- graficaLinea(getListIpc()$"3_02")
  exportarLatex(paste(getPath(),"3_02.tex",sep="/"),g7)
  
  g7777<- graficaBar(getListIpc()$"3_03")
  g7777 <- etiquetasBarras(g7777)
  exportarLatex(paste( getPath(), "3_03.tex", sep = "/"),g7777)
  
  g9<- graficaLinea(getListIpc()$"3_05")
  exportarLatex(paste( getPath(), "3_05.tex", sep = "/"),g9)
  
  g12<- graficaBar(getListIpc()$"3_06")
  g12<- etiquetasBarras(g12)
  exportarLatex(paste( getPath(), "3_06.tex", sep = "/"),g12)
  
  g18<- graficaBar(getListIpc()$"3_07")
  g18 <- etiquetasBarras(g18)
  exportarLatex(paste( getPath(), "3_07.tex", sep = "/"),g18)
  
  g19<- graficaBar(getListIpc()$"3_08")
  g19 <- etiquetasBarras(g19)
  exportarLatex(paste( getPath(), "3_08.tex", sep = "/"),g19)
  
  g200<- graficaLinea(getListIpc()$"3_09")
  exportarLatex(paste( getPath(), "3_09.tex", sep = "/"),g200)
  
  g202<- graficaLinea(getListIpc()$"3_10")
  exportarLatex(paste( getPath(), "3_10.tex", sep = "/"),g202)
  
  g203<- graficaLinea(getListIpc()$"3_11")
  exportarLatex(paste( getPath(), "3_11.tex", sep = "/"),g203)
  
  g204<- graficaLinea(getListIpc()$"3_12")
  exportarLatex(paste( getPath(), "3_12.tex", sep = "/"),g204)
  
  g205<- graficaLinea(getListIpc()$"3_13")
  exportarLatex(paste( getPath(), "3_13.tex", sep = "/"),g205)
  
  g206<- graficaLinea(getListIpc()$"3_14")
  exportarLatex(paste( getPath(), "3_14.tex", sep = "/"),g206)
  
  g207<- graficaLinea(getListIpc()$"3_15")
  exportarLatex(paste( getPath(), "3_15.tex", sep = "/"),g207)
  
  g208<- graficaLinea(getListIpc()$"3_16")
  exportarLatex(paste( getPath(), "3_16.tex", sep = "/"),g208)
  
  g209<- graficaLinea(getListIpc()$"3_17")
  exportarLatex(paste( getPath(), "3_17.tex", sep = "/"),g209)
  
  g110<- graficaLinea(getListIpc()$"3_18")
  exportarLatex(paste( getPath(), "3_18.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"3_19")
  exportarLatex(paste( getPath(), "3_19.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"3_20")
  exportarLatex(paste( getPath(), "3_20.tex", sep = "/"),g112)
  
  g301<- graficaBar(getListIpc()$"3_21")
  g301<- etiquetasBarras(g301)
  exportarLatex(paste( getPath(), "3_21.tex", sep = "/"),g301)
  
  g17<- graficaBar(getListIpc()$"3_22")
  g17 <- etiquetasBarras(g17)
  exportarLatex(paste( getPath(), "3_22.tex", sep = "/"),g17)
  
  
  g13<- graficaLinea(getListIpc()$"3_24")
  exportarLatex(paste( getPath(), "3_24.tex", sep = "/"),g13)
  
  g16<- graficaBar(getListIpc()$"3_25")
  g16 <- etiquetasBarras(g16)
  exportarLatex(paste( getPath(), "3_25.tex", sep = "/"),g16)
  
  g22<- graficaBar(getListIpc()$"3_26")
  g22 <- etiquetasBarras(g22)
  exportarLatex(paste( getPath(), "3_26.tex", sep = "/"),g22)
  
  g23<- graficaBar(getListIpc()$"3_27")
  g23 <- etiquetasBarras(g23)
  exportarLatex(paste( getPath(), "3_27.tex", sep = "/"),g23)
  
  g100<- graficaLinea(getListIpc()$"3_28")
  exportarLatex(paste( getPath(), "3_28.tex", sep = "/"),g100)
  
  g102<- graficaLinea(getListIpc()$"3_29")
  exportarLatex(paste( getPath(), "3_29.tex", sep = "/"),g102)
  
  g103<- graficaLinea(getListIpc()$"3_30")
  exportarLatex(paste( getPath(), "3_30.tex", sep = "/"),g103)
  
  g104<- graficaLinea(getListIpc()$"3_31")
  exportarLatex(paste( getPath(), "3_31.tex", sep = "/"),g104)
  
  g105<- graficaLinea(getListIpc()$"3_32")
  exportarLatex(paste( getPath(), "3_32.tex", sep = "/"),g105)
  
  g106<- graficaLinea(getListIpc()$"3_33")
  exportarLatex(paste( getPath(), "3_33.tex", sep = "/"),g106)
  
  g107<- graficaLinea(getListIpc()$"3_34")
  exportarLatex(paste( getPath(), "3_34.tex", sep = "/"),g107)
  
  g108<- graficaLinea(getListIpc()$"3_35")
  exportarLatex(paste( getPath(), "3_35.tex", sep = "/"),g108)
  
  g110<- graficaLinea(getListIpc()$"3_36")
  exportarLatex(paste( getPath(), "3_36.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"3_37")
  exportarLatex(paste( getPath(), "3_37.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"3_38")
  exportarLatex(paste( getPath(), "3_38.tex", sep = "/"),g112)
  
  g302<- graficaBar(getListIpc()$"3_39")
  g302<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "3_39.tex", sep = "/"),g302)
  
  g303<- graficaBar(getListIpc()$"3_40")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "3_40.tex", sep = "/"),g303)
  
  g303<- graficaBar(getListIpc()$"3_41")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "3_41.tex", sep = "/"),g303)
  
  g111<- graficaLinea(getListIpc()$"3_42")
  exportarLatex(paste( getPath(), "3_42.tex", sep = "/"),g111)
  
}

#'Fucnion para hacer el capitulo 4

capitulo4 <- function(){
  print("Este es el capitulo4")
  cuatroEtiquetas()
  anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  g7<- graficaLinea(getListIpc()$"4_02")
  exportarLatex(paste(getPath(),"4_02.tex",sep="/"),g7)
  
  g7777<- graficaBar(getListIpc()$"4_03")
  g7777 <- etiquetasBarras(g7777)
  exportarLatex(paste( getPath(), "4_03.tex", sep = "/"),g7777)
  
  g9<- graficaLinea(getListIpc()$"4_05")
  exportarLatex(paste( getPath(), "4_05.tex", sep = "/"),g9)
  
  g12<- graficaBar(getListIpc()$"4_06")
  g12<- etiquetasBarras(g12)
  exportarLatex(paste( getPath(), "4_06.tex", sep = "/"),g12)
  
  g18<- graficaBar(getListIpc()$"4_07")
  g18 <- etiquetasBarras(g18)
  exportarLatex(paste( getPath(), "4_07.tex", sep = "/"),g18)
  
  g19<- graficaBar(getListIpc()$"4_08")
  g19 <- etiquetasBarras(g19)
  exportarLatex(paste( getPath(), "4_08.tex", sep = "/"),g19)
  
  g200<- graficaLinea(getListIpc()$"4_09")
  exportarLatex(paste( getPath(), "4_09.tex", sep = "/"),g200)
  
  g202<- graficaLinea(getListIpc()$"4_10")
  exportarLatex(paste( getPath(), "4_10.tex", sep = "/"),g202)
  
  g203<- graficaLinea(getListIpc()$"4_11")
  exportarLatex(paste( getPath(), "4_11.tex", sep = "/"),g203)
  
  g204<- graficaLinea(getListIpc()$"4_12")
  exportarLatex(paste( getPath(), "4_12.tex", sep = "/"),g204)
  
  g205<- graficaLinea(getListIpc()$"4_13")
  exportarLatex(paste( getPath(), "4_13.tex", sep = "/"),g205)
  
  g206<- graficaLinea(getListIpc()$"4_14")
  exportarLatex(paste( getPath(), "4_14.tex", sep = "/"),g206)
  
  g207<- graficaLinea(getListIpc()$"4_15")
  exportarLatex(paste( getPath(), "4_15.tex", sep = "/"),g207)
  
  g208<- graficaLinea(getListIpc()$"4_16")
  exportarLatex(paste( getPath(), "4_16.tex", sep = "/"),g208)
  
  g209<- graficaLinea(getListIpc()$"4_17")
  exportarLatex(paste( getPath(), "4_17.tex", sep = "/"),g209)
  
  g110<- graficaLinea(getListIpc()$"4_18")
  exportarLatex(paste( getPath(), "4_18.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"4_19")
  exportarLatex(paste( getPath(), "4_19.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"4_20")
  exportarLatex(paste( getPath(), "4_20.tex", sep = "/"),g112)
  
  g301<- graficaBar(getListIpc()$"4_21")
  g301<- etiquetasBarras(g301)
  exportarLatex(paste( getPath(), "4_21.tex", sep = "/"),g301)
  
  g17<- graficaBar(getListIpc()$"4_22")
  g17 <- etiquetasBarras(g17)
  exportarLatex(paste( getPath(), "4_22.tex", sep = "/"),g17)
  
  
  g13<- graficaLinea(getListIpc()$"4_24")
  exportarLatex(paste( getPath(), "4_24.tex", sep = "/"),g13)
  
  g16<- graficaBar(getListIpc()$"4_25")
  g16 <- etiquetasBarras(g16)
  exportarLatex(paste( getPath(), "4_25.tex", sep = "/"),g16)
  
  g22<- graficaBar(getListIpc()$"4_26")
  g22 <- etiquetasBarras(g22)
  exportarLatex(paste( getPath(), "4_26.tex", sep = "/"),g22)
  
  g23<- graficaBar(getListIpc()$"4_27")
  g23 <- etiquetasBarras(g23)
  exportarLatex(paste( getPath(), "4_27.tex", sep = "/"),g23)
  
  g100<- graficaLinea(getListIpc()$"4_28")
  exportarLatex(paste( getPath(), "4_28.tex", sep = "/"),g100)
  
  g102<- graficaLinea(getListIpc()$"4_29")
  exportarLatex(paste( getPath(), "4_29.tex", sep = "/"),g102)
  
  g103<- graficaLinea(getListIpc()$"4_30")
  exportarLatex(paste( getPath(), "4_30.tex", sep = "/"),g103)
  
  g104<- graficaLinea(getListIpc()$"4_31")
  exportarLatex(paste( getPath(), "4_31.tex", sep = "/"),g104)
  
  g105<- graficaLinea(getListIpc()$"4_32")
  exportarLatex(paste( getPath(), "4_32.tex", sep = "/"),g105)
  
  g106<- graficaLinea(getListIpc()$"4_33")
  exportarLatex(paste( getPath(), "4_33.tex", sep = "/"),g106)
  
  g107<- graficaLinea(getListIpc()$"4_34")
  exportarLatex(paste( getPath(), "4_34.tex", sep = "/"),g107)
  
  g108<- graficaLinea(getListIpc()$"4_35")
  exportarLatex(paste( getPath(), "4_35.tex", sep = "/"),g108)
  
  g110<- graficaLinea(getListIpc()$"4_36")
  exportarLatex(paste( getPath(), "4_36.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"4_37")
  exportarLatex(paste( getPath(), "4_37.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"4_38")
  exportarLatex(paste( getPath(), "4_38.tex", sep = "/"),g112)
  
  g302<- graficaBar(getListIpc()$"4_39")
  g302<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "4_39.tex", sep = "/"),g302)
  
  g303<- graficaBar(getListIpc()$"4_40")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "4_40.tex", sep = "/"),g303)
  
  g303<- graficaBar(getListIpc()$"4_41")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "4_41.tex", sep = "/"),g303)
  
  g111<- graficaLinea(getListIpc()$"4_42")
  exportarLatex(paste( getPath(), "4_42.tex", sep = "/"),g111)
  
}

#'Fucnion para hacer el capitulo 5

capitulo5 <- function(){
  print("Este es el capitulo5")
  cuatroEtiquetas()
  anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  g7<- graficaLinea(getListIpc()$"5_02")
  exportarLatex(paste(getPath(),"5_02.tex",sep="/"),g7)
  
  g7777<- graficaBar(getListIpc()$"5_03")
  g7777 <- etiquetasBarras(g7777)
  exportarLatex(paste( getPath(), "5_03.tex", sep = "/"),g7777)
  
  g9<- graficaLinea(getListIpc()$"5_05")
  exportarLatex(paste( getPath(), "5_05.tex", sep = "/"),g9)
  
  g12<- graficaBar(getListIpc()$"5_06")
  g12<- etiquetasBarras(g12)
  exportarLatex(paste( getPath(), "5_06.tex", sep = "/"),g12)
  
  g18<- graficaBar(getListIpc()$"5_07")
  g18 <- etiquetasBarras(g18)
  exportarLatex(paste( getPath(), "5_07.tex", sep = "/"),g18)
  
  g19<- graficaBar(getListIpc()$"5_08")
  g19 <- etiquetasBarras(g19)
  exportarLatex(paste( getPath(), "5_08.tex", sep = "/"),g19)
  
  g200<- graficaLinea(getListIpc()$"5_09")
  exportarLatex(paste( getPath(), "5_09.tex", sep = "/"),g200)
  
  g202<- graficaLinea(getListIpc()$"5_10")
  exportarLatex(paste( getPath(), "5_10.tex", sep = "/"),g202)
  
  g203<- graficaLinea(getListIpc()$"5_11")
  exportarLatex(paste( getPath(), "5_11.tex", sep = "/"),g203)
  
  g204<- graficaLinea(getListIpc()$"5_12")
  exportarLatex(paste( getPath(), "5_12.tex", sep = "/"),g204)
  
  g205<- graficaLinea(getListIpc()$"5_13")
  exportarLatex(paste( getPath(), "5_13.tex", sep = "/"),g205)
  
  g206<- graficaLinea(getListIpc()$"5_14")
  exportarLatex(paste( getPath(), "5_14.tex", sep = "/"),g206)
  
  g207<- graficaLinea(getListIpc()$"5_15")
  exportarLatex(paste( getPath(), "5_15.tex", sep = "/"),g207)
  
  g208<- graficaLinea(getListIpc()$"5_16")
  exportarLatex(paste( getPath(), "5_16.tex", sep = "/"),g208)
  
  g209<- graficaLinea(getListIpc()$"5_17")
  exportarLatex(paste( getPath(), "5_17.tex", sep = "/"),g209)
  
  g110<- graficaLinea(getListIpc()$"5_18")
  exportarLatex(paste( getPath(), "5_18.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"5_19")
  exportarLatex(paste( getPath(), "5_19.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"5_20")
  exportarLatex(paste( getPath(), "5_20.tex", sep = "/"),g112)
  
  g301<- graficaBar(getListIpc()$"5_21")
  g301<- etiquetasBarras(g301)
  exportarLatex(paste( getPath(), "5_21.tex", sep = "/"),g301)
  
  g17<- graficaBar(getListIpc()$"5_22")
  g17 <- etiquetasBarras(g17)
  exportarLatex(paste( getPath(), "5_22.tex", sep = "/"),g17)
  
  
  g13<- graficaLinea(getListIpc()$"5_24")
  exportarLatex(paste( getPath(), "5_24.tex", sep = "/"),g13)
  
  g16<- graficaBar(getListIpc()$"5_25")
  g16 <- etiquetasBarras(g16)
  exportarLatex(paste( getPath(), "5_25.tex", sep = "/"),g16)
  
  g22<- graficaBar(getListIpc()$"5_26")
  g22 <- etiquetasBarras(g22)
  exportarLatex(paste( getPath(), "5_26.tex", sep = "/"),g22)
  
  g23<- graficaBar(getListIpc()$"5_27")
  g23 <- etiquetasBarras(g23)
  exportarLatex(paste( getPath(), "5_27.tex", sep = "/"),g23)
  
  g100<- graficaLinea(getListIpc()$"5_28")
  exportarLatex(paste( getPath(), "5_28.tex", sep = "/"),g100)
  
  g102<- graficaLinea(getListIpc()$"5_29")
  exportarLatex(paste( getPath(), "5_29.tex", sep = "/"),g102)
  
  g103<- graficaLinea(getListIpc()$"5_30")
  exportarLatex(paste( getPath(), "5_30.tex", sep = "/"),g103)
  
  g104<- graficaLinea(getListIpc()$"5_31")
  exportarLatex(paste( getPath(), "5_31.tex", sep = "/"),g104)
  
  g105<- graficaLinea(getListIpc()$"5_32")
  exportarLatex(paste( getPath(), "5_32.tex", sep = "/"),g105)
  
  g106<- graficaLinea(getListIpc()$"5_33")
  exportarLatex(paste( getPath(), "5_33.tex", sep = "/"),g106)
  
  g107<- graficaLinea(getListIpc()$"5_34")
  exportarLatex(paste( getPath(), "5_34.tex", sep = "/"),g107)
  
  g108<- graficaLinea(getListIpc()$"5_35")
  exportarLatex(paste( getPath(), "5_35.tex", sep = "/"),g108)
  
  g110<- graficaLinea(getListIpc()$"5_36")
  exportarLatex(paste( getPath(), "5_36.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"5_37")
  exportarLatex(paste( getPath(), "5_37.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"5_38")
  exportarLatex(paste( getPath(), "5_38.tex", sep = "/"),g112)
  
  g302<- graficaBar(getListIpc()$"5_39")
  g302<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "5_39.tex", sep = "/"),g302)
  
  g303<- graficaBar(getListIpc()$"5_40")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "5_40.tex", sep = "/"),g303)
  
  g303<- graficaBar(getListIpc()$"5_41")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "5_41.tex", sep = "/"),g303)
  
  g111<- graficaLinea(getListIpc()$"5_42")
  exportarLatex(paste( getPath(), "5_42.tex", sep = "/"),g111)
  
}

#'Fucnion para hacer el capitulo 6

capitulo6 <- function(){
  print("Este es el capitulo6")
  cuatroEtiquetas()
  anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  g7<- graficaLinea(getListIpc()$"6_02")
  exportarLatex(paste(getPath(),"6_02.tex",sep="/"),g7)
  
  g7777<- graficaBar(getListIpc()$"6_03")
  g7777 <- etiquetasBarras(g7777)
  exportarLatex(paste( getPath(), "6_03.tex", sep = "/"),g7777)
  
  g9<- graficaLinea(getListIpc()$"6_05")
  exportarLatex(paste( getPath(), "6_05.tex", sep = "/"),g9)
  
  g12<- graficaBar(getListIpc()$"6_06")
  g12<- etiquetasBarras(g12)
  exportarLatex(paste( getPath(), "6_06.tex", sep = "/"),g12)
  
  g18<- graficaBar(getListIpc()$"6_07")
  g18 <- etiquetasBarras(g18)
  exportarLatex(paste( getPath(), "6_07.tex", sep = "/"),g18)
  
  g19<- graficaBar(getListIpc()$"6_08")
  g19 <- etiquetasBarras(g19)
  exportarLatex(paste( getPath(), "6_08.tex", sep = "/"),g19)
  
  g200<- graficaLinea(getListIpc()$"6_09")
  exportarLatex(paste( getPath(), "6_09.tex", sep = "/"),g200)
  
  g202<- graficaLinea(getListIpc()$"6_10")
  exportarLatex(paste( getPath(), "6_10.tex", sep = "/"),g202)
  
  g203<- graficaLinea(getListIpc()$"6_11")
  exportarLatex(paste( getPath(), "6_11.tex", sep = "/"),g203)
  
  g204<- graficaLinea(getListIpc()$"6_12")
  exportarLatex(paste( getPath(), "6_12.tex", sep = "/"),g204)
  
  g205<- graficaLinea(getListIpc()$"6_13")
  exportarLatex(paste( getPath(), "6_13.tex", sep = "/"),g205)
  
  g206<- graficaLinea(getListIpc()$"6_14")
  exportarLatex(paste( getPath(), "6_14.tex", sep = "/"),g206)
  
  g207<- graficaLinea(getListIpc()$"6_15")
  exportarLatex(paste( getPath(), "6_15.tex", sep = "/"),g207)
  
  g208<- graficaLinea(getListIpc()$"6_16")
  exportarLatex(paste( getPath(), "6_16.tex", sep = "/"),g208)
  
  g209<- graficaLinea(getListIpc()$"6_17")
  exportarLatex(paste( getPath(), "6_17.tex", sep = "/"),g209)
  
  g110<- graficaLinea(getListIpc()$"6_18")
  exportarLatex(paste( getPath(), "6_18.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"6_19")
  exportarLatex(paste( getPath(), "6_19.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"6_20")
  exportarLatex(paste( getPath(), "6_20.tex", sep = "/"),g112)
  
  g301<- graficaBar(getListIpc()$"6_21")
  g301<- etiquetasBarras(g301)
  exportarLatex(paste( getPath(), "6_21.tex", sep = "/"),g301)
  
  g17<- graficaBar(getListIpc()$"6_22")
  g17 <- etiquetasBarras(g17)
  exportarLatex(paste( getPath(), "6_22.tex", sep = "/"),g17)
  
  
  g13<- graficaLinea(getListIpc()$"6_24")
  exportarLatex(paste( getPath(), "6_24.tex", sep = "/"),g13)
  
  g16<- graficaBar(getListIpc()$"6_25")
  g16 <- etiquetasBarras(g16)
  exportarLatex(paste( getPath(), "6_25.tex", sep = "/"),g16)
  
  g22<- graficaBar(getListIpc()$"6_26")
  g22 <- etiquetasBarras(g22)
  exportarLatex(paste( getPath(), "6_26.tex", sep = "/"),g22)
  
  g23<- graficaBar(getListIpc()$"6_27")
  g23 <- etiquetasBarras(g23)
  exportarLatex(paste( getPath(), "6_27.tex", sep = "/"),g23)
  
  g100<- graficaLinea(getListIpc()$"6_28")
  exportarLatex(paste( getPath(), "6_28.tex", sep = "/"),g100)
  
  g102<- graficaLinea(getListIpc()$"6_29")
  exportarLatex(paste( getPath(), "6_29.tex", sep = "/"),g102)
  
  g103<- graficaLinea(getListIpc()$"6_30")
  exportarLatex(paste( getPath(), "6_30.tex", sep = "/"),g103)
  
  g104<- graficaLinea(getListIpc()$"6_31")
  exportarLatex(paste( getPath(), "6_31.tex", sep = "/"),g104)
  
  g105<- graficaLinea(getListIpc()$"6_32")
  exportarLatex(paste( getPath(), "6_32.tex", sep = "/"),g105)
  
  g106<- graficaLinea(getListIpc()$"6_33")
  exportarLatex(paste( getPath(), "6_33.tex", sep = "/"),g106)
  
  g107<- graficaLinea(getListIpc()$"6_34")
  exportarLatex(paste( getPath(), "6_34.tex", sep = "/"),g107)
  
  g108<- graficaLinea(getListIpc()$"6_35")
  exportarLatex(paste( getPath(), "6_35.tex", sep = "/"),g108)
  
  g110<- graficaLinea(getListIpc()$"6_36")
  exportarLatex(paste( getPath(), "6_36.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"6_37")
  exportarLatex(paste( getPath(), "6_37.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"6_38")
  exportarLatex(paste( getPath(), "6_38.tex", sep = "/"),g112)
  
  g302<- graficaBar(getListIpc()$"6_39")
  g302<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "6_39.tex", sep = "/"),g302)
  
  g303<- graficaBar(getListIpc()$"6_40")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "6_40.tex", sep = "/"),g303)
  
  g303<- graficaBar(getListIpc()$"6_41")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "6_41.tex", sep = "/"),g303)
  
  g111<- graficaLinea(getListIpc()$"6_42")
  exportarLatex(paste( getPath(), "6_42.tex", sep = "/"),g111)
  
}

#'Fucnion para hacer el capitulo 7

capitulo7 <- function(){
  print("Este es el capitulo7")
  cuatroEtiquetas()
  anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  g7<- graficaLinea(getListIpc()$"7_02")
  exportarLatex(paste(getPath(),"7_02.tex",sep="/"),g7)
  
  g7777<- graficaBar(getListIpc()$"7_03")
  g7777 <- etiquetasBarras(g7777)
  exportarLatex(paste( getPath(), "7_03.tex", sep = "/"),g7777)
  
  g9<- graficaLinea(getListIpc()$"7_05")
  exportarLatex(paste( getPath(), "7_05.tex", sep = "/"),g9)
  
  g12<- graficaBar(getListIpc()$"7_06")
  g12<- etiquetasBarras(g12)
  exportarLatex(paste( getPath(), "7_06.tex", sep = "/"),g12)
  
  g18<- graficaBar(getListIpc()$"7_07")
  g18 <- etiquetasBarras(g18)
  exportarLatex(paste( getPath(), "7_07.tex", sep = "/"),g18)
  
  g19<- graficaBar(getListIpc()$"7_08")
  g19 <- etiquetasBarras(g19)
  exportarLatex(paste( getPath(), "7_08.tex", sep = "/"),g19)
  
  g200<- graficaLinea(getListIpc()$"7_09")
  exportarLatex(paste( getPath(), "7_09.tex", sep = "/"),g200)
  
  g202<- graficaLinea(getListIpc()$"7_10")
  exportarLatex(paste( getPath(), "7_10.tex", sep = "/"),g202)
  
  g203<- graficaLinea(getListIpc()$"7_11")
  exportarLatex(paste( getPath(), "7_11.tex", sep = "/"),g203)
  
  g204<- graficaLinea(getListIpc()$"7_12")
  exportarLatex(paste( getPath(), "7_12.tex", sep = "/"),g204)
  
  g205<- graficaLinea(getListIpc()$"7_13")
  exportarLatex(paste( getPath(), "7_13.tex", sep = "/"),g205)
  
  g206<- graficaLinea(getListIpc()$"7_14")
  exportarLatex(paste( getPath(), "7_14.tex", sep = "/"),g206)
  
  g207<- graficaLinea(getListIpc()$"7_15")
  exportarLatex(paste( getPath(), "7_15.tex", sep = "/"),g207)
  
  g208<- graficaLinea(getListIpc()$"7_16")
  exportarLatex(paste( getPath(), "7_16.tex", sep = "/"),g208)
  
  g209<- graficaLinea(getListIpc()$"7_17")
  exportarLatex(paste( getPath(), "7_17.tex", sep = "/"),g209)
  
  g110<- graficaLinea(getListIpc()$"7_18")
  exportarLatex(paste( getPath(), "7_18.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"7_19")
  exportarLatex(paste( getPath(), "7_19.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"7_20")
  exportarLatex(paste( getPath(), "7_20.tex", sep = "/"),g112)
  
  g301<- graficaBar(getListIpc()$"7_21")
  g301<- etiquetasBarras(g301)
  exportarLatex(paste( getPath(), "7_21.tex", sep = "/"),g301)
  
  g17<- graficaBar(getListIpc()$"7_22")
  g17 <- etiquetasBarras(g17)
  exportarLatex(paste( getPath(), "7_22.tex", sep = "/"),g17)
  
  
  g13<- graficaLinea(getListIpc()$"7_24")
  exportarLatex(paste( getPath(), "7_24.tex", sep = "/"),g13)
  
  g16<- graficaBar(getListIpc()$"7_25")
  g16 <- etiquetasBarras(g16)
  exportarLatex(paste( getPath(), "7_25.tex", sep = "/"),g16)
  
  g22<- graficaBar(getListIpc()$"7_26")
  g22 <- etiquetasBarras(g22)
  exportarLatex(paste( getPath(), "7_26.tex", sep = "/"),g22)
  
  g23<- graficaBar(getListIpc()$"7_27")
  g23 <- etiquetasBarras(g23)
  exportarLatex(paste( getPath(), "7_27.tex", sep = "/"),g23)
  
  g100<- graficaLinea(getListIpc()$"7_28")
  exportarLatex(paste( getPath(), "7_28.tex", sep = "/"),g100)
  
  g102<- graficaLinea(getListIpc()$"7_29")
  exportarLatex(paste( getPath(), "7_29.tex", sep = "/"),g102)
  
  g103<- graficaLinea(getListIpc()$"7_30")
  exportarLatex(paste( getPath(), "7_30.tex", sep = "/"),g103)
  
  g104<- graficaLinea(getListIpc()$"7_31")
  exportarLatex(paste( getPath(), "7_31.tex", sep = "/"),g104)
  
  g105<- graficaLinea(getListIpc()$"7_32")
  exportarLatex(paste( getPath(), "7_32.tex", sep = "/"),g105)
  
  g106<- graficaLinea(getListIpc()$"7_33")
  exportarLatex(paste( getPath(), "7_33.tex", sep = "/"),g106)
  
  g107<- graficaLinea(getListIpc()$"7_34")
  exportarLatex(paste( getPath(), "7_34.tex", sep = "/"),g107)
  
  g108<- graficaLinea(getListIpc()$"7_35")
  exportarLatex(paste( getPath(), "7_35.tex", sep = "/"),g108)
  
  g110<- graficaLinea(getListIpc()$"7_36")
  exportarLatex(paste( getPath(), "7_36.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"7_37")
  exportarLatex(paste( getPath(), "7_37.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"7_38")
  exportarLatex(paste( getPath(), "7_38.tex", sep = "/"),g112)
  
  g302<- graficaBar(getListIpc()$"7_39")
  g302<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "7_39.tex", sep = "/"),g302)
  
  g303<- graficaBar(getListIpc()$"7_40")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "7_40.tex", sep = "/"),g303)
  
  g303<- graficaBar(getListIpc()$"7_41")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "7_41.tex", sep = "/"),g303)
  
  g111<- graficaLinea(getListIpc()$"7_42")
  exportarLatex(paste( getPath(), "7_42.tex", sep = "/"),g111)
  
}

#'Fucnion para hacer el capitulo 8

capitulo8 <- function(){
  print("Este es el capitulo8")
  cuatroEtiquetas()
  anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  g7<- graficaLinea(getListIpc()$"8_02")
  exportarLatex(paste(getPath(),"8_02.tex",sep="/"),g7)
  
  g7777<- graficaBar(getListIpc()$"8_03")
  g7777 <- etiquetasBarras(g7777)
  exportarLatex(paste( getPath(), "8_03.tex", sep = "/"),g7777)
  
  g9<- graficaLinea(getListIpc()$"8_05")
  exportarLatex(paste( getPath(), "8_05.tex", sep = "/"),g9)
  
  g12<- graficaBar(getListIpc()$"8_06")
  g12<- etiquetasBarras(g12)
  exportarLatex(paste( getPath(), "8_06.tex", sep = "/"),g12)
  
  g18<- graficaBar(getListIpc()$"8_07")
  g18 <- etiquetasBarras(g18)
  exportarLatex(paste( getPath(), "8_07.tex", sep = "/"),g18)
  
  g19<- graficaBar(getListIpc()$"8_08")
  g19 <- etiquetasBarras(g19)
  exportarLatex(paste( getPath(), "8_08.tex", sep = "/"),g19)
  
  g200<- graficaLinea(getListIpc()$"8_09")
  exportarLatex(paste( getPath(), "8_09.tex", sep = "/"),g200)
  
  g202<- graficaLinea(getListIpc()$"8_10")
  exportarLatex(paste( getPath(), "8_10.tex", sep = "/"),g202)
  
  g203<- graficaLinea(getListIpc()$"8_11")
  exportarLatex(paste( getPath(), "8_11.tex", sep = "/"),g203)
  
  g204<- graficaLinea(getListIpc()$"8_12")
  exportarLatex(paste( getPath(), "8_12.tex", sep = "/"),g204)
  
  g205<- graficaLinea(getListIpc()$"8_13")
  exportarLatex(paste( getPath(), "8_13.tex", sep = "/"),g205)
  
  g206<- graficaLinea(getListIpc()$"8_14")
  exportarLatex(paste( getPath(), "8_14.tex", sep = "/"),g206)
  
  g207<- graficaLinea(getListIpc()$"8_15")
  exportarLatex(paste( getPath(), "8_15.tex", sep = "/"),g207)
  
  g208<- graficaLinea(getListIpc()$"8_16")
  exportarLatex(paste( getPath(), "8_16.tex", sep = "/"),g208)
  
  g209<- graficaLinea(getListIpc()$"8_17")
  exportarLatex(paste( getPath(), "8_17.tex", sep = "/"),g209)
  
  g110<- graficaLinea(getListIpc()$"8_18")
  exportarLatex(paste( getPath(), "8_18.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"8_19")
  exportarLatex(paste( getPath(), "8_19.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"8_20")
  exportarLatex(paste( getPath(), "8_20.tex", sep = "/"),g112)
  
  g301<- graficaBar(getListIpc()$"8_21")
  g301<- etiquetasBarras(g301)
  exportarLatex(paste( getPath(), "8_21.tex", sep = "/"),g301)
  
  g17<- graficaBar(getListIpc()$"8_22")
  g17 <- etiquetasBarras(g17)
  exportarLatex(paste( getPath(), "8_22.tex", sep = "/"),g17)
  
  
  g13<- graficaLinea(getListIpc()$"8_24")
  exportarLatex(paste( getPath(), "8_24.tex", sep = "/"),g13)
  
  g16<- graficaBar(getListIpc()$"8_25")
  g16 <- etiquetasBarras(g16)
  exportarLatex(paste( getPath(), "8_25.tex", sep = "/"),g16)
  
  g22<- graficaBar(getListIpc()$"8_26")
  g22 <- etiquetasBarras(g22)
  exportarLatex(paste( getPath(), "8_26.tex", sep = "/"),g22)
  
  g23<- graficaBar(getListIpc()$"8_27")
  g23 <- etiquetasBarras(g23)
  exportarLatex(paste( getPath(), "8_27.tex", sep = "/"),g23)
  
  g100<- graficaLinea(getListIpc()$"8_28")
  exportarLatex(paste( getPath(), "8_28.tex", sep = "/"),g100)
  
  g102<- graficaLinea(getListIpc()$"8_29")
  exportarLatex(paste( getPath(), "8_29.tex", sep = "/"),g102)
  
  g103<- graficaLinea(getListIpc()$"8_30")
  exportarLatex(paste( getPath(), "8_30.tex", sep = "/"),g103)
  
  g104<- graficaLinea(getListIpc()$"8_31")
  exportarLatex(paste( getPath(), "8_31.tex", sep = "/"),g104)
  
  g105<- graficaLinea(getListIpc()$"8_32")
  exportarLatex(paste( getPath(), "8_32.tex", sep = "/"),g105)
  
  g106<- graficaLinea(getListIpc()$"8_33")
  exportarLatex(paste( getPath(), "8_33.tex", sep = "/"),g106)
  
  g107<- graficaLinea(getListIpc()$"8_34")
  exportarLatex(paste( getPath(), "8_34.tex", sep = "/"),g107)
  
  g108<- graficaLinea(getListIpc()$"8_35")
  exportarLatex(paste( getPath(), "8_35.tex", sep = "/"),g108)
  
  g110<- graficaLinea(getListIpc()$"8_36")
  exportarLatex(paste( getPath(), "8_36.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"8_37")
  exportarLatex(paste( getPath(), "8_37.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"8_38")
  exportarLatex(paste( getPath(), "8_38.tex", sep = "/"),g112)
  
  g302<- graficaBar(getListIpc()$"8_39")
  g302<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "8_39.tex", sep = "/"),g302)
  
  g303<- graficaBar(getListIpc()$"8_40")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "8_40.tex", sep = "/"),g303)
  
  g303<- graficaBar(getListIpc()$"8_41")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "8_41.tex", sep = "/"),g303)
  
  g111<- graficaLinea(getListIpc()$"8_42")
  exportarLatex(paste( getPath(), "8_42.tex", sep = "/"),g111)
  
}

#'Fucnion para hacer el capitulo 9

capitulo9 <- function(){
  print("Este es el capitulo9")
  cuatroEtiquetas()
  anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  g7<- graficaLinea(getListIpc()$"9_02")
  exportarLatex(paste(getPath(),"9_02.tex",sep="/"),g7)
  
  g7777<- graficaBar(getListIpc()$"9_03")
  g7777 <- etiquetasBarras(g7777)
  exportarLatex(paste( getPath(), "9_03.tex", sep = "/"),g7777)
  
  g9<- graficaLinea(getListIpc()$"9_05")
  exportarLatex(paste( getPath(), "9_05.tex", sep = "/"),g9)
  
  g12<- graficaBar(getListIpc()$"9_06")
  g12<- etiquetasBarras(g12)
  exportarLatex(paste( getPath(), "9_06.tex", sep = "/"),g12)
  
  g18<- graficaBar(getListIpc()$"9_07")
  g18 <- etiquetasBarras(g18)
  exportarLatex(paste( getPath(), "9_07.tex", sep = "/"),g18)
  
  g19<- graficaBar(getListIpc()$"9_08")
  g19 <- etiquetasBarras(g19)
  exportarLatex(paste( getPath(), "9_08.tex", sep = "/"),g19)
  
  g200<- graficaLinea(getListIpc()$"9_09")
  exportarLatex(paste( getPath(), "9_09.tex", sep = "/"),g200)
  
  g202<- graficaLinea(getListIpc()$"9_10")
  exportarLatex(paste( getPath(), "9_10.tex", sep = "/"),g202)
  
  g203<- graficaLinea(getListIpc()$"9_11")
  exportarLatex(paste( getPath(), "9_11.tex", sep = "/"),g203)
  
  g204<- graficaLinea(getListIpc()$"9_12")
  exportarLatex(paste( getPath(), "9_12.tex", sep = "/"),g204)
  
  g205<- graficaLinea(getListIpc()$"9_13")
  exportarLatex(paste( getPath(), "9_13.tex", sep = "/"),g205)
  
  g206<- graficaLinea(getListIpc()$"9_14")
  exportarLatex(paste( getPath(), "9_14.tex", sep = "/"),g206)
  
  g207<- graficaLinea(getListIpc()$"9_15")
  exportarLatex(paste( getPath(), "9_15.tex", sep = "/"),g207)
  
  g208<- graficaLinea(getListIpc()$"9_16")
  exportarLatex(paste( getPath(), "9_16.tex", sep = "/"),g208)
  
  g209<- graficaLinea(getListIpc()$"9_17")
  exportarLatex(paste( getPath(), "9_17.tex", sep = "/"),g209)
  
  g110<- graficaLinea(getListIpc()$"9_18")
  exportarLatex(paste( getPath(), "9_18.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"9_19")
  exportarLatex(paste( getPath(), "9_19.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"9_20")
  exportarLatex(paste( getPath(), "9_20.tex", sep = "/"),g112)
  
  g301<- graficaBar(getListIpc()$"9_21")
  g301<- etiquetasBarras(g301)
  exportarLatex(paste( getPath(), "9_21.tex", sep = "/"),g301)
  
  g17<- graficaBar(getListIpc()$"9_22")
  g17 <- etiquetasBarras(g17)
  exportarLatex(paste( getPath(), "9_22.tex", sep = "/"),g17)
  
  
  g13<- graficaLinea(getListIpc()$"9_24")
  exportarLatex(paste( getPath(), "9_24.tex", sep = "/"),g13)
  
  g16<- graficaBar(getListIpc()$"9_25")
  g16 <- etiquetasBarras(g16)
  exportarLatex(paste( getPath(), "9_25.tex", sep = "/"),g16)
  
  g22<- graficaBar(getListIpc()$"9_26")
  g22 <- etiquetasBarras(g22)
  exportarLatex(paste( getPath(), "9_26.tex", sep = "/"),g22)
  
  g23<- graficaBar(getListIpc()$"9_27")
  g23 <- etiquetasBarras(g23)
  exportarLatex(paste( getPath(), "9_27.tex", sep = "/"),g23)
  
  g100<- graficaLinea(getListIpc()$"9_28")
  exportarLatex(paste( getPath(), "9_28.tex", sep = "/"),g100)
  
  g102<- graficaLinea(getListIpc()$"9_29")
  exportarLatex(paste( getPath(), "9_29.tex", sep = "/"),g102)
  
  g103<- graficaLinea(getListIpc()$"9_30")
  exportarLatex(paste( getPath(), "9_30.tex", sep = "/"),g103)
  
  g104<- graficaLinea(getListIpc()$"9_31")
  exportarLatex(paste( getPath(), "9_31.tex", sep = "/"),g104)
  
  g105<- graficaLinea(getListIpc()$"9_32")
  exportarLatex(paste( getPath(), "9_32.tex", sep = "/"),g105)
  
  g106<- graficaLinea(getListIpc()$"9_33")
  exportarLatex(paste( getPath(), "9_33.tex", sep = "/"),g106)
  
  g107<- graficaLinea(getListIpc()$"9_34")
  exportarLatex(paste( getPath(), "9_34.tex", sep = "/"),g107)
  
  g108<- graficaLinea(getListIpc()$"9_35")
  exportarLatex(paste( getPath(), "9_35.tex", sep = "/"),g108)
  
  g110<- graficaLinea(getListIpc()$"9_36")
  exportarLatex(paste( getPath(), "9_36.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"9_37")
  exportarLatex(paste( getPath(), "9_37.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"9_38")
  exportarLatex(paste( getPath(), "9_38.tex", sep = "/"),g112)
  
  g302<- graficaBar(getListIpc()$"9_39")
  g302<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "9_39.tex", sep = "/"),g302)
  
  g303<- graficaBar(getListIpc()$"9_40")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "9_40.tex", sep = "/"),g303)
  
  g303<- graficaBar(getListIpc()$"9_41")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "9_41.tex", sep = "/"),g303)
  
  g111<- graficaLinea(getListIpc()$"9_42")
  exportarLatex(paste( getPath(), "9_42.tex", sep = "/"),g111)
  
}

#'Fucnion para hacer el capitulo 10

capitulo10 <- function(){
  print("Este es el capitulo10")
  cuatroEtiquetas()
  anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  g7<- graficaLinea(getListIpc()$"10_02")
  exportarLatex(paste(getPath(),"10_02.tex",sep="/"),g7)
  
  g7777<- graficaBar(getListIpc()$"10_03")
  g7777 <- etiquetasBarras(g7777)
  exportarLatex(paste( getPath(), "10_03.tex", sep = "/"),g7777)
  
  g9<- graficaLinea(getListIpc()$"10_05")
  exportarLatex(paste( getPath(), "10_05.tex", sep = "/"),g9)
  
  g12<- graficaBar(getListIpc()$"10_06")
  g12<- etiquetasBarras(g12)
  exportarLatex(paste( getPath(), "10_06.tex", sep = "/"),g12)
  
  g18<- graficaBar(getListIpc()$"10_07")
  g18 <- etiquetasBarras(g18)
  exportarLatex(paste( getPath(), "10_07.tex", sep = "/"),g18)
  
  g19<- graficaBar(getListIpc()$"10_08")
  g19 <- etiquetasBarras(g19)
  exportarLatex(paste( getPath(), "10_08.tex", sep = "/"),g19)
  
  g200<- graficaLinea(getListIpc()$"10_09")
  exportarLatex(paste( getPath(), "10_09.tex", sep = "/"),g200)
  
  g202<- graficaLinea(getListIpc()$"10_10")
  exportarLatex(paste( getPath(), "10_10.tex", sep = "/"),g202)
  
  g203<- graficaLinea(getListIpc()$"10_11")
  exportarLatex(paste( getPath(), "10_11.tex", sep = "/"),g203)
  
  g204<- graficaLinea(getListIpc()$"10_12")
  exportarLatex(paste( getPath(), "10_12.tex", sep = "/"),g204)
  
  g205<- graficaLinea(getListIpc()$"10_13")
  exportarLatex(paste( getPath(), "10_13.tex", sep = "/"),g205)
  
  g206<- graficaLinea(getListIpc()$"10_14")
  exportarLatex(paste( getPath(), "10_14.tex", sep = "/"),g206)
  
  g207<- graficaLinea(getListIpc()$"10_15")
  exportarLatex(paste( getPath(), "10_15.tex", sep = "/"),g207)
  
  g208<- graficaLinea(getListIpc()$"10_16")
  exportarLatex(paste( getPath(), "10_16.tex", sep = "/"),g208)
  
  g209<- graficaLinea(getListIpc()$"10_17")
  exportarLatex(paste( getPath(), "10_17.tex", sep = "/"),g209)
  
  g110<- graficaLinea(getListIpc()$"10_18")
  exportarLatex(paste( getPath(), "10_18.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"10_19")
  exportarLatex(paste( getPath(), "10_19.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"10_20")
  exportarLatex(paste( getPath(), "10_20.tex", sep = "/"),g112)
  
  g301<- graficaBar(getListIpc()$"10_21")
  g301<- etiquetasBarras(g301)
  exportarLatex(paste( getPath(), "10_21.tex", sep = "/"),g301)
  
  g17<- graficaBar(getListIpc()$"10_22")
  g17 <- etiquetasBarras(g17)
  exportarLatex(paste( getPath(), "10_22.tex", sep = "/"),g17)
  
  
  g13<- graficaLinea(getListIpc()$"10_24")
  exportarLatex(paste( getPath(), "10_24.tex", sep = "/"),g13)
  
  g16<- graficaBar(getListIpc()$"10_25")
  g16 <- etiquetasBarras(g16)
  exportarLatex(paste( getPath(), "10_25.tex", sep = "/"),g16)
  
  g22<- graficaBar(getListIpc()$"10_26")
  g22 <- etiquetasBarras(g22)
  exportarLatex(paste( getPath(), "10_26.tex", sep = "/"),g22)
  
  g23<- graficaBar(getListIpc()$"10_27")
  g23 <- etiquetasBarras(g23)
  exportarLatex(paste( getPath(), "10_27.tex", sep = "/"),g23)
  
  g100<- graficaLinea(getListIpc()$"10_28")
  exportarLatex(paste( getPath(), "10_28.tex", sep = "/"),g100)
  
  g102<- graficaLinea(getListIpc()$"10_29")
  exportarLatex(paste( getPath(), "10_29.tex", sep = "/"),g102)
  
  g103<- graficaLinea(getListIpc()$"10_30")
  exportarLatex(paste( getPath(), "10_30.tex", sep = "/"),g103)
  
  g104<- graficaLinea(getListIpc()$"10_31")
  exportarLatex(paste( getPath(), "10_31.tex", sep = "/"),g104)
  
  g105<- graficaLinea(getListIpc()$"10_32")
  exportarLatex(paste( getPath(), "10_32.tex", sep = "/"),g105)
  
  g106<- graficaLinea(getListIpc()$"10_33")
  exportarLatex(paste( getPath(), "10_33.tex", sep = "/"),g106)
  
  g107<- graficaLinea(getListIpc()$"10_34")
  exportarLatex(paste( getPath(), "10_34.tex", sep = "/"),g107)
  
  g108<- graficaLinea(getListIpc()$"10_35")
  exportarLatex(paste( getPath(), "10_35.tex", sep = "/"),g108)
  
  g110<- graficaLinea(getListIpc()$"10_36")
  exportarLatex(paste( getPath(), "10_36.tex", sep = "/"),g110)
  
  g111<- graficaLinea(getListIpc()$"10_37")
  exportarLatex(paste( getPath(), "10_37.tex", sep = "/"),g111)
  
  g112<- graficaLinea(getListIpc()$"10_38")
  exportarLatex(paste( getPath(), "10_38.tex", sep = "/"),g112)
  
  g302<- graficaBar(getListIpc()$"10_39")
  g302<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "10_39.tex", sep = "/"),g302)
  
  g303<- graficaBar(getListIpc()$"10_40")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "10_40.tex", sep = "/"),g303)
  
  g303<- graficaBar(getListIpc()$"10_41")
  g303<- etiquetasBarras(g302)
  exportarLatex(paste( getPath(), "10_41.tex", sep = "/"),g303)
  
  g111<- graficaLinea(getListIpc()$"10_42")
  exportarLatex(paste( getPath(), "10_42.tex", sep = "/"),g111)
  
}
