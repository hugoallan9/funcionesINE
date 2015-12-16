#'Función para hacer la gráficas trimestrales
#'de comercio exterior 
#'@param lista Es el listado que contiene los data frame de comercio exterior
#'@param ruta Es el path de salida para los tex con las graficas
#'
graficasComercio <- function(lista, ruta, modalidad = "trimestral"){
  pre <- T
  if( toupper(modalidad)  == "TRIMESTRAL"){
    trimestral()
    pre <-T
  }else if ( toupper(modalidad) == "ANUAL"){
    anual(rgb(0,0,1), rgb(0.6156862745098039,0.7333333333333333,1))
  }else {
    presentacion()
    pre <- T
  }
  
  g1<- graficaLineaTrim(lista$'1_01')
  exportarLatex(paste(ruta,"1_01.tex", sep=""),g1)
  
  g2 <- graficaCol(lista$"1_02", ordenar = F)
  g2 <- etiquetasHorizontales(g2)
  g2 <- rotarEtiX(g2)
  g2 <- g2+ ggplot2::geom_abline(intercept = 0, slope = 0)  
  exportarLatex(paste(ruta,"1_02.tex", sep=""),g2)
  
  g3 <- graficaLineaTrim(lista$"1_03")
  exportarLatex(paste(ruta,"1_03.tex", sep=""),g3)
  
  g4<- graficaCol(lista$"1_04", ordenar = F)#cambiar a col
  g4 <- etiquetasHorizontales(g4)
  g4 <- rotarEtiX(g4)
  g4 <- g4+ ggplot2::geom_abline(intercept = 0, slope = 0) 
  exportarLatex(paste(ruta,"1_04.tex", sep=""),g4)
  
  
  g5 <- graficaColCategorias(lista$'1_05', ruta = paste(ruta, '1_05.tex'), escala = 'milesmillones')
  
  
  g6 <- graficaCol(lista$"1_06", ordenar =  F)# cambiar a col
  g6 <- etiquetasHorizontales(g6)
  g6 <- rotarEtiX(g6)
  g6 <- g6 + ggplot2::geom_abline(intercept = 0, slope = 0) 
  exportarLatex(paste(ruta,"1_06.tex", sep=""),g6)
  
  #g7<- graficaCol(lista$"1_07")
  #g7 <- etiquetasHorizontales(g7)
  #g7 <- rotarEtiX(g7)
  #exportarLatex("CET1//1_07.tex",g7)
  
  #g8<- graficaCol(lista$"1_08")
  #g8 <- etiquetasHorizontales(g8)
  #g8 <- rotarEtiX(g8)
  #exportarLatex("CET1//1_08.tex",g8)
  
  g9<- graficaBar(lista$"1_09")
  g9 <- etiquetasBarras(g9)
  exportarLatex(paste(ruta,"1_09.tex", sep=""),g9)
  
  #g9<- graficaCol(lista$"1_09")
  #g9 <- etiquetasHorizontales(g9)
  #g9 <- rotarEtiX(g9)
  #exportarLatex("CET1//1_09.tex",g9)
  
  g10<- graficaCol(lista$"1_10")
  g10 <- etiquetasHorizontales(g10)
  g10 <- rotarEtiX(g10)
  exportarLatex(paste(ruta,"1_10.tex", sep=""),g10)
  
  
  g11<- graficaBar(lista$"1_11")
  g11 <- etiquetasBarras(g11)
  exportarLatex(paste(ruta,"1_11.tex", sep=""),g11)
  
  g12 <- graficaBar(lista$"1_12")
  g12 <- etiquetasBarras(g12)
  exportarLatex(paste(ruta,"1_12.tex", sep=""),g12)
  
  
  g13<- graficaCol(lista$"1_13")
  g13 <- etiquetasHorizontales(g13)
  g13 <- rotarEtiX(g13)
  exportarLatex(paste(ruta,"1_13.tex", sep=""),g13)
  
  g14<- graficaBar(lista$"1_14")
  g14 <- etiquetasBarras(g14, margenIz = -4)
  exportarLatex(paste(ruta,"1_14.tex", sep=""),g14)
  
  
  g15<- graficaLineaTrim(lista$"2_01")
  exportarLatex(paste(ruta,"2_01.tex", sep=""),g15)
  
  
  g16<- graficaCol(lista$"2_02", ordenar = F)
  g16 <- etiquetasHorizontales(g16)
  g16 <- rotarEtiX(g16)
  exportarLatex(paste(ruta,"2_02.tex", sep=""),g16)
  
  
  g17<- graficaLineaTrim(lista$"2_03")
  exportarLatex(paste(ruta,"2_03.tex", sep=""),g17)
  
  
  g18<- graficaCol(lista$"2_04", ordenar = F)
  g18 <- etiquetasHorizontales(g18)
  g18 <- rotarEtiX(g18)
  exportarLatex(paste(ruta,"2_04.tex", sep=""),g18)
  
  
  g20<- graficaCol(lista$"2_06", ordenar = F)
  g20 <- etiquetasHorizontales(g20)
  g20 <- rotarEtiX(g20)
  exportarLatex(paste(ruta,"2_06.tex", sep=""),g20)
  
  g23<- graficaBar(lista$"2_09")
  g23<- etiquetasBarras(g23, margenIz = 0)
  exportarLatex(paste(ruta,"2_09.tex", sep=""),g23)
  
  g24<- graficaCol(lista$"2_10")
  g24<- etiquetasHorizontales(g24)
  g24<- rotarEtiX(g24)
  exportarLatex(paste(ruta,"2_10.tex", sep=""),g24)
  
  g25<- graficaBar(lista$"2_11")
  g25<- etiquetasBarras(g25, margenIz = -3)
  exportarLatex(paste(ruta,"2_11.tex", sep=""),g25)
  
  
  g26<- graficaBar(lista$"2_12")
  g26<- etiquetasBarras(g26, margenIz=0)
  exportarLatex(paste(ruta,"2_12.tex", sep=""),g26)
  
  g27<- graficaCol(lista$"2_13")
  g27<- etiquetasHorizontales(g27)
  g27<- rotarEtiX(g27)
  exportarLatex(paste(ruta,"2_13.tex", sep=""),g27)
  
  
  g28<- graficaBar(lista$"2_14")
  g28<- etiquetasBarras(g28, margenIz = 0)
  exportarLatex(paste(ruta,"2_14.tex", sep=""),g28)
  
  g49<- graficaLineaTrim(lista$"3_01")
  exportarLatex(paste(ruta,"3_01.tex", sep=""),g49)
  
  
  g30<- graficaCol(lista$"3_02", ordenar = F)
  g30<- etiquetasHorizontales(g30)
  g30 <- rotarEtiX(g30)
  exportarLatex(paste(ruta,"3_02.tex", sep=""),g30)
  
  
  g31<- graficaCol(lista$"3_03", ordenar = F)
  g31 <- etiquetasHorizontales(g31)
  g31 <- rotarEtiX(g31)
  exportarLatex(paste(ruta,"3_03.tex", sep=""),g31)
  
  
  g32<- graficaCol(lista$"3_04", ordenar = F)
  g32 <- etiquetasHorizontales(g32)
  g32 <- rotarEtiX(g32)
  exportarLatex(paste(ruta,"3_04.tex", sep=""),g32)
  
  g40<- graficaCol(lista$"3_06", ordenar = F)
  g40 <- etiquetasHorizontales(g40)
  g40 <- rotarEtiX(g40)
  exportarLatex(paste(ruta,"3_06.tex", sep=""),g40)
  
  g43<- graficaBar(lista$"3_09")
  g43<- etiquetasBarras(g43, margenIz = -12)
  exportarLatex(paste(ruta,"3_09.tex", sep=""),g43)
  
  g44<- graficaCol(lista$"3_10")
  g44<- etiquetasHorizontales(g44)
  g44<- rotarEtiX(g44)
  exportarLatex(paste(ruta,"3_10.tex", sep=""),g44)
  
  g45<- graficaBar(lista$"3_11")
  g45<- etiquetasBarras(g45, margenIz = -3)
  exportarLatex(paste(ruta,"3_11.tex", sep=""),g45)
  
  
  g46<- graficaBar(lista$"3_12")
  g46<- etiquetasBarras(g46, margenIz=-3)
  exportarLatex(paste(ruta,"3_12.tex", sep=""),g46)
  
  g47<- graficaCol(lista$"3_13")
  g47<- etiquetasHorizontales(g47)
  g47<- rotarEtiX(g47)
  exportarLatex(paste(ruta,"3_13.tex", sep=""),g47)
  
  
  g48<- graficaBar(lista$"3_14")
  g48<- etiquetasBarras(g48, margenIz = 0)
  exportarLatex(paste(ruta,"3_14.tex", sep=""),g48)

    
  g50<- graficaLineaTrim(lista$"4_01")
  exportarLatex(paste(ruta,"4_01.tex", sep=""),g50)
  
  
  g51<- graficaCol(lista$"4_02", ordenar = F)
  g51 <- etiquetasHorizontales(g51)
  g51 <- rotarEtiX(g51)
  exportarLatex(paste(ruta,"4_02.tex", sep=""),g51)
  
  
  g52<- graficaLineaTrim(lista$"4_03")
  exportarLatex(paste(ruta,"4_03.tex", sep=""),g52)
  
  
  g53<- graficaCol(lista$"4_04", ordenar = F)
  g53 <- etiquetasHorizontales(g53)
  g53 <- rotarEtiX(g53)
  exportarLatex(paste(ruta,"4_04.tex", sep=""),g53)
  
  g60<- graficaCol(lista$"4_06", ordenar = F)
  g60 <- etiquetasHorizontales(g60)
  g60 <- rotarEtiX(g60)
  exportarLatex(paste(ruta,"4_06.tex", sep=""),g60)
  
  g63<- graficaBar(lista$"4_09")
  g63<- etiquetasBarras(g63, margenIz = -4)
  exportarLatex(paste(ruta,"4_09.tex", sep=""),g63)
  
  g64<- graficaCol(lista$"4_10")
  g64<- etiquetasHorizontales(g64)
  g64<- rotarEtiX(g64)
  exportarLatex(paste(ruta,"4_10.tex", sep=""),g64)
  
  g65<- graficaBar(lista$"4_11")
  g65<- etiquetasBarras(g65, margenIz = -3)
  exportarLatex(paste(ruta,"4_11.tex", sep=""),g65)
  
  
  g66<- graficaBar(lista$"4_12")
  g66<- etiquetasBarras(g66, margenIz=-2)
  exportarLatex(paste(ruta,"4_12.tex", sep=""),g66)
  
  g67<- graficaCol(lista$"4_13")
  g67<- etiquetasHorizontales(g67)
  g67<- rotarEtiX(g67)
  exportarLatex(paste(ruta,"4_13.tex", sep=""),g67)
  
  
  g68<- graficaBar(lista$"4_14")
  g68<- etiquetasBarras(g68, margenIz = -4)
  exportarLatex(paste(ruta,"4_14.tex", sep=""),g68)
}