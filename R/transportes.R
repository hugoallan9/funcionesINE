#'Función encargada de fabricar las graficas para
#'las estadísticas vitales.
#'@param lista Listado de R que contiene los data frame de vitales
#'@param ruta Ruta de salida para los archivos de las gráficas
#'@param modalidad String que puede ser, trimestral o anual. Por defecto
#'se toma como trimestral.
#'
graficasTransportes<- function(lista, ruta, modalidad = "trimestral"){
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
  
  
  t3<- graficaLinea(lista$"1_01", escala = "miles", inicio=2000,precision = 0)
  exportarLatex(paste( sep="", ruta,  "1_01.tex"), t3)
  
  t4 <- graficaLinea(lista$"1_02", escala = "miles", inicio=2000)
  exportarLatex(paste( sep="", ruta,  "1_02.tex"), t4)
  
  g5 <- graficaLinea(lista$"1_03", escala = "miles", inicio=500)
  exportarLatex(paste( sep="", ruta,  "1_03.tex"), g5)
  
  g577 <- graficaLinea(lista$"1_04", escala = "miles", inicio=500)
  exportarLatex(paste( sep="", ruta,  "1_04.tex"), g577)
  
  g588 <- graficaLinea(lista$"1_05", escala = "miles", inicio=50)
  exportarLatex(paste( sep="", ruta,  "1_05.tex"), g588)
  
  g599 <- graficaLinea(lista$"1_06", inicio=5000)
  exportarLatex(paste( sep="", ruta,  "1_06.tex"), g599)
  
  t6 <- graficaLinea(lista$"1_07", inicio=5000)
  exportarLatex(paste( sep="", ruta,  "1_07.tex"), t6)
  
  t7 <- graficaLinea(lista$"1_08", inicio=2000)
  exportarLatex(paste( sep="", ruta,  "1_08.tex"), t7)
  
  
  t9 <- graficaLinea(lista$"1_09", escala = "miles", inicio=50)
  exportarLatex(paste( sep="", ruta,  "1_09.tex"), t9)
  
  
  t13 <- graficaLinea(lista$"1_10", escala = "miles", inicio=50)
  exportarLatex(paste( sep="", ruta,  "1_10.tex"), t13)
  
  
  t10 <- graficaLinea(lista$"1_11", inicio=1000)
  exportarLatex(paste( sep="", ruta,  "1_11.tex"), t10)
  
  
  t11 <- graficaLinea(lista$"1_12", inicio=1000)
  exportarLatex(paste( sep="", ruta,  "1_12.tex"), t11)
  
  
  
  
  
  
  
  t50   <- graficaLinea(lista$"2_01", inicio=1000)
  exportarLatex(paste( sep="", ruta,  "2_01.tex"),t50)
  t51 <- graficaLinea(lista$"2_02", inicio=1000)
  exportarLatex(paste( sep="", ruta,  "2_02.tex"),t51)
  
  t63 <- graficaCol(lista$"2_03")
  t63 <- etiquetasHorizontales(t63)
  t63 <- rotarEtiX2(t63)
  exportarLatex(paste( sep="", ruta,  "2_03.tex"),t63)
  
  t62 <- graficaBar(lista$"2_04")
  t62<- etiquetasBarras(t62)
  exportarLatex(paste( sep="", ruta,  "2_04.tex"),t62)
  
  
  t54 <- graficaLinea(lista$"2_05", inicio=100)
  exportarLatex(paste( sep="", ruta,  "2_05.tex"),t54)
  t55 <- graficaLinea(lista$"2_06", inicio=50)
  exportarLatex(paste( sep="", ruta,  "2_06.tex"),t55)
  t56 <- graficaLinea(lista$"2_07", inicio=500)
  exportarLatex(paste( sep="", ruta,  "2_07.tex"),t56)
  t57 <- graficaLinea(lista$"2_08", inicio=500)
  exportarLatex(paste( sep="", ruta,  "2_08.tex"),t57)
  t58 <- graficaLinea(lista$"2_09", inicio=500)
  exportarLatex(paste( sep="", ruta,  "2_09.tex"),t58)
  t59 <- graficaLinea(lista$"2_10", inicio=50)
  exportarLatex(paste( sep="", ruta,  "2_10.tex"),t59)
  t60 <- graficaLinea(lista$"2_11", inicio=500)
  exportarLatex(paste( sep="", ruta,  "2_11.tex"),t60)
  t61 <- graficaLinea(lista$"2_12", inicio=50)
  exportarLatex(paste( sep="", ruta,  "2_12.tex"),t61)
  
  t52 <- graficaLinea(lista$"2_13", inicio=50)
  exportarLatex(paste( sep="", ruta,  "2_13.tex"),t52)
  t53 <- graficaLinea(lista$"2_14", inicio=0)
  exportarLatex(paste( sep="", ruta,  "2_14.tex"),t53)
  t64 <- graficaLinea(lista$"2_15", inicio=500)
  exportarLatex(paste( sep="", ruta,  "2_15.tex"),t64)
  t65 <- graficaLinea(lista$"2_16", inicio=500)
  exportarLatex(paste( sep="", ruta,  "2_16.tex"),t65)
  t66 <- graficaLinea(lista$"2_17", inicio=0)
  exportarLatex(paste( sep="", ruta,  "2_17.tex"),t66)
  t67 <- graficaLinea(lista$"2_18", inicio=50)
  exportarLatex(paste( sep="", ruta,  "2_18.tex"),t67)
  t68 <- graficaLinea(lista$"2_19", inicio=100)
  exportarLatex(paste( sep="", ruta,  "2_19.tex"),t68)
  t69 <- graficaLinea(lista$"2_20", inicio=500)
  exportarLatex(paste( sep="", ruta,  "2_20.tex"),t69)
  t70 <- graficaLinea(lista$"2_21", inicio=0)
  exportarLatex(paste( sep="", ruta,  "2_21.tex"),t70)
  t71 <- graficaLinea(lista$"2_22", inicio=50)
  exportarLatex(paste( sep="", ruta,  "2_22.tex"),t71)
  t72 <- graficaLinea(lista$"2_23", inicio=50)
  exportarLatex(paste( sep="", ruta,  "2_23.tex"),t72)
  t73 <- graficaLinea(lista$"2_24", inicio=100)
  exportarLatex(paste( sep="", ruta,  "2_24.tex"),t73)
  t74 <- graficaLinea(lista$"2_25", inicio=10)
  exportarLatex(paste( sep="", ruta,  "2_25.tex"),t74)
  t75 <- graficaLinea(lista$"2_26", inicio=50)
  exportarLatex(paste( sep="", ruta,  "2_26.tex"),t75)
  t76 <- graficaCol(lista$"2_27")
  t76<- etiquetasHorizontales(t76)
  exportarLatex(paste( sep="", ruta,  "2_27.tex"),t76)
  
  
  
  
  
  
  t18 <- graficaLinea(lista$"3_01", inicio=10)
  exportarLatex(paste( sep="", ruta,  "3_01.tex"), t18)
  
  
  t20   <- graficaLinea(lista$"3_02", inicio=10)
  exportarLatex(paste( sep="", ruta,  "3_02.tex"), t20)
  t21 <- graficaLinea(lista$"3_03", inicio=10)
  exportarLatex(paste( sep="", ruta,  "3_03.tex"), t21)
  
  
  t22 <- graficaColCategorias(lista$"3_04",ruta = paste( sep="", ruta,  "3_04.tex"),etiquetasCategorias = "A", etiquetas="V", ejeX = "v",ancho = 0.6)
  
  t23 <- graficaColCategorias(lista$"3_05",ruta = paste( sep="", ruta,  "3_05.tex"),etiquetasCategorias = "A", etiquetas="V", ejeX = "v",ancho = 0.6)
  
  t24 <- graficaColCategorias(lista$"3_06",ruta = paste( sep="", ruta,  "3_06.tex"),etiquetasCategorias = "A", etiquetas="V", ejeX = "v",ancho = 0.6)
  
  t25 <- graficaColCategorias(lista$"3_07",ruta = paste( sep="", ruta,  "3_07.tex"),etiquetasCategorias = "A", etiquetas="V", ejeX = "v",ancho = 0.6)
  
  
  t26 <- graficaColCategorias(lista$"3_08",ruta = paste( sep="", ruta,  "3_08.tex"),etiquetasCategorias = "A", etiquetas="h", ejeX = "h",ancho = 0.6)
  
  
  t27 <- graficaLinea(lista$"3_09", inicio=10)
  exportarLatex(paste( sep="", ruta,  "3_09.tex"), t27)
  
  t28 <- graficaLinea(lista$"3_10", inicio=10)
  exportarLatex(paste( sep="", ruta,  "3_10.tex"), t28)
  
  t29 <- graficaLinea(lista$"3_11", inicio=10)
  exportarLatex(paste( sep="", ruta,  "3_11.tex"), t29)
  
  t30 <- graficaLinea(lista$"3_12", inicio=10)
  exportarLatex(paste( sep="", ruta,  "3_12.tex"), t30)
  
  t36 <- graficaLinea(lista$"3_13", inicio=10)
  exportarLatex(paste( sep="", ruta,  "3_13.tex"), t36)
  
  t37 <- graficaLinea(lista$"3_14", inicio=0)
  exportarLatex(paste( sep="", ruta,  "3_14.tex"), t37)
  
  t43 <- graficaLinea(lista$"3_15", inicio=0)
  exportarLatex(paste( sep="", ruta,  "3_15.tex"), t43)
  
  t44 <- graficaLinea(lista$"3_16", inicio=0)
  exportarLatex(paste( sep="", ruta,  "3_16.tex"), t44)
  
  t45 <- graficaLinea(lista$"3_17", inicio=10)
  exportarLatex(paste( sep="", ruta,  "3_17.tex"), t45)
  
  t46 <- graficaLinea(lista$"3_18", inicio=10)
  exportarLatex(paste( sep="", ruta,  "3_18.tex"), t46)
  
  
  t47 <- graficaLinea(lista$"3_19", inicio=10)
  exportarLatex(paste( sep="", ruta,  "3_19.tex"), t47)
  
  t48 <- graficaLinea(lista$"3_20", inicio=10)
  exportarLatex(paste( sep="", ruta,  "3_20.tex"), t48)
  
  
  
  
  
  
  
  t25 <- graficaCol(lista$"4_01", ordenar = "F", escala = "miles")
  t25 <- etiquetasHorizontales(t25)
  exportarLatex(paste( sep="", ruta,  "4_01.tex"), t25)
  
  
  t32 <- graficaBar(lista$"4_02", ordenar = "F")
  t32 <- etiquetasBarras(t32)
  exportarLatex(paste( sep="", ruta,  "4_02.tex"), t32)
  
  
  t26 <- graficaCol(lista$"4_03")
  t26 <- etiquetasVerticales(t26)
  t26 <- rotarEtiX2(t26)
  exportarLatex(paste( sep="", ruta,  "4_03.tex"), t26)
  
  t27 <- graficaBar(lista$"4_04")
  t27 <- etiquetasBarras(t27,margenIz = -10.5)
  exportarLatex(paste( sep="", ruta,  "4_04.tex"), t27)
  
  
  
  t29 <- graficaCol(lista$"4_05", ancho = 0.5)
  t29 <- etiquetasHorizontales(t29)
  exportarLatex(paste( sep="", ruta,  "4_05.tex"), t29)
  
  
  t30 <- graficaLinea(lista$"4_06", inicio = 0)
  exportarLatex(paste( sep="", ruta,  "4_06.tex"), t30)
  
  t31 <- graficaLinea(lista$"4_07", inicio = 0)
  exportarLatex(paste( sep="", ruta,  "4_07.tex"), t31)
  
  
 
}