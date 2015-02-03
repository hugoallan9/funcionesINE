#' Funcion basica para graficar Columnas
#' @param data Data frame para elaborar la grafica
#' @param color1 Color con el que se desea hacer la grafica
#' @param color2 Color secundario para las graficas
#' @param ancho Porcentaje que ocupan las columnas, segun paquete ggplot2
#' @param ordenar Booleano que indica si los datos deben ser ordenados
graficaCol <- function(data, color1=pkg.env$color1, ancho = 0.6, ordenar = TRUE)
{
  ggplot2::theme_set(pkg.env$temaColumnas)
  names(data)<- c("x","y")
  data <- data[ordenarNiveles(data, ordenar),]
  data$x <- factor(data$x, levels = data$x)
  levels(data$x) <- gsub("\\\\n", "\n", levels(data$x))
  grafica <- ggplot2::ggplot(data, ggplot2::aes(x, y))
  grafica <- grafica + 
    ggplot2::geom_bar(stat = 'identity', colour = calcularRampa(data, color1), fill = calcularRampa(data,NA), width = ancho, position =  "dodge")+
    ggplot2::labs(x=NULL,y=NULL)+
    ggplot2::scale_y_continuous(breaks=NULL, expand= c(0.0,0.0))+
    ggplot2::geom_abline(intercept = 0, slope = 0)
  return(grafica)
}

#'Hace graficas de Barras
#'@param data Data frame con el cual se hace la grafica
#'@param color1 El color principal para la grafica
#'@param ancho Ancho de las barras en porcentaje, segun ggplot2
#'@param ordenar Booelano que indica si los datos deben ser ordenados o no
graficaBar <- function(data, color1=pkg.env$color1, ancho = 0.6, ordenar = TRUE)
{
  ggplot2::theme_set(pkg.env$temaBarras)
  names(data)<- c("x","y")
  data <- data[rev(ordenarNiveles(data, ordenar)),]
  data$x <- factor(data$x, levels = data$x)
  levels(data$x) <- gsub("\\n", "\n", levels(data$x))
  grafica <- ggplot2::ggplot(data, ggplot2::aes(x, y))
  grafica <- grafica + 
    ggplot2::geom_bar(stat = 'identity',fill = calcularRampa(data, NA), colour = calcularRampa(data, color1), width = ancho, position =  "dodge")+
    ggplot2::labs(x=NULL,y=NULL)+
    ggplot2::scale_y_continuous(breaks=NULL, expand= c(0.0,0.0))+
    ggplot2::coord_flip()
  return(grafica)
}

#'Hace una grafica de linea, util para series historicas
#'
#'@param data El data frame para hacer la gráfica
#'@param color1 El color en el que se desea la linea
#'@param inicio El dato desde donde se quiere que se visualice la gráfica
#'@param ancho El grosor de la linea
#'@export

graficaLinea <- function(data, color1 = pkg.env$color1, inicio = 0, ancho = 1.7)
{
  ggplot2::theme_set(pkg.env$temaColumnas)
  names(data)<- c("x","y")
  grafica <- ggplot2::ggplot(data, ggplot2::aes(x,y))
  grafica <- grafica + ggplot2::geom_line( colour = color1, size = ancho)+
    ggplot2::labs(x=NULL,y=NULL)
  grafica <- etiquetasLineas(grafica, calcularPosiciones(grafica))
  minimo <- min(ggplot2::ggplot_build(grafica)$data[[1]]$y)
  maximo <- max(ggplot2::ggplot_build(grafica)$data[[1]]$y)
  limite <- minimo - 0.3*(maximo - minimo)
  if(ggplot2::ggplot_build(grafica)$data[[1]]$y[1] > 3)
  {
    grafica <- grafica + ggplot2::scale_y_continuous(limits = c(limite,NA))+
      ggplot2::theme(plot.margin = grid::unit(c(2.5,3,0,-7), "mm"))
  }
  else
  {
    grafica <- grafica + ggplot2::scale_y_continuous(limits = c(limite,NA))+
      ggplot2::theme(plot.margin = unit(c(2.5,3,0,-4), "mm"))
  }
  return(grafica)
}

#'Genera graficas de lineas para series historicas de los trimestrales
#'
#'@param data El data frame con el que se hará la grafica
#'@param color1 El color en el que se desea la linea
#'@param inicio El punto en el eje y a partir del cual se desea mostrar la grafica
#'@param El ancho de la linea
#'@return El objeto ggplot2 listo para grafica
#'@export
graficaLineaTrim <- function(data, color1 = color, inicio = 0, ancho = 0.5)
{
  ggplot2::theme_set(pkg.env$temaColumnas)
  names(data)<- c("x","y")
  nomX <- data$x
  data$x <- factor(data$x, as.character(data$x))
  grafica <- ggplot2::ggplot(data, ggplot2::aes(x,y, group = 1))
  grafica <- grafica + ggplot2::geom_line( colour = color1, size = ancho)+
    ggplot2::labs(x=NULL,y=NULL)+
    ggplot2::theme(axis.text.x = ggplto2::element_text(family = "Open Sans Condensed Light",angle = 90, vjust =0.5 , hjust= 1))
  grafica <- etiquetasLineas(grafica, calcularPosiciones(grafica))
  minimo <- min(ggplot2::ggplot_build(grafica)$data[[1]]$y)
  maximo <- max(ggplot2::ggplot_build(grafica)$data[[1]]$y)
  limite <- minimo - 0.5*(maximo - minimo)
  if(ggplot2::ggplot_build(grafica)$data[[1]]$y[1] > 3)
  {
    grafica <- grafica + ggplot2::scale_y_continuous(limits = c(limite,NA))+
      ggplot2::theme(plot.margin = grid::unit(c(2.5,3,0,-7), "mm")) #-9
  }
  else
  {
    grafica <- grafica + ggplot2::scale_y_continuous(limits = c(limite,NA))+
      ggplot2::theme(plot.margin = grid::unit(c(2.5,3,0,-3), "mm")) #-4
  }
  return(grafica)
}

#' Hace graficas de anillo para dos y tres categorias
#' 
#' @param data Data frame con el cual se hace el anillo
#' @param nombre Ruta en la cual se desea generar el tex
#' @return No retorna
#' @export
graficaAnillo <- function(data, nombre)
{
  names(data)<- c("x","y")
  data <- data[ordenarNiveles(data),]
  data$x <- factor(data$x, levels = data$x)
  data$x <- factor(data$x, as.character(data$x))
  tikzDevice::tikz(nombre, standAlone = TRUE, bg = "transparent",bareBones = FALSE, width = 3.19, height= 1.91, sanitize= F)
  ggplot2::theme_set(pkg.env$temaAnillo)
  data$ymax = cumsum(data$y)
  data$ymin = c(0, head(data$ymax, n=-1))
  y.breaks <- cumsum(data$y)-data$y/2
  colores <- calcularRampaAnillo(data$x, categoria = F)
  p1 <- ggplot2::ggplot(data, ggplot2::aes(fill =  x, ymax = ymax, ymin = ymin ,xmax= 10, xmin= 5))+
    #geom_rect(show_guide=F)+
    ggplot2::geom_rect(fill = colores ,colour= "black", show_guide=F)+
    ggplot2::labs(x=NULL, y=NULL)+
    ggplot2::scale_y_continuous(breaks = y.breaks, labels=data$y, expand = c(0,0))+
    ggplot2::labs(x = NULL, y=NULL)+
    ggplot2::coord_polar(theta ="y")+
    ggplot2::xlim(c(0,10 ))+
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  temp<- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p1))
  temp$layout$clip[temp$layout$name=="panel"] <- "off"
  grid::grid.draw(temp)
  if(length(data$y) == 2){
    tikzDevice::tikzCoord(2*3.19/3, 1.91/2, name= "rect", units = "inches") ## ESTA ES LA QUE FUNCIONA 
    tikzDevice::tikzCoord(0,mm2inch(2.5+ 4), name = "desY", units= "inches")
    tikzDevice::tikzCoord(mm2inch(2.5),mm2inch(0+ 4), name = "desX", units = "inches")
    tikzDevice::tikzCoord(mm2inch(2.5),-mm2inch(0+ 4), name = "mdesX", units = "inches")
    tikzDevice::tikzAnnotate("\\definecolor[named]{ct1}{rgb}{0.0,0.0,0.0}")
    tikzDevice::tikzAnnotate("\\coordinate (t1) at ($(rect) + 0.5*(desX) + 0.5*(desY)$);")
    tikzDevice::tikzAnnotate("\\coordinate (t2) at ($(rect)+0.5*(mdesX)-0.5*(desY)$);")
    tikzDevice::tikzAnnotate(c("\\draw [color=ct1] ($(rect)+(desY)$) rectangle ($(rect)+(desX)$);"))
    tikzDevice::tikzAnnotate(c("\\node [text width=",
                    mm2pt(20), 
                   ",right= 0.3cm of t1,scale = 0.9]{", as.character(data$x[[1]]),"};"))
    tikzDevice::tikzAnnotate(c("\\path [fill=blue] ($(rect)-(desY)$) rectangle ($(rect)+(mdesX)$);"))
    tikzDevice::tikzAnnotate(c("\\node [text width=",
                   mm2pt(20), 
                   ",right= 0.3cm of t2,scale = 0.9]{",as.character(data$x[[2]]),"};"))  
  }else{
    tikzDevice::tikzCoord(2*3.19/3, 1.91/2, name= "rect", units = "inches") ## ESTA ES LA QUE FUNCIONA 
    tikzDevice::tikzCoord(0,mm2inch(1.25 + 0), name = "desY", units= "inches")
    tikzDevice::tikzCoord(mm2inch(2.5),mm2inch(0-1.25), name = "desX", units = "inches")
    tikzDevice::tikzCoord(mm2inch(2.5),-mm2inch(0+ 4), name = "mdesX", units = "inches")
    tikzDevice::tikzCoord(mm2inch(1.25),0, name="tdesX", units = "inches")
    tikzDevice::tikzCoord(0,mm2inch(6+1.25), name ="tdesY", units = "inches")
    tikzDevice::tikzCoord(0,mm2inch(6), name = "espacio", units = "inches")
    tikzDevice::tikzCoord(0, mm2inch(2.5), name = "lonY", units = "inches")
    tikzDevice::tikzCoord(mm2inch(2.5),0, name = "lonX", units = "inches")
    tikzDevice::tikzAnnotate("\\definecolor[named]{ct1}{HTML}{000000}")
    tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct2}{HTML}{",substr(colores[2],2,7),"}"))
    tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct3}{HTML}{",substr(colores[3],2,7),"}"))
    tikzDevice::tikzAnnotate("\\coordinate (t2) at ($(rect) +0.5*(lonX)$);")
    tikzDevice::tikzAnnotate("\\coordinate (t1) at ($(rect)+ 0.5*(lonX) + (lonY) + (espacio) $);")
    tikzDevice::tikzAnnotate("\\coordinate (t3) at ($(rect) + 0.5*(lonX) - (lonY) - (espacio)$);")
    tikzDevice::tikzAnnotate(c("\\draw [color=ct1] ($(rect)+1.5*(lonY) + (espacio)$) rectangle ($(rect)+(lonX)+ 0.5*(lonY) + (espacio)$);"))
    tikzDevice::tikzAnnotate(c("\\node [text width=",
                   mm2pt(20), 
                   ",right= 0.3cm of t1,scale = 0.9]{",as.character(data$x[[1]]),"};"))
    tikzDevice::tikzAnnotate(c("\\path [fill=ct2] ($(rect)+0.5*(lonY)$) rectangle ($(rect)+(lonX)-0.5*(lonY)$);"))
    tikzDevice::tikzAnnotate(c("\\node [text width=",
                   mm2pt(20), 
                   ",right= 0.3cm of t2,scale = 0.9]{", as.character(data$x[[2]]),"};"))
    tikzDevice::tikzAnnotate(c("\\path [fill=ct3] ($(rect)-1.5*(lonY) - (espacio)$) rectangle ($(rect)+(lonX)- 0.5*(lonY) - (espacio)$);"))
    tikzDevice::tikzAnnotate(c("\\node [text width=",
                   mm2pt(20), 
                   ",right= 0.3cm of t3,scale = 0.9]{",as.character(data$x[[3]]),"};"))
      
  }
  dev.off()
}

#' Graficas de columnas por categorias
#' 
#' @param data El data frame para hacer la grafica, con el formato de tres o mas columnas de la forma y,z,w,...
#' @param etiquetasCategorias Indica la posición en las que se desea poner las etiquetas para las categorias.
#' Por defecto la posición es a la derecha, lo que se denota con la letra "I", cuando las etiquetas se desean en la parte
#' superior de la grafica se debe indicar con la letra "A"
#' @param escala Indica la escala en la cual debe estar el eje y de la grafica. Por defecto se encuentra en normal. Las opciones
#' son "miles", "millones" o "milesmillones".
#' @param ruta Nombre de la ruta en la que se desea guardar la salida del archivo tex para su compilacion
#' @param etiquetas Posición de las etiquetas. Por defecto se ponen verticales, para pasar a horizontales de escribir "h"
#' @return No regresa ningun valor

graficaColCategorias <- function(data, etiquetasCategorias = "A", escala = "normal", ruta, etiquetas = "v"){
  tikzDevice::tikz(ruta, standAlone = TRUE, bg = "transparent",bareBones = FALSE, width = pkg.env$ancho, height= pkg.env$alto, sanitize= F)
  x <- rep(data$x,length(data)-1)
  y <- NULL
  for(i in 2:length(data)){
    y <- c(y,as.matrix(data)[,i])
  }
  categoria <- gl(length(data)-1, length(data$x), labels = names(data))
  dataLista <- data.frame(x,y,categoria)
  dataLista <- fact2Num(dataLista)
  if(toupper(escala) == "MILES"){
    dataLista$y <- dataLista$y/1000
  }else if(toupper(escala) == "MILLONES"){
    dataLista$y <- dataLista$y/1000000
  }
  
  colores <-   rampaColAgrupadas(dataLista)
  print(colores)
  dataLista$x <- as.character(dataLista$x)
  ggplot2::theme_set(pkg.env$temaColumnas)
  grafica <- ggplot2::ggplot(dataLista, ggplot2::aes(x = x, y = y, fill = categoria))+
    ggplot2::geom_bar(stat = 'identity', position =  "dodge")+
    ggplot2::labs(x=NULL, y=NULL)+
    ggplot2::scale_y_continuous(breaks=NULL, expand = c(0,0))+
    ggplot2::geom_abline(intercept = 0, slope = 0)+
    #ggplot2::scale_x_discrete()+
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = NULL),
      panel.background = ggplot2::element_rect(fill = NULL),
      plot.margin = grid::unit(c(0,0,0,0),"mm")
      )+
    ggplot2::scale_fill_manual(values=colores)+
    ggplot2::guides(fill = F)+
    ggplot2::geom_text(ggplot2::aes(familly = "Open Sans Condensed Light",label=formatC(y,format = "f",big.mark = ",", digits = 1)), position=ggplot2::position_dodge(width=0.9),size=3.2, angle = 90, hjust=-0.2, vjust = 0.5)
  
  altoRect <- max(calcularAlto(names(data)[2]), calcularAlto(names(data)[3]))
  print(altoRect)
  
  if ( toupper(etiquetas) == "V" ){
    max <-ggplot2::ggplot_build(grafica)$panel$ranges[[1]]$y.range[2] 
    longitud <- tikzDevice::getLatexStrWidth(formatC(max,format = "f",big.mark = ",", digits = 1), cex = pkg.env$fEscala) 
    longitud <- pt2mm(longitud + altoRect) + 1.2 + 2 ## Se contempla la distancia de las barras a las etiquetas y de las etiquetas a la leyenda
    print(c(" La longitud en mm es: ", longitud))
    grafica <- grafica + ggplot2::theme(
      plot.margin = grid::unit(c(longitud,0,0,0),"mm")
    )
  }
  
  
  temp<- ggplot2::ggplot_gtable(ggplot2::ggplot_build(grafica))
  temp$layout$clip[temp$layout$name=="panel"] <- "off"
  grid::grid.draw(temp)
  if(etiquetasCategorias == "D"){
    if(length(data$y) == 2){
      tikzDevice::tikzCoord(2*6/3, 5/2, name= "rect", units = "inches") ## ESTA ES LA QUE FUNCIONA 
      tikzDevice::tikzCoord(0,mm2inch(2.5+ 4), name = "desY", units= "inches")
      tikzDevice::tikzCoord(mm2inch(2.5),mm2inch(0+ 4), name = "desX", units = "inches")
      tikzDevice::tikzCoord(mm2inch(2.5),-mm2inch(0+ 4), name = "mdesX", units = "inches")
      tikzDevice::tikzAnnotate("\\definecolor[named]{ct1}{rgb}{0.0,0.0,0.0}")
      tikzDevice::tikzAnnotate("\\coordinate (t1) at ($(rect) + 0.5*(desX) + 0.5*(desY)$);")
      tikzDevice::tikzAnnotate("\\coordinate (t2) at ($(rect)+0.5*(mdesX)-0.5*(desY)$);")
      tikzDevice::tikzAnnotate(c("\\draw [color=ct1] ($(rect)+(desY)$) rectangle ($(rect)+(desX)$);"))
      tikzDevice::tikzAnnotate(c("\\node [text width=",
                                 mm2pt(20), 
                                 ",right= 0.3cm of t1,scale = 0.9]{", as.character(data$x[[1]]),"};"))
      tikzDevice::tikzAnnotate(c("\\path [fill=blue] ($(rect)-(desY)$) rectangle ($(rect)+(mdesX)$);"))
      tikzDevice::tikzAnnotate(c("\\node [text width=",
                                 mm2pt(20), 
                                 ",right= 0.3cm of t2,scale = 0.9]{",as.character(data$x[[2]]),"};"))  
    }else{
      tikzDevice::tikzCoord(2*pkg.env$ancho/3, pkg.env$alto/2, name= "rect", units = "inches") ## ESTA ES LA QUE FUNCIONA 
      tikzDevice::tikzCoord(0,mm2inch(1.25 + 0), name = "desY", units= "inches")
      tikzDevice::tikzCoord(mm2inch(2.5),mm2inch(0-1.25), name = "desX", units = "inches")
      tikzDevice::tikzCoord(mm2inch(2.5),-mm2inch(0+ 4), name = "mdesX", units = "inches")
      tikzDevice::tikzCoord(mm2inch(1.25),0, name="tdesX", units = "inches")
      tikzDevice::tikzCoord(0,mm2inch(6+1.25), name ="tdesY", units = "inches")
      tikzDevice::tikzCoord(0,mm2inch(6), name = "espacio", units = "inches")
      tikzDevice::tikzCoord(0, mm2inch(2.5), name = "lonY", units = "inches")
      tikzDevice::tikzCoord(mm2inch(2.5),0, name = "lonX", units = "inches")
      tikzDevice::tikzAnnotate("\\definecolor[named]{ct1}{HTML}{000000}")
      tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct2}{HTML}{",substr(colores[2],2,7),"}"))
      tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct3}{HTML}{",substr(colores[3],2,7),"}"))
      tikzDevice::tikzAnnotate("\\coordinate (t2) at ($(rect) +0.5*(lonX)$);")
      tikzDevice::tikzAnnotate("\\coordinate (t1) at ($(rect)+ 0.5*(lonX) + (lonY) + (espacio) $);")
      tikzDevice::tikzAnnotate("\\coordinate (t3) at ($(rect) + 0.5*(lonX) - (lonY) - (espacio)$);")
      tikzDevice::tikzAnnotate(c("\\draw [color=ct1] ($(rect)+1.5*(lonY) + (espacio)$) rectangle ($(rect)+(lonX)+ 0.5*(lonY) + (espacio)$);"))
      tikzDevice::tikzAnnotate(c("\\node [text width=",
                                 mm2pt(20), 
                                 ",right= 0.3cm of t1,scale = 0.9]{",as.character( (names(data))[2] ),"};"))
      tikzDevice::tikzAnnotate(c("\\path [fill=ct2] ($(rect)+0.5*(lonY)$) rectangle ($(rect)+(lonX)-0.5*(lonY)$);"))
      tikzDevice::tikzAnnotate(c("\\node [text width=",
                                 mm2pt(20), 
                                 ",right= 0.3cm of t2,scale = 0.9]{", as.character( (names(data))[3] ),"};"))
      tikzDevice::tikzAnnotate(c("\\path [fill=ct3] ($(rect)-1.5*(lonY) - (espacio)$) rectangle ($(rect)+(lonX)- 0.5*(lonY) - (espacio)$);"))
      tikzDevice::tikzAnnotate(c("\\node [text width=",
                                 mm2pt(20), 
                                 ",right= 0.3cm of t3,scale = 0.9]{",as.character( (names(data))[4] ),"};"))
      
    }
    }else{
      if(length(levels(dataLista$categoria)) -1 == 2){
        ## Caluculando las posiciones de las etiquetas para que quede centrado
        lonEtiqueta1 <- pt2mm(mm2inch(tikzDevice::getLatexStrWidth(names(data)[2], cex = 0.9)))
        lonEtiqueta2 <- pt2mm(mm2inch(tikzDevice::getLatexStrWidth(names(data)[3], cex = 0.9)))
        print(names(data)[3])
        apoyoX <- 0
        separacion <- 0
        print(lonEtiqueta1 + mm2inch(3) + mm2inch(pkg.env$longCuadrado))
        print(lonEtiqueta2 + mm2inch(3) + mm2inch(pkg.env$longCuadrado))
        if( mm2inch(lonEtiqueta1) + mm2inch(3) + mm2inch(pkg.env$longCuadrado) < 0.5 * pkg.env$ancho - pkg.env$tol && 
              mm2inch(lonEtiqueta2) + mm2inch(3) + mm2inch(pkg.env$longCuadrado)  < 0.5 * pkg.env$ancho - pkg.env$tol  )
        {
          print("CASO 1")
           print(paste("El punto medio para la primera etiqueta es: ", 0.5 * ( 0.5 * pkg.env$ancho + pkg.env$tol ), sep = " "))
           apoyoX  <- 0.5 * ( 0.5 * pkg.env$ancho + pkg.env$tol ) -  0.5 * ( mm2inch( lonEtiqueta1 ) + mm2inch(3) + mm2inch(pkg.env$longCuadrado) )
           finEtiqueta1 <- 0.5 * ( 0.5 * pkg.env$ancho + pkg.env$tol ) +  0.5 * ( mm2inch( lonEtiqueta1 ) + mm2inch(3) + mm2inch(pkg.env$longCuadrado) )
           print(paste("El fin de la etiqueta 1 es:" , finEtiqueta1, sep = " "))
           separacion <- ( 0.5 * pkg.env$ancho - finEtiqueta1 ) + 0.5 * ( 0.5 * pkg.env$ancho - pkg.env$tol - ( mm2inch( lonEtiqueta2 ) + mm2inch(3) + mm2inch(pkg.env$longCuadrado) ) )  
        }else if( 1.10 * ( mm2inch( lonEtiqueta1 ) + 2 * mm2inch(3) + 2 * mm2inch(pkg.env$longCuadrado) + mm2inch(lonEtiqueta2) ) <  pkg.env$ancho - 2 * pkg.env$tol){
          print("CASO 2")
          apoyoX <- ( 0.5 * pkg.env$ancho  + pkg.env$tol ) - 0.5 * 1.10 * ( lonEtiqueta1 + 2 * mm2inch(3) + 2 * mm2inch(pkg.env$longCuadrado) + lonEtiqueta2 )
          separacion <-  mm2inch(pkg.env$longCuadrado) + mm2inch(3)  + mm2inch( lonEtiqueta1 )  + 0.10 * ( mm2inch(lonEtiqueta1) + 2 * mm2inch(3) + 2 * mm2inch(pkg.env$longCuadrado) + mm2inch(lonEtiqueta2) ) 
        }
        
        
        print(paste("El valor de apoyo es:" , apoyoX, sep = " "))
        print(paste("La separción es:" , separacion, sep = " "))
        print(paste("La tolerancia es:" , pkg.env$tol, sep = " "))
        print(paste("La distancia de un cuadro a otro es:" ,separacion + lonEtiqueta1, sep = " "))
        tikzDevice::tikzCoord(apoyoX, 1.91-mm2inch(pt2mm(altoRect)), name= "apoyo", units = "inches") ## ESTA ES LA QUE FUNCIONA 
        tikzDevice::tikzCoord(mm2inch(pkg.env$longCuadrado),mm2inch(pt2mm(altoRect)), name = "longitudFicticia", units= "inches")
        tikzDevice::tikzCoord(mm2inch(pkg.env$longCuadrado),mm2inch(pkg.env$longCuadrado), name = "longitud", units= "inches")
        tikzDevice::tikzCoord(separacion + lonEtiqueta1 + mm2inch(3) + mm2inch(pkg.env$longCuadrado),mm2inch(0), name = "desX", units = "inches")
        tikzDevice::tikzCoord(mm2inch(0), 0.5* mm2inch(pt2mm(altoRect)) - 0.5*mm2inch(pkg.env$longCuadrado), name = "desY", units = "inches")
        #tikzDevice::tikzCoord(mm2inch(10),0, name = "mdesX", units = "inches")
        tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct1}{HTML}{",substr(colores[1],2,7),"}"))
        tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct2}{HTML}{",substr(colores[2],2,7),"}"))
        tikzDevice::tikzAnnotate(c("\\path [fill=none] (apoyo) rectangle ($(apoyo)+(longitudFicticia)$)"))
        tikzDevice::tikzAnnotate(c("node [xshift=0.3cm,inner sep=0pt, outer sep=0pt,text width=",
                                   inc2pt(0.5 * pkg.env$ancho - pkg.env$tol - mm2inch(3) - mm2inch(pkg.env$longCuadrado ) ), 
                                   ",midway,right,scale = 0.9, draw]{", as.character( names(data)[2] ),"};"))
        tikzDevice::tikzAnnotate(c("\\path [fill=ct1] ( $(apoyo)  + (desY) $) rectangle ($(apoyo)+ (desY) +(longitud)$);"))
        tikzDevice::tikzAnnotate(c("\\path [fill=none] ($(apoyo)+(desX)$) rectangle ($(apoyo)+(desX)+(longitudFicticia)$)"))
        tikzDevice::tikzAnnotate(c("node [xshift = 0.3cm, inner sep=0pt, outer sep=0pt,text width=",
                                   inc2pt(0.5 * pkg.env$ancho - pkg.env$tol - mm2inch(3) - mm2inch(pkg.env$longCuadrado ) ), 
                                   ",midway,right,scale = 0.9,draw]{",as.character( names(data)[3] ),"};"))  
        tikzDevice::tikzAnnotate(c("\\path [fill=ct2] ( $(apoyo)  + (desY) + (desX) $) rectangle ($(apoyo)+ (desY)+ (desX) +(longitud)$);"))
        }else{
        tikzDevice::tikzCoord(2*3.19/3, 1.91/2, name= "rect", units = "inches") ## ESTA ES LA QUE FUNCIONA 
        tikzDevice::tikzCoord(0,mm2inch(1.25 + 0), name = "desY", units= "inches")
        tikzDevice::tikzCoord(mm2inch(2.5),mm2inch(0-1.25), name = "desX", units = "inches")
        tikzDevice::tikzCoord(mm2inch(2.5),-mm2inch(0+ 4), name = "mdesX", units = "inches")
        tikzDevice::tikzCoord(mm2inch(1.25),0, name="tdesX", units = "inches")
        tikzDevice::tikzCoord(0,mm2inch(6+1.25), name ="tdesY", units = "inches")
        tikzDevice::tikzCoord(0,mm2inch(6), name = "espacio", units = "inches")
        tikzDevice::tikzCoord(0, mm2inch(2.5), name = "lonY", units = "inches")
        tikzDevice::tikzCoord(mm2inch(2.5),0, name = "lonX", units = "inches")
        tikzDevice::tikzAnnotate("\\definecolor[named]{ct1}{HTML}{000000}")
        tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct2}{HTML}{",substr(colores[2],2,7),"}"))
        tikzDevice::tikzAnnotate(c("\\definecolor[named]{ct3}{HTML}{",substr(colores[3],2,7),"}"))
        tikzDevice::tikzAnnotate("\\coordinate (t2) at ($(rect) +0.5*(lonX)$);")
        tikzDevice::tikzAnnotate("\\coordinate (t1) at ($(rect)+ 0.5*(lonX) + (lonY) + (espacio) $);")
        tikzDevice::tikzAnnotate("\\coordinate (t3) at ($(rect) + 0.5*(lonX) - (lonY) - (espacio)$);")
        tikzDevice::tikzAnnotate(c("\\draw [color=ct1] ($(rect)+1.5*(lonY) + (espacio)$) rectangle ($(rect)+(lonX)+ 0.5*(lonY) + (espacio)$);"))
        tikzDevice::tikzAnnotate(c("\\node [text width=",
                                   mm2pt(20), 
                                   ",right= 0.3cm of t1,scale = 0.9]{",as.character( (names(data))[2] ),"};"))
        tikzDevice::tikzAnnotate(c("\\path [fill=ct2] ($(rect)+0.5*(lonY)$) rectangle ($(rect)+(lonX)-0.5*(lonY)$);"))
        tikzDevice::tikzAnnotate(c("\\node [text width=",
                                   mm2pt(20), 
                                   ",right= 0.3cm of t2,scale = 0.9]{", as.character( (names(data))[3] ),"};"))
        tikzDevice::tikzAnnotate(c("\\path [fill=ct3] ($(rect)-1.5*(lonY) - (espacio)$) rectangle ($(rect)+(lonX)- 0.5*(lonY) - (espacio)$);"))
        tikzDevice::tikzAnnotate(c("\\node [text width=",
                                   mm2pt(20), 
                                   ",right= 0.3cm of t3,scale = 0.9]{",as.character( (names(data))[4] ),"};"))
    }
  }
  grDevices::dev.off()
  return(grafica)
}