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
    ggplot2::scale_y_continuous(breaks=NULL, expand= c(0.0,0.0))
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