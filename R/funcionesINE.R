#' Hace una paleta del color indicado
#' 
#' @param data El data frame con la informacion
#' @param color1 El color primario con el cual se desea hacer la paleta
#' @param color2 El color secundario que va cuando encuentra palabras clave
#' @return Vector con la rampa de colores
calcularRampa <- function(data, color1= pkg.env$color1, color2 = pkg.env$color2)
{
  rampa = NULL
  for(elemento in data$x)
  {
    if(elemento %in% pkg.env$ignorado)
    {
      rampa = c(rampa,pkg.env$gris)
    }
    else if(elemento %in% pkg.env$repu)
    {
      rampa = c(rampa, color2)
    }
    else
    {
      rampa = c(rampa,color1)
    }
  }
  return(rampa)
}

#' Funcion de uso interno para ordenar los niveles según los datos
#' @param data El data frame con el cual se desean hacer los calculos
#' @param ordenar Booleno que indica si se deben ordenar los datos de mayor a menor
#' @return Vector con los valores en el orden deseado
ordenarNiveles <- function(data, ordenar = TRUE)
{
  nuevoOrden <- NULL
  ignNombre <- NULL
  ign = 0
  orden <- NULL
  if(ordenar)
  {
    orden <- order(data$y, decreasing = T)
    
  }
  else
  {
    for(i in (1:length(data$x)))
      orden <- c(orden,i)
  }
  for(elemento in orden)
  {
    if( data[elemento,]$x %in% pkg.env$ignorado)
    {
      ign = 1
      pos <- elemento
    }
    else{
      nuevoOrden <- c(nuevoOrden, elemento)
    }
  }
  
  if(ign == 1)
  {
    nuevoOrden <- c(nuevoOrden, pos)
  }
  
  return(nuevoOrden)
}


#'Función para ordenar los niveles de un data frame 
#'excluyendo ciertas palabras claves, como ignorado y otros. 
#'Se puede hacer personalizable eligiendo los identificadores de los
#'niveles que se desean excluir.
#'@param data El data frame con el que se desea trabajar
#'@param palabras Vector de palabras claves que se desean excluir,
#'@return Data frame ordenado y con las exclusiones de los niveles

excluirNiveles <- function(data, palabras = pkg.env$exclusion){
  temp <- NULL
  orden <- NULL
  orden <- order(data$y, decreasing = T)
  nuevoOrden <- NULL
  for(elemento in orden)
  {
    if( !(tolower(data[elemento,]$x) %in% palabras) )
    {
      nuevoOrden <- c(nuevoOrden, elemento)
    }
  }  
  
  return(data[nuevoOrden,])
}


#'Funcion en fase que beta, en teoria mide el ancho de una palabra y determina si existe el espacio
#'suficiente para que quepa en la grafica
#'@param graph Objeto del tipo ggplot2 que se desea modificar
#'@param ancho El ancho de las barras en porcentaje, segun indicaciones de ggplot2
existeTraslape <- function(graph,ancho = 0.6)
{
  ejeX <- 99.1 *0.0393700787
  etiquetas <- ggplot2::ggplot_build(graph)$panel$ranges[[1]]$x.labels
  tam <- list()
  for(i in 1:length(etiquetas))
  {
    tam[i] <- tikzDevice::getLatexStrWidth(etiquetas[[i]], cex = ancho)   
  }
  lapply(tam, pt2mm)
  nuBarras <- length(etiquetas)
  semiEspacio <- 99.1/(2*nuBarras)
}

#' Convierte de puntos a milimetros
#' @param unidad Valor numerico que desea ser convertido
#' @return Valor en milimetros
#' @export
pt2mm <- function(unidad)
{
  return (unidad*0.352777778)
}

#' Funcion experimental que determina cuando un dato es entero o no 
#' solamente datos enteros o del tipo flotante
#' @param x Dato numerico
#' @return Un valor booleano indicando si el dato es entero o flotante
#' @export
is.wholenumber <-
function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

#' Funcion experimental que determina si un vector numerico 
#' esta conformado exclusivamente por valores entero o no
#' @param Vector numerico
#' @return Un valor booleano indicando si el vector es de enteros o no
sonEnteros <- function(data)
{
  contador = 0
  res <- FALSE
  for(i in (1:length(data$y))){
    if(is.wholenumber(data$y[[i]]) == FALSE){
      break
    }
  else{
    contador = contador +1
    }
  }
  if(contador == length(data$y)){
    res <- 1
  }
  else{
    res <-0
  }
  return(res)
}

#' Funcion para convertir de pulgadas a milimetros
#' @param number Dato a ser convertido
#' @return Valor en milimetros
#' @export
inc2mm <-function(number)
{
  return(number*25.4)
}

#' Funcion para convertir de pulgadas a puntos
#' @param number Dato a ser convertido
#' @return Valor en puntos
inc2pt <- function(number)
{
  return(number*72)
}

#'Funcion para convertir de milimetros a pulgadas
#'@param number Dato a ser convertido
#'@return Valor en pulgadas
mm2inch <- function(number)
{
  return(number*0.0393700787)
}

#'Funcion para convertir de milimetros a puntos
#'@param number Dato a ser convertido
#'@return Valor en puntos
mm2pt <- function(number)
{
  return(mm2inch(inc2pt(number)))
}

#'Funcion que calcula las posiciones para las etiquetas en las graficas de linea
#'
#'@param graph Objeto del tipo ggplot2 al cual se le quiere poner las etiquetas
#'@return Un vector indicando las posiciones de las etiquetas

calcularPosiciones <- function(graph)
{
  #SIMBOLOGIA
  # 1 HACIA ARRIBA
  #-1 HACIA ABAJO
  #0.5 A LA DERECHA
  #-0.5 A LA IZQUIERDA
  data <- ggplot2::ggplot_build(graph)$data[[1]]
  print(data)
  posiciones <- NULL
  if(data$y[[1]] < data$y[[2]])
  {
    posiciones <- c(posiciones, -1)
  }
  else{
    posiciones <- c(posiciones, 1)  
  }
  
  
  for(i in 2:(length(data$y)-1))
  {
    if(data$y[[i-1]] == data$y[[i]])
    {
      if(data$y[[i+1]] > data$y[[i]])
      {
        posiciones <- c(posiciones, -1)
      }else if(data$y[[i+1]] == data$y[[i]])
      {
        posiciones <- c(posiciones, 1)
      }else if(data$y[[i+1]] < data$y[[i]]){
        posiciones <- c(posiciones, 1)
      }
    }else if(data$y[[i-1]] > data$y[[i]])
    {
      if(data$y[[i]] > data$y[[i+1]])
      {
        posiciones <- c(posiciones, 0.5)
      }
      else{
        posiciones <- c(posiciones, -1)
      }
    }else
    {
      if(data$y[[i]] < data$y[[i+1]])
      {
        posiciones <- c(posiciones, -0.5)
      }
      else
      {
        posiciones <- c(posiciones, 1)
      }
    }
  }
  if(data$y[[length(data$y)]] == data$y[[length(data$y)-1]])
  {
    posiciones <- c(posiciones, 1)
  }else if(data$y[[length(data$y)]] < data$y[[length(data$y)-1]])
  {
    posiciones <- c(posiciones, -1)
  }else
  {
    posiciones <- c(posiciones, 1)
  }
  print("Las etiquetas son: ")
  print( posiciones )
  return(posiciones)
}


#'Funcion que calcula las posiciones para las etiquetas en las graficas de linea Doble
#'
#'@param graph Objeto del tipo ggplot2 al cual se le quiere poner las etiquetas
#'@return Un vector indicando las posiciones de las etiquetas

calcularPosicionesDobles <- function(graph)
{
  #SIMBOLOGIA
  # 1 HACIA ARRIBA
  #-1 HACIA ABAJO
  #0.5 A LA DERECHA
  #-0.5 A LA IZQUIERDA
  data <- ggplot2::ggplot_build(graph)$data[[1]]
  data2 <- ggplot2::ggplot_build(graph)$data[[2]]
  posiciones <- NULL
  posiciones2 <- NULL
  
  
  ##Etiquetado para la primera linea
  if(data$y[[1]] < data$y[[2]])
  {
    posiciones <- c(posiciones, -1)
  }
  else{
    posiciones <- c(posiciones, 1)  
  }
  
  
  for(i in 2:(length(data$y)-1))
  {
    if(data$y[[i-1]] == data$y[[i]])
    {
      if(data$y[[i+1]] > data$y[[i]])
      {
        posiciones <- c(posiciones, -1)
      }else if(data$y[[i+1]] == data$y[[i]])
      {
        posiciones <- c(posiciones, 1)
      }else if(data$y[[i+1]] < data$y[[i]]){
        posiciones <- c(posiciones, 1)
      }
    }else if(data$y[[i-1]] > data$y[[i]])
    {
      if(data$y[[i]] > data$y[[i+1]])
      {
        posiciones <- c(posiciones, 0.5)
      }
      else{
        posiciones <- c(posiciones, -1)
      }
    }else
    {
      if(data$y[[i]] < data$y[[i+1]])
      {
        posiciones <- c(posiciones, -0.5)
      }
      else
      {
        posiciones <- c(posiciones, 1)
      }
    }
  }
  if(data$y[[length(data$y)]] == data$y[[length(data$y)-1]])
  {
    posiciones <- c(posiciones, 1)
  }else if(data$y[[length(data$y)]] < data$y[[length(data$y)-1]])
  {
    posiciones <- c(posiciones, -1)
  }else
  {
    posiciones <- c(posiciones, 1)
  }
  
  
  ##Etiquetado para la segunda linea
  if(data2$y[[1]] < data2$y[[2]])
  {
    posiciones2 <- c(posiciones2, -1)
  }
  else{
    posiciones2 <- c(posiciones2, 1)  
  }
  
  
  for(i in 2:(length(data2$y)-1))
  {
    if(data2$y[[i-1]] == data2$y[[i]])
    {
      if(data2$y[[i+1]] > data2$y[[i]])
      {
        posiciones2 <- c(posiciones2, -1)
      }else if(data2$y[[i+1]] == data2$y[[i]])
      {
        posiciones2 <- c(posiciones2, 1)
      }else if(data2$y[[i+1]] < data2$y[[i]]){
        posiciones2 <- c(posiciones2, 1)
      }
    }else if(data2$y[[i-1]] > data2$y[[i]])
    {
      if(data2$y[[i]] > data2$y[[i+1]])
      {
        posiciones2 <- c(posiciones2, 0.5)
      }
      else{
        posiciones2 <- c(posiciones2, -1)
      }
    }else
    {
      if(data2$y[[i]] < data2$y[[i+1]])
      {
        posiciones2 <- c(posiciones2, -0.5)
      }
      else
      {
        posiciones2 <- c(posiciones2, 1)
      }
    }
  }
  if(data2$y[[length(data2$y)]] == data2$y[[length(data2$y)-1]])
  {
    posiciones2 <- c(posiciones2, 1)
  }else if(data2$y[[length(data2$y)]] < data2$y[[length(data2$y)-1]])
  {
    posiciones2 <- c(posiciones2, -1)
  }else
  {
    posiciones2 <- c(posiciones2, 1)
  }
  
  
  
  
  print("Las etiquetas son: ")
  print( list(posiciones, posiciones2)  )
  return( list(posiciones, posiciones2) )
}

#'Le pone las etiquetas a una grafica de linea
#'
#'@param graph Objeto del tipo ggplot2 que desea anotar
#'@param posiciones Vector de posiciones en que van las etiquetas
#'@param precision Numero de decimales con el que se desea el vector respuesta.
#'Por defecto se usa un decimal
etiquetasLineas <- function(graph, posiciones, precision=1)
{
  print(c('La precision es: ', precision ))
  pre <- precision
  print(pre)
  d <- ggplot2::ggplot_build(graph)$data[[1]]
  enteros <- sonEnteros(d)
  
  
  lista <- NULL
  lista <- c(lista,max(d$y), min(d$y))
  if( !(d$y[[1]] %in% lista) ){
    lista <- c(lista, d$y[[1]])
  }
  
  if( !(d$y[[length(posiciones)]] %in% lista) ){
    lista <- c(lista, d$y[[length(posiciones)]])
  }
  print(lista)
  maximo <- F
  minimo <- F
  for(i in 1:length(posiciones))
  {
    dato <- d$y[[i]] 
    ultimo <- d$y[[length(posiciones)]]
    if( !(dato %in% lista) ){
      dato <- NA
    }else{
      if(i != length(posiciones) ){
        lista <- lista[lista != dato ]
      }
    }
    
    if(enteros == 0)
    {
      d$etiqueta <- formatC(as.numeric(completarEtiquetas(dato,i,tam = length(d$x))), format = 'f', big.mark = ',', digits = pre)
    }
    else
    {
      d$etiqueta <- formatC(as.numeric(completarEtiquetas(dato,i,tam = length(d$x))), format = 'f', big.mark = ',', digits = pre, drop0trailing = T)
    }
    
    print("#####LAS ETIQUETAS SON ##########" )
    print(d$etiqueta)
    
    if(posiciones[[i]] == 1)
    {
      graph <- graph + ggplot2::geom_text(data = d, ggplot2::aes(label=ifelse(etiqueta == 'NA' ,"",etiqueta),family="Open Sans Condensed Light"),size=pkg.env$tamEti,hjust = 0.5, vjust = -0.5)
    }else if(posiciones[[i]] == -1)
    {
      graph <- graph + ggplot2::geom_text(data = d,ggplot2::aes(label=ifelse(etiqueta == 'NA',"",etiqueta),family="Open Sans Condensed Light"),size=pkg.env$tamEti,hjust = 0.5, vjust = 1.5)
    }else if(posiciones[[i]] == 0.5)
    {
      graph <- graph +ggplot2::geom_text(data =d,ggplot2::aes(label=ifelse(etiqueta == 'NA',"", etiqueta),family="Open Sans Condensed Light"),size=pkg.env$tamEti,hjust = 0, vjust = -0.5)
    }
    else
    {
      graph <- graph + ggplot2::geom_text(data = d,ggplot2::aes(label=ifelse(etiqueta == 'NA',"",etiqueta),family="Open Sans Condensed Light"),size=pkg.env$tamEti,hjust = 1.2, vjust = 0)
    }
    
    
    
  }
  return(graph)
}



#'Le pone las etiquetas a una grafica de linea
#'
#'@param graph Objeto del tipo ggplot2 que desea anotar
#'@param posiciones Vector de posiciones en que van las etiquetas
#'@param precision Numero de decimales con el que se desea el vector respuesta.
#'Por defecto se usa un decimal
#'@return Gráfica con las etiquetas puestasw
#'@export
etiquetasLineasDobles <- function(graph, pos, precision=1)
{
  print(c('La precision es: ', precision ))
  pre <- precision
  d1 <- ggplot2::ggplot_build(graph)$data[[1]]
  d2 <- ggplot2::ggplot_build(graph)$data[[2]]
  enteros1 <- sonEnteros(d1)
  enteros2 <- sonEnteros(d2)
  posiciones <- pos[[1]]
  posiciones2 <- pos[[2]]
  
  
  for(i in 1:length(posiciones))
  {
    dato <- d1$y[[i]] 
    
    if(enteros1 == 0)
    {
      d1$etiqueta <- formatC(as.numeric(completarEtiquetas(dato,i,tam = length(d1$x))), format = 'f', big.mark = ',', digits = pre)
    }
    else
    {
      d1$etiqueta <- formatC(as.numeric(completarEtiquetas(dato,i,tam = length(d1$x))), format = 'f', big.mark = ',', digits = pre, drop0trailing = T)
    }
    
    
    if(posiciones[[i]] == 1)
    {
      graph <- graph + ggplot2::geom_text(data = d1, ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA' ,"",etiqueta),family="Open Sans Condensed Light"),size=pkg.env$tamEti,hjust = 0.5, vjust = -0.5)
    }else if(posiciones[[i]] == -1)
    {
      graph <- graph + ggplot2::geom_text(data = d1,ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA',"",etiqueta),family="Open Sans Condensed Light"),size=pkg.env$tamEti,hjust = 0.5, vjust = 1.5)
    }else if(posiciones[[i]] == 0.5)
    {
      graph <- graph +ggplot2::geom_text(data =d1,ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA',"", etiqueta),family="Open Sans Condensed Light"),size=pkg.env$tamEti,hjust = 0, vjust = -0.5)
    }
    else
    {
      graph <- graph + ggplot2::geom_text(data = d1,ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA',"",etiqueta),family="Open Sans Condensed Light"),size=pkg.env$tamEti,hjust = 1.2, vjust = 0)
    }
    
  }
  
  
  for(i in 1:length(posiciones2))
  {
    dato <- d2$y[[i]] 
    
    if(enteros1 == 0)
    {
      d2$etiqueta <- formatC(as.numeric(completarEtiquetas(dato,i,tam = length(d2$x))), format = 'f', big.mark = ',', digits = pre)
    }
    else
    {
      d2$etiqueta <- formatC(as.numeric(completarEtiquetas(dato,i,tam = length(d2$x))), format = 'f', big.mark = ',', digits = pre, drop0trailing = T)
    }
    
    
    if(posiciones2[[i]] == 1)
    {
      graph <- graph + ggplot2::geom_text(data = d2, ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA' ,"",etiqueta),family="Open Sans Condensed Light"),size=pkg.env$tamEti,hjust = 0.5, vjust = -0.5)
    }else if(posiciones2[[i]] == -1)
    {
      graph <- graph + ggplot2::geom_text(data = d2,ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA',"",etiqueta),family="Open Sans Condensed Light"),size=pkg.env$tamEti,hjust = 0.5, vjust = 1.5)
    }else if(posiciones2[[i]] == 0.5)
    {
      graph <- graph +ggplot2::geom_text(data =d2,ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA',"", etiqueta),family="Open Sans Condensed Light"),size=pkg.env$tamEti,hjust = 0, vjust = -0.5)
    }
    else
    {
      graph <- graph + ggplot2::geom_text(data = d2,ggplot2::aes(y=y,label=ifelse(etiqueta == 'NA',"",etiqueta),family="Open Sans Condensed Light"),size=pkg.env$tamEti,hjust = 1.2, vjust = 0)
    }
    
  }
  
  return(graph)
}



#'Funcion interna que le pone el relleno al vector de etiquetas
#'del tipo ("","",etiqueta,"",..)
#'
#'@param dato Dato que va en el vector
#'@param posicion Es la posicion que ocupa el dato dentro del vector
#'@param tam El tamano del vector de salida
#'@return El vector completo rellenado de la forma ("", "", dato, "", ...)
completarEtiquetas <- function(dato,posicion, tam = 5)
{
  etiquetas <- NULL
  for(i in 1:tam)
  {
    if(i == posicion)
    {
      etiquetas <- c(etiquetas, dato)
    }
    else
    {
      etiquetas <- c(etiquetas,"")  
    }
  }
  return(etiquetas)
}


#'Funcion que rota las etiquetas en el eje X para graficas de columnas que poseen
#'etiquetas horizontales para las columnas 
#'
#'@param graph El objeto ggpot2 que se desea modificar
#'@return Objeto ggplot2 modificado en las etiquetas del eje X
rotarEtiX <- function(graph)
{
  
  longitud <- 2.8 + 2
  graph <- graph + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust =0.5 , hjust= 1))+
    ggplot2::theme(plot.margin = grid::unit(c(longitud,0,3,0), "mm"))
  
}

#'Funcion que rota las etiquetas en el eje X para graficas de columnas que poseen
#'etiquetas verticales para las columnas 
#'
#'@param graph El objeto ggpot2 que se desea modificar
#'@return Objeto ggplot2 modificado en las etiquetas del eje X
rotarEtiX2 <- function(graph)
{
  max <-ggplot2::ggplot_build(graph)$panel$ranges[[1]]$y.range[2]
  longitud <- tikzDevice::getLatexStrWidth(formatC(max,format = "f",big.mark = ",", digits = 1), cex = pkg.env$fEscala) 
  longitud <- longitud*0.352777778 + 1
  graph <- graph + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust =0.5 , hjust= 1))+
    ggplot2::theme(plot.margin = grid::unit(c(longitud,0,3,0), "mm"))
}


#' Anota las etiquetas para una grafica de barras
#' 
#' @param graph Objeto ggplot2 que se desea anotar
#' @param margenIz Controla el margen izquierdo de la grafica
#' @return Retorna objeto ggplot2 listo para graficar
etiquetasBarras <- function(graph, margenIz = 0  )
{
  max <-ggplot2::ggplot_build(graph)$panel$ranges[[1]]$x.range[2]
  longitud <- tikzDevice::getLatexStrWidth(formatC(max,format = "f",big.mark = ",", digits = 1), cex = pkg.env$fEscala) 
  longitud <- longitud*0.352777778 + 2.3
  mIz <- 0 + margenIz
  if(sonEnteros(ggplot2::ggplot_build(graph)$data[[1]]) == 0)
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = "Open Sans Condensed Light",label= formatC(y,format = "f",big.mark = ",", digits = 1)), size=3, hjust=-0.5, vjust = 0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(0,longitud,0,6), "mm")) 
  }
  else
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = "Open Sans Condensed Light",label= formatC(y,format = "f",big.mark = ",", digits = 1,drop0trailing = T)), size=3, hjust=-0.5, vjust = 0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(0,longitud,0,6), "mm")) 
  }
  
}


#'Pone etiquetas a las columnas en una grafica de columnas
#'
#'@param graph El objeto ggplot2 que se desea anotar
#'@return Objeto ggplot2 anotado listo para usar
#'@export
etiquetasHorizontales <- function(graph, precision = 1)
{
  longitud <- 4
  if(sonEnteros(ggplot2::ggplot_build(graph)$data[[1]]) == 0)
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = "Open Sans Condensed Light",label= formatC(y,format = "f",digits = 1,big.mark = ",", drop0trailing = F)),size=3, hjust=0.5, vjust = -0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(longitud,0,3,0), "mm"))
  }
  else
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = "Open Sans Condensed Light",label= formatC(y,format = "f",digits = 1,big.mark = ",", drop0trailing = T)),size=3, hjust=0.5, vjust = -0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(longitud,0,3,0), "mm"))
  }
}



#'Pone etiquetas verticales a las columnas en una grafica de columnas 
#'
#'@param graph El objeto ggplot2 que se desea anotar
#'@return Objeto ggplot2 anotado listo para usar
#'@export
etiquetasVerticales <- function(graph)
{
  max <-ggplot2::ggplot_build(graph)$panel$ranges[[1]]$y.range[2] 
  longitud <- tikzDevice::getLatexStrWidth(formatC(max,format = "f",big.mark = ",", digits = 1), cex = pkg.env$fEscala) 
  longitud <- longitud*0.352777778 + 1
  if(sonEnteros(ggplot2::ggplot_build(graph)$data[[1]]) == 0)
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = "Open Sans Condensed Light",label= formatC(y, big.mark = ",", format = "f", digits =1)), angle = 90, size=3, hjust=-0.1, vjust = 0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(longitud,0,0,0), "mm"))  
  }
  else
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = "Open Sans Condensed Light",label= formatC(y, big.mark = ",", format = "f", digits =0, drop0trailing = T)), angle = 90, size=3, hjust=-0.1, vjust = 0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(longitud,0,0,0), "mm"))
  }
}


#'Exporta a codigo tikz en LaTeX usando tikzDevice
#'
#'@param nombre Ruta del fichero LaTeX
#'@param graph Objeto ggplot2 que se desea exportar a LaTeX
exportarLatex <- function(nombre = grafica.tex, graph, preambulo = F)
{
  tikzDevice::tikz(nombre, standAlone = preambulo, bareBones = TRUE, bg = "transparent",width = pkg.env$ancho, height= pkg.env$alto, sanitize = F)
  temp<- ggplot2::ggplot_gtable(ggplot2::ggplot_build(graph))
  temp$layout$clip[temp$layout$name=="panel"] <- "off"
  grid::grid.draw(temp)
  dev.off()
}


#'Calcula la rampa de colores para usar en las graficas
#'de anillo, mandando los ignorados hasta el final y haciendo
#'la separcion por categorias (blanco para una categoría y negro para la otra)
#'El gris siempre se usa para ignorado
#'
#'@param x El vector de datos en el cual se basa la paleta de colores
#'@param categoria Booleano que indica si se desea categorizar la rampa
#'@return El vector de la paleta de colores
#'@export 

calcularRampaAnillo <- function(x, categoria = TRUE){
  rampa = NULL
  if(categoria == TRUE){
    if("IGNORADO" %in% toupper(x))
    {
      #print("IGNORADO")
      rampa = c(grDevices::rgb(1,1,1), grDevices::rgb(0.6,0.6,0.6), pkg.env$gris)
    }else{
      rampa = c(grDevices::rgb(1,1,1), grDevices::rgb(0.6,0.6,0.6))
    }
  }else{
    rampaAux <- grDevices::colorRampPalette(c(grDevices::rgb(1,1,1), grDevices::rgb(0.6,0.6,0.6)))
    if("IGNORADO" %in% toupper(x)){
      rampa <- c(rampaAux(2), pkg.env$gris)
    }else{
      rampa <- rampaAux(length(x))
    }
  }
  return(rampa)
}


#' Convierte los factores de un data frame a datos numericos
#' 
#'@param tabla El data frame que se quiere trabajar
#'@return Regresa el data frame con valores numéricos en lugar de factores
#'@export  
fact2Num <- function(tabla)
{
  print(tabla)
  nombres <- names(tabla)
  names(tabla) <- c("x","y")
  if(is.factor(tabla$y))
  {
    tabla$y<- as.numeric(levels(tabla$y))[tabla$y]    
  }
  else
  {
    tabla$y<- as.numeric(tabla$y)   
  }
  names(tabla) <- nombres
  #print(names(tablas))
  return(tabla)
}

#'Cambia la codificación en ciertas partes del data frame
#'@param tabla Es el data frame al cual se le desea cambiar la 
#'codificacion
#'@return Data frame recodificado
#'

cambiarCodificacion <- function(tabla){
  print(names(tabla))
  nombres <- names(tabla)
  if(nombres[1] == "X")
    nombres[1] <- "x"
  if(nombres[2] == "Y")
    nombres[2] <- "y"
  names(tabla) <- nombres
  ##nombres <- gsub("\\.", " ", nombres)
  nombres <- iconv(nombres, to = "UTF8//TRANSLIT")
  names(tabla) <- nombres
  x <- tabla$x
  x <- iconv(x, to = "UTF8//TRANSLIT")
  tabla$x <- x
  return(tabla)
}

#'Función que comila con xelatex y muestra el resultado
#'@param ruta Es la ruta de donde se desea guardar el fichero .tex
#'@param mostrar Booleano, cuando es verdadero muestra el pdf compilado.
#'@export
compilar <- function(ruta = "", mostrar = T){
  shell(cmd=paste("cd", dirname(ruta), "&&xelatex  --synctex=1 --interaction=nonstopmode",ruta), mustWork=TRUE, intern=TRUE, translate=TRUE)
  if( mostrar == T){
    shell.exec(paste(dirname(ruta), gsub(".tex",".pdf",basename(ruta)), sep="/"))  
  }
  
}



preview <- function(graph)
{
  nombre = tempfile(pattern="Preview", tmpdir= paste(normalizePath(getwd()),"Temporal", sep="\\"))
  tikz(paste(nombre,".tex", sep= ""), standAlone = TRUE, bg = "transparent",bareBones = FALSE, width = 3.19, height= 1.91, sanitize= F)
  temp<- ggplot_gtable(ggplot_build(graph))
  temp$layout$clip[temp$layout$name=="panel"] <- "off"
  grid.draw(temp)
  dev.off()
  shell(cmd=paste("xelatex   --synctex=1 --interaction=nonstopmode", "--output-directory",dirname(nombre),paste(nombre,".tex", sep="")))
  shell.exec(paste(nombre,".pdf", sep=""))
}


#'Calcula la rampa de colores para usar en las graficas
#'de columnas agrupadas, mandando los ignorados hasta el final y haciendo
#'la separcion por categorias (blanco para una categoría y negro para la otra)
#'El gris siempre se usa para ignorado
#'
#'@param data  El data frame con el que se harán los calculos
#'@return El vector de la paleta de colores

rampaColAgrupadas <- function(data){
  rampa = NULL
  rampa1 = NULL
  if(nrow(subset(data, y>0)) > 0){
    print("No hay valores negativos")
    if(toupper("Ignorado") %in% toupper(data$categoria)){
      print("Hay ignorados")
      if( pkg.env$modalidad == "trimestral"){
        rampa = c(grDevices::rgb(1,1,1), grDevices::rgb(0.5,0.5,0.5), pkg.env$gris)
        rampa1 = c(grDevices::rgb(0,0,0), grDevices::rgb(0.5,0.5,0.5), pkg.env$gris)
      }
      
    }else{
      print("No hay ignorados")
      if( pkg.env$modalidad == "trimestral"){
        print("La modalidad es trimestral")
        rampaAux = grDevices::colorRampPalette(c(grDevices::rgb(1,1,1), grDevices::rgb(0.5,0.5,0.5)))
        rampaAux1 <- grDevices::colorRampPalette(c(grDevices::rgb(0,0,0), grDevices::rgb(0.5,0.5,0.5)))
        rampa = rampaAux(length(levels(data$categoria)))
        rampa1 = rampaAux1(length(levels(data$categoria)))  
      }else if(pkg.env$modalidad == "anual" || pkg.env$modalidad == "presentacion"){
        print("Es anual o presentacion")
        rampaAux = grDevices::colorRampPalette( c(pkg.env$color1, pkg.env$color2 ) )
        rampaAux1 <- rampaAux
        if (length(levels(data$categoria) ) == 3 ){
          print("Hay 3 categorias")
          print(levels(data$categoria))
          rampa = c(rampaAux(2), pkg.env$gris)
          rampa1 = c(rampaAux(2), pkg.env$gris)
        }else{
          rampa = rampaAux(length(levels(data$categoria)))
          rampa1 = rampaAux1(length(levels(data$categoria)))  
        }
        
      }
      
    }
  }else {
    print("Hay valores negativos")
    rampaAux = grDevices::colorRampPalette(c(grDevices::rgb(0.2,0.2,0.2), grDevices::rgb(0.4,0.4,0.4)))
    rampa = rampaAux(2)
    rampa = c(rampa, pkg.env$gris)
  }
  return(list(rampa,rampa1))
}

#' Función para hacer carga masiva 
#' de archivos CSV para su posterior uso con una lista 
#' dentro de R.
#' 
#' @param ruta Ruta dentro del disco duro en la cual están contenidos los CSV
#' @return Una lista con los data frame que contiene la información.

cargaMasiva <- function (ruta, recodificar = F) {
  filenames <- list.files(path = ruta, pattern = ".csv", full.names = TRUE)
  numfiles <- length(filenames)
  fn <- list.files(path = paste(tempdir(), "CSV", sep = "/") , full.names = TRUE)
  file.remove(fn)
  if ( recodificar ){
    unlink(paste(tempdir(), "CSV", sep = "/"), force = T)
    dir.create(path = paste(tempdir(), "CSV", sep = "/") )
    for (i in 1:numfiles)
    {
      shell(cmd=paste("iconv -f ISO-8859-1 -t UTF-8 <\"",filenames[[i]],"\">", paste(tempdir(),"CSV", basename(filenames[[i]]),sep="/"), sep = ""), mustWork=TRUE, intern=F, translate=TRUE)
    }
    dir <- paste(tempdir(),"CSV", sep = "/")
  }else{
    dir <- ruta
  }
  
  
  filenames <- list.files(path = dir, pattern = ".csv", full.names = TRUE)
  All <- lapply(filenames,function(i){
    read.csv(i,header = TRUE, sep = ";",  fileEncoding="iso-8859-1", check.names = F)
  })
  filenames <- gsub(".csv","", filenames)
  names(All) <- basename(filenames)
  tablas <- lapply(All,fact2Num)
  tablas <- lapply(tablas, cambiarCodificacion)
  return(tablas)
}

#'Función que calcula el cambio interanual en porcentaje para un data frame dado
#'
#'@param data El data frame sobre el cual se desea hacer el calculo
#'@param paso El paso de retroceso para el calculo
#'@return Cambio interanual
#'
cambioInterAnual <- function(data, primeraPos = 5, ultimaPos = 9){
    cambio <- ( data$y[ultimaPos] / data$y[primeraPos] ) *100
    return(abs(100-cambio))
}


#'Función que calcula el cambio interanual neto para un data frame dado
#'
#'@param data El data frame sobre el cual se desea hacer el calculo
#'@param paso El paso de retroceso para el calculo
#'@return Cambio interanual
#'
cambioInterAnualNeto <- function(data, paso = 4){
  cambio <- ( data$y[length(data$y)] - data$y[length(data$y) - paso] )
  return(abs(cambio))
}

#'Función para poner parametrización del formato de trimestrales
trimestral <- function(){
  pkg.env$alto <- 1.91 
  pkg.env$ancho <- 3.19
  options(tikzDocumentDeclaration= "\\documentclass[10pt,twoside]{book}")
  pkg.env$modalidad <- "trimestral"
  cambiarGraficas(tamFuente = 10)
}

#'Función para poner parametrización del formato anual
#'@param color1 Color primario para graficar, definido por el usuario. Este debe ser definido 
#'con el formato rgb(v1,v2,v3, maxColorValue = 255), si maxColorValue no está definido
#'se usa por defecto 1. 
#'@param color2 Color secundario para graficar, definido por el usuario con el formato 
#'rgb(v1,v2,v3, maxColorValue = 255), si maxColorValue no está definido
#'se usa por defecto 1. 
anual <- function(color1, color2){
  pkg.env$alto <- 2.75
  pkg.env$ancho <- 4
  options(tikzDocumentDeclaration= "\\documentclass[11pt,twoside]{book}")
  pkg.env$fontSize = 11
  pkg.env$color1 <- color1
  pkg.env$color2 <- color2
  pkg.env$colorRelleno <- color1
  pkg.env$modalidad <- "anual"
  cambiarGraficas(tamFuente = 11)
  pkg.env$tamEti <- 3.7
}

#'Función para poner parametrización del formato de trimestrales
presentacion <- function(){
  pkg.env$alto <- 1.91 
  pkg.env$ancho <- 3.19
  options(tikzDocumentDeclaration= "\\documentclass[10pt,twoside]{book}")
  pkg.env$modalidad <- "trimestral"
  cambiarGraficas(tamFuente = 10)
  pkg.env$tamEti <- 3.2
  pkg.env$color1 <- rgb(0,0,1) #0 0 0
  pkg.env$color2 <- rgb(0.3,0.7,1)
  pkg.env$colorRelleno <- rgb(0,0,1) # 1 1 1 
}

