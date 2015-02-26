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
  print(ordenar)
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
  print(nuevoOrden)
  return(nuevoOrden)
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
  print(posiciones)
  return(posiciones)
}

#'Le pone las etiquetas a una grafica de linea
#'
#'@param graph Objeto del tipo ggplot2 que desea anotar
#'@param posiciones Vector de posiciones en que van las etiquetas
etiquetasLineas <- function(graph, posiciones)
{
  d <- ggplot2::ggplot_build(graph)$data[[1]]
  for(i in 1:length(posiciones))
  {
    dato <- d$y[[i]]
    d$etiqueta <- as.numeric(completarEtiquetas(dato,i,tam = length(d$x)))
    if(sonEnteros(d) == 0)
    {
      if(posiciones[[i]] == 1)
      {
        graph <- graph + ggplot2::geom_text(data = d, ggplot2::aes(label=ifelse(is.na(etiqueta),"",formatC(etiqueta,format = "f",big.mark = ",", digits = 1)),family="Open Sans Condensed Light"),size=3.2,hjust = 0.5, vjust = -0.5)
      }else if(posiciones[[i]] == -1)
      {
        graph <- graph + ggplot2::geom_text(data = d,ggplot2::aes(label=ifelse(is.na(etiqueta),"",formatC(etiqueta,format = "f",big.mark = ",", digits = 1)),family="Open Sans Condensed Light"),size=3.2,hjust = 0.5, vjust = 1.5)
      }else if(posiciones[[i]] == 0.5)
      {
        graph <- graph +ggplot2::geom_text(data =d,ggplot2::aes(label=ifelse(is.na(etiqueta),"",formatC(etiqueta,format = "f",big.mark = ",", digits = 1)),family="Open Sans Condensed Light"),size=3.2,hjust = 0, vjust = -0.5)
      }
      else
      {
        graph <- graph + ggplot2::geom_text(data = d,ggplot2::aes(label=ifelse(is.na(etiqueta),"",formatC(etiqueta,format = "f",big.mark = ",", digits = 1)),family="Open Sans Condensed Light"),size=3.2,hjust = 1.2, vjust = 0)
      }      
    }
    else
    {if(posiciones[[i]] == 1)
    {
      graph <- graph + ggplot2::geom_text(data = d, ggplot2::aes(label=ifelse(is.na(etiqueta),"",formatC(etiqueta,format = "f",big.mark = ",", digits = 1, drop0trailing = T)),family="Open Sans Condensed Light"),size=3.2,hjust = 0.5, vjust = -0.5)
    }else if(posiciones[[i]] == -1)
    {
      graph <- graph + ggplot2::geom_text(data = d,ggplot2::aes(label=ifelse(is.na(etiqueta),"",formatC(etiqueta,format = "f",big.mark = ",", digits = 1, drop0trailing = T)),family="Open Sans Condensed Light"),size=3.2,hjust = 0.5, vjust = 1.5)
    }else if(posiciones[[i]] == 0.5)
    {
      graph <- graph + ggplot2::geom_text(data =d,ggplot2::aes(label=ifelse(is.na(etiqueta),"",formatC(etiqueta,format = "f",big.mark = ",", digits = 1, drop0trailing = T)),family="Open Sans Condensed Light"),size=3.2,hjust = 0, vjust = -0.5)
    }
    else
    {
      graph <- graph + ggplot2::geom_text(data = d,ggplot2::aes(label=ifelse(is.na(etiqueta),"",formatC(etiqueta,format = "f",big.mark = ",", digits = 1, drop0trailing = T)),family="Open Sans Condensed Light"),size=3.2,hjust = 1.2, vjust = 0)
    }
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
    ggplot2::theme(plot.margin = grid::unit(c(longitud,0,0,0), "mm"))
  
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
    ggplot2::theme(plot.margin = grid::unit(c(longitud,0,0,0), "mm"))
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
      ggplot2::geom_text(ggplot2::aes(family = "Open Sans Condensed Light",label= formatC(y,format = "f",big.mark = ",", digits = 1, drop0trailing = T)), size=3, hjust=-0.5, vjust = 0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(0,longitud,0,0), "mm")) 
  }
  else
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = "Open Sans Condensed Light",label= formatC(y,format = "f",big.mark = ",", digits = 1)), size=3, hjust=-0.5, vjust = 0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(0,longitud,0,0), "mm")) 
  }
  
}


#'Pone etiquetas a las columnas en una grafica de columnas
#'
#'@param graph El objeto ggplot2 que se desea anotar
#'@return Objeto ggplot2 anotado listo para usar
#'@export
etiquetasHorizontales <- function(graph)
{
  longitud <- 4
  if(sonEnteros(ggplot2::ggplot_build(graph)$data[[1]]) == 0)
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = "Open Sans Condensed Light",label= formatC(y,format = "f",digits = 1,big.mark = ",", drop0trailing = T)),size=3, hjust=0.5, vjust = -0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(longitud,0,0,0), "mm"))
  }
  else
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = "Open Sans Condensed Light",label= formatC(y,format = "f",digits = 0,big.mark = ",")),size=3, hjust=0.5, vjust = -0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(longitud,0,0,0), "mm"))
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
      ggplot2::geom_text(ggplot2::aes(family = "Open Sans Condensed Light",label= formatC(y, big.mark = ",", format = "f", digits =1, drop0trailing = T)), angle = 90, size=3, hjust=-0.1, vjust = 0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(longitud,0,0,0), "mm"))  
  }
  else
  {
    graph <- graph +
      ggplot2::geom_text(ggplot2::aes(family = "Open Sans Condensed Light",label= formatC(y, big.mark = ",", format = "f", digits =0)), angle = 90, size=3, hjust=-0.1, vjust = 0.5)+
      ggplot2::theme(plot.margin = grid::unit(c(longitud,0,0,0), "mm"))
  }
}


#'Exporta a codigo tikz en LaTeX usando tikzDevice
#'
#'@param nombre Ruta del fichero LaTeX
#'@param graph Objeto ggplot2 que se desea exportar a LaTeX
exportarLatex <- function(nombre = grafica.tex, graph)
{
  tikzDevice::tikz(nombre, standAlone = F, bareBones = TRUE, bg = "transparent",width = pkg.env$ancho, height= pkg.env$alto, sanitize = F)
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
      rampa = c(grDevices::rgb(1,1,1), grDevices::rgb(0.8,0.8,0.8), pkg.env$gris)
    }else{
      rampa = c(grDevices::rgb(1,1,1), grDevices::rgb(0.8,0.8,0.8))
    }
  }else{
    rampaAux <- grDevices::colorRampPalette(c(grDevices::rgb(1,1,1), grDevices::rgb(0.8,0.8,0.8)))
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
  nombres <- names(tabla)
  nombres <- gsub("\\.", " ", nombres)
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
#'@export 

rampaColAgrupadas <- function(data){
  rampa = NULL
  if(nrow(subset(data, y>0)) == 0){
    print("No hay valores negativos")
    if(toupper("Ignorado") %in% toupper(data$categoria)){
      print("Hay ignorados")
      rampa = c(grDevices::rgb(1,1,1), grDevices::rgb(0,0,0), pkg.env$gris)
    }else{
      print("No hay ignorados")
      rampaAux = grDevices::colorRampPalette(c(grDevices::rgb(0.3,0.3,0.3), grDevices::rgb(0.5,0.5,0.5)))
      rampa = rampaAux(length(levels(dataL)))
    }
  }else {
    print("Hay valores negativos")
    rampaAux = grDevices::colorRampPalette(c(grDevices::rgb(0.2,0.2,0.2), grDevices::rgb(0.4,0.4,0.4)))
    rampa = rampaAux(2)
    rampa = c(rampa, pkg.env$gris)
  }
  return(rampa)
}

#' Función para hacer carga masiva 
#' de archivos CSV para su posterior uso con una lista 
#' dentro de R.
#' 
#' @param ruta Ruta dentro del disco duro en la cual están contenidos los CSV
#' @return Una lista con los data frame que contiene la información.
#' @export

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
    read.csv(i, sep = ";", encoding = "Latin-1")
  })
  filenames <- gsub(".csv","", filenames)
  names(All) <- basename(filenames)
  tablas <- lapply(All,fact2Num)
  tablas <- lapply(tablas, cambiarCodificacion)
  return(tablas)
}

#'Función que calcula el cambio interanual para un data frame dado
#'
#'@param data El data frame sobre el cual se desea hacer el calculo
#'@param paso El paso de retroceso para el calculo
#'@return Cambio interanual
#'
cambioInterAnual <- function(data, paso = 4){
    cambio <- ( data$y[length(data$y)] / data$y[length(data$y) - paso] ) *100
    return(abs(100-cambio))
}


