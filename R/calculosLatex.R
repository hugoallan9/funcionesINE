#'Función que calcula el alto de un cuadrado de texto dentro de LaTeX
#'@param factor Es el factor de escalamiento para el texto dentro del
#'código tikz
#'@param texto Es el texto que se desea poner dentro del rectángulo en LaTeX
#'@param largo Es la longitud máxima para el cuadro de texto. Dicha longitud debe ser ingresada en milimetros.
#'@return Alto del cuadro que enmarca al texto ingresado. Este valor es devuelto en puntos.
#'@export

calcularAlto <- function( texto, factor = 0.90, largo = 20 ){
  ##Preparando el archivo tex que se usará para calcular 
  ##el alto del texto
  texDir <- tempdir()
  texLog <- normalizePath(file.path(texDir, 'calculoAltura.log'), '/', FALSE)
  texFile <- normalizePath(file.path(texDir, 'calculoAltura.tex'), '/', FALSE)
  
  ## Creando los archivos
  ##file.create(texLog)
  ##file.create(texFile)
  
  # Abrir el texto con permisos de escritura.
  texIn <- file(texFile, 'w')
  
  # Escritura del preámbulo para el archivo LaTeX
  writeLines(getOption("tikzDocumentDeclaration"), texIn)
  
  writeLines("\\usepackage[active,tightpage,xetex]{preview}", texIn)
  
  writeLines("\\usepackage{fontspec,xunicode}", texIn)
  
  writeLines("\\PreviewEnvironment{pgfpicture}", texIn)
  
  writeLines("\\setlength\\PreviewBorder{0pt}", texIn)
  
  writeLines("\\setlength\\PreviewBorder{0pt}", texIn)
  
  writeLines("\\usetikzlibrary{calc}", texIn)

  writeLines("\\usetikzlibrary{positioning}", texIn)
  
  writeLines("\\usepackage{fontspec,xunicode}", texIn)
  
  writeLines("\\setmainfont{Open Sans Condensed Light}", texIn)
  
  writeLines("\\usetikzlibrary{calc}", texIn)
  
  writeLines("\\batchmode", texIn)
  
  #Escritura del cuerpo del documento
  writeLines("\\begin{document}", texIn)
  
  writeLines("\\begin{tikzpicture}", texIn)
  
  writeLines(paste("\\node[inner sep=0pt, outer sep=0pt, text width =",as.character(mm2pt(largo)), 
              ", scale = " ,as.character(factor),"] (TeX){", sep = ""), texIn)
  

  
  
  #\node[inner sep=0pt, outer sep=0pt, ,text width= 56.692913328pt ,scale=0.90] (TeX) {Indígena esto es una pruebita};
  #\path let \p1 = ($(TeX.north) - (TeX.south)$), 
  #\n1 = {veclen(\x1,\y1)} in (TeX.east) -- (TeX.west)
  #node{ \typeout{tikzTeXWidth=\n1} };
  #\makeatletter
  #%\@@end
  
  close(texIn)

}