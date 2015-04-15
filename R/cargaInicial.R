.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Este paquete usa una fuente en especifico, registrandola")
  ## Load all fonts
  extrafont::loadfonts("pdf", quiet = TRUE)
  extrafont::loadfonts("postscript", quiet = TRUE)
  if (.Platform$OS.type == "windows") {
    extrafont::loadfonts("win", quiet = TRUE)
  }
  options(tikzDefaultEngine = "xetex")
  #options(tikzXelatex = "/usr/local/texlive/2014/bin/x86_64-linux/xelatex")
  options(tikzXelatexPackages = c("\\usepackage[T1]{fontenc}",
                                  "\\usepackage{tikz}\n",
                                  "\\usepackage[active,tightpage,xetex]{preview}\n",
                                  "\\usepackage{fontspec,xunicode}\n",
                                  "\\PreviewEnvironment{pgfpicture}\n",
                                  "\\setlength\\PreviewBorder{0pt}\n",
                                  "\\usetikzlibrary{calc}\n",
                                  "\\usetikzlibrary{positioning}\n",
                                  "\\usepackage{fontspec,xunicode}\n",
                                  "\\setmainfont{Open Sans Condensed Light}\n"))
  
  options(tikzUnicodeMetricPackages = c("\\usetikzlibrary{calc}\n"))
  
  pkg.env$temaAnillo <- pkg.env$temaINE
  pkg.env$temaAnillo <- pkg.env$temaAnillo +ggplot2::theme(
    plot.margin = grid::unit(c(0,inc2mm(3.19/4),0,-20),"mm"), axis.line.y = ggplot2::element_line(colour=NA),
    axis.ticks.y = ggplot2::element_line(colour=NA),
    axis.line.x = ggplot2::element_line(colour = NA),
    panel.margin = grid::unit(c(0,inc2mm(3.19/4),0,-20),"mm"),
    axis.text.y = ggplot2::element_text(colour = NA, vjust = -3, hjust = -3),
    axis.text.x = ggplot2::element_text(family = "Open Sans Condensed Light", colour = "black", face = "plain", size = pkg.env$fontSize, hjust = -10, vjust =-10, angle = 0, lineheight = 0.9)
  ) 
  # Variables para el tamaño de la gráfica
  
  pkg.env$alto <- 1.91 
  pkg.env$ancho <- 3.19
  
  # Variables para las legendas
  pkg.env$longCuadrado <- 2.5 #<--- Esta está en mm
  pkg.env$tol <- pkg.env$ancho/40
  
}