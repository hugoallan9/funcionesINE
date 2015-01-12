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
  
  options(tikzDocumentDeclaration= "\\documentclass[10pt,twoside]{book}")
}