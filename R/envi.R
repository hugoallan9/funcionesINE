pkg.env <- new.env()
pkg.env$fEscala <- 0.85039370025172
pkg.env$color1 <- rgb(0,0,0, maxColorValue = 255) #0 0 0
pkg.env$color2 <- rgb(0.5,0.5,0.5)
pkg.env$colorRelleno <- rgb(1,1,1) # 1 1 1 
pkg.env$repu <- c("Total República, Total republica, Total república, Total Republica")
pkg.env$ignorado <- c("Ignorado", "ignorado", "IGNORADO", "Ignorada", "ignorada")
pkg.env$exclusion <- c(pkg.env$ignorado, "otro", "otros", "otra", "otras")
pkg.env$gris <- rgb(200,200,200, maxColorValue = 255)
pkg.env$grisBase <- rgb(152,152,152, maxColorValue = 255)
pkg.env$fontSize <-10
pkg.env$temaINE <- ggplot2::theme_gray(base_size = pkg.env$fontSize, base_family = "Open Sans Condensed Light")+ ggplot2::theme(
  text= ggplot2::element_text(family = "Open Sans Condensed Light", face = "plain", colour='black', size = pkg.env$fontSize),
  axis.text.x = ggplot2::element_text(family = "Open Sans Condensed Light", colour = "black", face = "plain", size = pkg.env$fontSize, hjust = 0.5, vjust =0.5, angle = 0, lineheight = 0.9),
  axis.text.y = ggplot2::element_text(family = "Open Sans Condensed Light", colour = "black", face = "plain", size = pkg.env$fontSize, hjust = 0.5, vjust =0.5, angle = 0, lineheight = 0.9),
  panel.background = ggplot2::element_rect(fill = NA),
  panel.grid = ggplot2::element_line(colour = NA),
  panel.grid.major = ggplot2::element_line(colour = NA),
  panel.grid.minor = ggplot2::element_line(colour = NA),
  panel.grid.major.y =  ggplot2::element_line(colour = NA),
  axis.line = ggplot2::element_line(colour = pkg.env$grisBase),
  plot.margin = rep(grid::unit(0,"null"),4),
  axis.ticks = ggplot2::element_line(colour = NA),
  axis.ticks.x = ggplot2::element_line( size=NULL, color=NA ),
  axis.ticks.y = ggplot2::element_line(size = NULL, color=NA),
  plot.background = ggplot2::element_rect(fill = NA)
)

pkg.env$temaBarras <- pkg.env$temaINE
pkg.env$temaBarras <- pkg.env$temaBarras + ggplot2::theme(
  axis.line.y = ggplot2::element_line(colour = NA),
  axis.line.x = ggplot2::element_line(colour = NA),
  axis.text.y = ggplot2::element_text(hjust = 1, vjust = 0.5)
) 

pkg.env$temaColumnas <- pkg.env$temaINE
pkg.env$temaColumnas <- pkg.env$temaBarras + ggplot2::theme(
  axis.line.x = ggplot2::element_line(colour = NA),
  axis.line.y = ggplot2::element_line(colour = NA),
  axis.text.y = ggplot2::element_text(colour = NA)
)

pkg.env$modalidad = "trimestral"
pkg.env$tamEti <- 3.2




