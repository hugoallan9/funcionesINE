#'Función para mandar los correos de reportes
#'@param direccion Es la dirección del destinatario
#'@param asunto Es el asunto del correo
#'@cuerpo Mensaje que se desea transmitir
#'@ruta Es el archivo que se desea adjuntar en el correo electrónico

mandarCorreo <- function(direccion, asunto, cuerpo, ruta){
   sender <- "reportesine@gmail.com"
   #, "<rdnarcisoc@gmail.com>",
  recipients <- direccion
  mailR::send.mail(from = sender,
            to = recipients,
            subject=asunto,
            body = cuerpo,
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name="reportesine@gmail.com", passwd="Ine$2020", ssl=TRUE),
            authenticate = TRUE,
            send = TRUE,
            attach.files = ruta)
}


#'Funcion de compresión de datos para mapas
#'
#'
enviarMapas <- function(direccion, asunto, cuerpo, ruta){
  sender <- "reportesine@gmail.com"
  #, "<rdnarcisoc@gmail.com>",
  if (.Platform$OS.type == "windows") {
    ## No hay impelentacion  
  }else{
    archivo <- substr(basename(ruta), 1, nchar(basename(ruta))-4)
    print(archivo)
    cadena <-  'tar -zcvf /home/ineservidor/Mapas/mapas.tar.gz /home/ineservidor/Mapas/*.pdf'
    print(cadena)
    suppressWarnings(silence <- system( cadena, intern=T, ignore.stderr=T))
  }
  recipients <- direccion
  mailR::send.mail(from = sender,
                   to = recipients,
                   subject=asunto,
                   body = cuerpo,
                   smtp = list(host.name = "smtp.gmail.com", port = 465, 
                               user.name="reportesine@gmail.com", passwd="Ine$2020", ssl=TRUE),
                   authenticate = TRUE,
                   send = TRUE,
                   attach.files = '/home/ineservidor/Mapas/mapas.tar.gz')
}