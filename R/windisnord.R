#' windisnord
#'
#' disconnect windows nord connection
#' @export
windisnord <- function(){
  require(RCurl)
  oldwd <- getwd()
  setwd("C:/Program Files/NordVPN")
  myiporig <- tryCatch(RCurl::getURL("ifconfig.me"),error=function(e){return("err")})
  system("nordvpn -d")
  Sys.sleep(5)
  myip<- tryCatch(RCurl::getURL("ifconfig.me"),error=function(e){return("err")})
  setwd(oldwd)  
  print(paste0("IP CHANGED FROM: ",myiporig," TO: ",myip))

}