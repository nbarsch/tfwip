#' windisnord
#'
#' disconnect windows nord connection
#' @export
windisnord <- function(){
  oldwd <- getwd()
  setwd("C:/Program Files/NordVPN")
  myiporig <- RCurl::getURL("ifconfig.me")
  if(isTRUE(disconnect)){
    system("nordvpn -d")
    Sys.sleep(5)
  }
  myip <- RCurl::getURL("ifconfig.me")
  setwd(oldwd)  
  print(paste0("IP CHANGED FROM: ",myiporig," TO: ",myip))

}