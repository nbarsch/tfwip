#' renordwin()
#'
#' redo nordvpn connection
#' @param nordloc nord location to connect to, default="randomUS"
#' @param disconnect set TRUE to disconnect from nord before reconnecting
#' @param waitsecmin optional min number of seconds to wait after connecting
#' @param waitsecmax optional max number of seconds to wait after connecting
#' @export
renordwin <- function(waitsecmin=8,waitsecmax=10,nordloc="randomUS", disconnect=FALSE){
  oldwd <- getwd()
  setwd("C:/Program Files/NordVPN")
  myip1<- tryCatch(RCurl::getURL("ifconfig.me"),error=function(e){return("err")})
  myiporig <- myip1
  if(isTRUE(disconnect)){
    system("nordvpn -d")
    Sys.sleep(5)
  }
  if(nordloc=="randomUS"){
    require(rvest)
    require(httr)
    require(jsonlite)
    gr <- GET(url="https://api.nordvpn.com/server")
    gt <- content(gr,"text")
    gf <- fromJSON(gt, flatten = TRUE)
    gf <- gf[substr(gf$name,1,15)=="United States #",]
    
    #gf <- gf[gf$country=="United States",]
    sampnord <- sample(gf$name,1)
  }else{
    sampnord <- nordoc
  }
  system(paste0('nordvpn -c -n "',sampnord,'"'))
  randsleep <- sample(seq(waitsecmin, waitsecmax, by = 0.001), 1)
  Sys.sleep(randsleep)
  myip<- tryCatch(RCurl::getURL("ifconfig.me"),error=function(e){return("err")})
  if(myip==myip1){
    if(isTRUE(disconnect)){
      system("nordvpn -d")
      Sys.sleep(1)
    }
    myip1<- tryCatch(RCurl::getURL("ifconfig.me"),error=function(e){return("err")})
    if(nordloc=="randomUS"){
      require(rvest)
      require(httr)
      require(jsonlite)
      gr <- GET(url="https://api.nordvpn.com/server")
      gt <- content(gr,"text")
      gf <- fromJSON(gt, flatten = TRUE)
      gf <- gf[substr(gf$name,1,15)=="United States #",]
      
      #gf <- gf[gf$country=="United States",]
      sampnord <- sample(gf$name,1)
    }else{
      sampnord <- nord
    }
    system(paste0('nordvpn -c -n "',sampnord,'"'))
    randsleep <- sample(seq(waitsecmin, waitsecmax, by = 0.001), 1)
    Sys.sleep(randsleep)
    myip<- tryCatch(RCurl::getURL("ifconfig.me"),error=function(e){return("err")})
  }
  if(myip==myip1){
    if(isTRUE(disconnect)){
      system("nordvpn -d")
      Sys.sleep(1)
    }
    myip1<- tryCatch(RCurl::getURL("ifconfig.me"),error=function(e){return("err")})
    if(nordloc=="randomUS"){
      #gf <- gf[gf$country=="United States",]
      sampnord <- sample(gf$name,1)
    }else{
      sampnord <- nord
    }
    system(paste0('nordvpn -c -n "',sampnord,'"'))
    randsleep <- sample(seq(waitsecmin, waitsecmax, by = 0.001), 1)
    Sys.sleep(randsleep)
    myip<- tryCatch(RCurl::getURL("ifconfig.me"),error=function(e){return("err")})
  }
  print(paste0("IP CHANGED FROM: ",myiporig," TO: ",myip))
  setwd(oldwd)
}