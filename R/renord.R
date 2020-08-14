#' renord()
#'
#' redo nordvpn connection
#' @param nordloc nord location to connect to, default="random"
#' @param waitsec optional number of seconds to wait after connecting
#' @export
renord <- function(nordloc="randomcity",waitsec=4){
  if(tolower(nordloc)=="randomus"){
    library(rvest)
    library(httr)
    library(jsonlite)
    gr <- GET(url="https://api.nordvpn.com/server")
    gt <- content(gr,"text")
    gf <- fromJSON(gt, flatten = TRUE)
    gf <- gf[gf$country=="United States",]
    sampnord <- sample(gf$domain,1)
    sampnord <- gsub("nordvpn.com","",sampnord)
    sampnord <- gsub("[[:punct:]]","",sampnord)
  }
  library(geojsonio)
  library(sp)
  if(tolower(nordloc)=="randomcity"){
    nordcities <- c("Atlanta", "Chicago", "Los_Angeles",	"New_York",	"Salt_Lake_City",
                    "Buffalo", "Dallas", "Manassas",	"Phoenix", "San_Francisco",
                    "Charlotte",	"Denver", "Miami", "Saint_Louis",	"Seattle")
    sampnord <- sample(nordcities,1)
  }else{
    sampnord <- nordloc
  }
  system("nordvpn disconnect")
  Sys.sleep(3)
  system(paste0("nordvpn connect ",sampnord))
  if(is.numeric(waitsec)){
    Sys.sleep(waitsec)
  }
  nordstat <- system("nordvpn status",intern=T)
  while(grepl("Disconnected",nordstat)){
    if(tolower(nordloc)=="randomus"){
      library(rvest)
      library(httr)
      library(jsonlite)
      gr <- GET(url="https://api.nordvpn.com/server")
      gt <- content(gr,"text")
      gf <- fromJSON(gt, flatten = TRUE)
      gf <- gf[gf$country=="United States",]
      sampnord <- sample(gf$domain,1)
      sampnord <- gsub("nordvpn.com","",sampnord)
      sampnord <- gsub("[[:punct:]]","",sampnord)
    }
    library(geojsonio)
    library(sp)
    if(tolower(nordloc)=="randomcity"){
      nordcities <- c("Atlanta", "Chicago", "Los_Angeles",	"New_York",	"Salt_Lake_City",
                      "Buffalo", "Dallas", "Manassas",	"Phoenix", "San_Francisco",
                      "Charlotte",	"Denver", "Miami", "Saint_Louis",	"Seattle")
      sampnord <- sample(nordcities,1)
    }else{
      sampnord <- nordloc
    }
    system("nordvpn disconnect")
    Sys.sleep(3)
    system(paste0("nordvpn connect ",sampnord))
    if(is.numeric(waitsec)){
      Sys.sleep(waitsec)
    }
    nordstat <- system("nordvpn status",intern=T)
  }
}
