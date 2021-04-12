#' getua()
#'
#' get user agent based on specs or random 
#' @param brow browser type, one of firefox, safari, or chrome
#' @param browver string of browser version to use
#' @param uasys string of system type to use, one of windows, mac/macintosh, linux
#' @param forcenew true or false force a new update of user agents to be downloaded and used
#' @param mainbrow use only firefox, chrome, or safari uagents
#' @export
getua <- function(brow="random",browver="random",uasys="random",forcenew=FALSE, mainbrow=TRUE){
  
  require(lubridate)
  require(foreach)
  require(data.table)
  require(stringr)
  require(jsonlite)
  
  tyear <- year(now())
  tmonth <- month(now())
  tym <- paste0(tyear,tmonth)
  
  if(isTRUE(forcenew)){
    if(file.exists(paste0("uagents_",tym,".csv"))){file.remove(paste0("uagents_",tym,".csv"))}
    if(file.exists(paste0("uagents_",tym,".json"))){file.remove(paste0("uagents_",tym,".json"))}
  }
  
  if(!file.exists(paste0("uagents_",tym,".csv"))){
    if(!file.exists(paste0("uagents_",tym,".json"))){
      
      download.file(url="https://raw.githubusercontent.com/Kikobeats/top-user-agents/master/index.json",destfile=paste0("uagents_",tym,".json"))
      
    }
    tjson <- read_json(paste0("uagents_",tym,".json"))
    tua <- unlist(tjson)
    tdua <- data.frame(useragent=tua,stringsAsFactors = F)
    re <- "\\(([^()]+)\\)"
    
    
    
    
    foreach(ij=1:length(tjson))%do%{
      tuagent <- tdua$useragent[ij]
      origtuagent <- tuagent
      if(all(grepl("Chrome",word(tuagent,-2,-2)),grepl("Safari",word(tuagent,-1,-1)))){
        tuagent <- word(tuagent,1,-2)
      }
      
      tsys <- gsub(re, "\\1", str_extract_all(tuagent, re)[[1]])
      tsys <- unlist(strsplit(tsys,";"))[1]
      tsysgen <- "NOT_FOUND"
      if(grepl("windows",tolower(tsys))){tsysgen <- "windows"}
      if(grepl("macintosh",tolower(tsys))){tsysgen <- "mac"}
      if(grepl("x11",tolower(tsys))){tsysgen <- "linux"}
      
      tbrow <- trimws(gsub("[[:punct:]]","",gsub("[[:digit:]]","",unlist(strsplit(tuagent,"/"))[length(unlist(strsplit(tuagent,"/")))-1])))
      if(grepl("Chrome",tbrow)){tbrow <- "Chrome"}
      tbrowver <- unlist(strsplit(tuagent,"/"))[length(unlist(strsplit(tuagent,"/")))]
      
      tdata <- data.frame(browser=tbrow, browser_version=tbrowver, uasystem=tsys, uasysgen=tsysgen, uatxt=origtuagent,stringsAsFactors = F)
      fwrite(tdata,paste0("uagents_",tym,".csv"),append=T)
    }
    
    
  }
    
  tuagents <- fread(paste0("uagents_",tym,".csv"))
  
  if(isTRUE(mainbrow)){
    tuagents <- tuagents[tuagents$browser%in%c("Firefox","Safari","Chrome"),]
    tuagents <- tuagents[!grepl("YaBrowser",tuagents$uatxt),]
  }
  
  if(brow!="random"){
    tuagents <- tuagents[tolower(tuagents$browser)==brow,]
  }
  if(browver!="random"){
    tuagents <- tuagents[grep(browver,tuagents$browser_version),]
  }
  if(uasys!="random"){
    if(uasys=="win"){uasys <- "windows"}
    if(uasys=="lin"){uasys <- "linux"}
    if(uasys=="macintosh"){uasys <- "mac"}
    tuagents <- tuagents[tuagents$uasysgen==tolower(uasys),]
  }
  
  return(tuagents$uatxt[sample(c(1:nrow(tuagents)),1)])
}
