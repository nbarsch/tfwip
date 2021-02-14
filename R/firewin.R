#' firewin()
#'
#' launch windows firefox
#' @param firecaps firefox capabilities default=default, other options dtrackprotect dtrackprotectnofinger dtrackprotectdonot dtrackprotectdonotnofinger
#' @param noimage block images? default=TRUE
#' @param killjava kill java processes prior to launching, default=TRUE
#' @param killfire kill firefox processes prior to launching, default=TRUE
#' @param killports kill port set to use prior to connecting, default=FALSE
#' @param port port to use while connecting 
#' @param waitsecmin optional min number of seconds to wait after connecting
#' @param waitsecmax optional max number of seconds to wait after connecting
#' @export
firewin <- function(firecaps="dtrackprotectdonotnofinger",noimage=TRUE,killjava=TRUE,killfire=TRUE,killports=FALSE,port=4567,waitsecmin=2,waitsecmax=3){
  require(RSelenium)
  #gc()
  #system("killall firefox")
  if(isTRUE(killports)){
    system(paste0("kill -9 $(lsof -t -i:",as.integer(port)," -sTCP:LISTEN)"))
  }
  sinfo <- Sys.info()
  if(sinfo[["sysname"]]=="Linux"){
    if(isTRUE(killjava)){
      system("killall java",intern=F)
    }
    if(isTRUE(killfire)){
      system("killall firefox",intern=F)
    }
  }else{
    if(isTRUE(killjava)){
      system("taskkill /im java.exe /f",intern=F)
    }
    if(isTRUE(killfire)){
      system("taskkill /im firefox.exe /f",intern=F)
    }
  }

  
  if(isTRUE(noimage)){
    if(firecaps=="default"){
      eCaps = list(`moz:firefoxOptions` = list(
        prefs =list(
          "browser.cache.disk.enable" = FALSE,
          "browser.cache.memory.enable" = FALSE,
          "browser.cache.offline.enable" = FALSE,
          "browser.sessionstore.max_tabs_undo" = 0,
          "network.http.use-cache" = FALSE,
          #"network.cookie.cookieBehavior" = 2,
          #"network.cookie.lifetimePolicy" = 2,
          "webgl.disabled" = TRUE,
          #"geo.enabled" = FALSE,
          #"privacy.resistFingerprinting" = TRUE,
          'permissions.default.image'= 2,
          "privacy.clearOnShutdown.cache" = TRUE,
          "privacy.clearOnShutdown.cookies" = TRUE)
        #"privacy.trackingprotection.enabled"=TRUE,
        #"privacy.donottrackheader.enabled"=TRUE)
      )
      )
    }
    if(firecaps=="dtrackprotect"){
      eCaps = list(`moz:firefoxOptions` = list(
        prefs =list(
          "browser.cache.disk.enable" = FALSE,
          "browser.cache.memory.enable" = FALSE,
          "browser.cache.offline.enable" = FALSE,
          "browser.sessionstore.max_tabs_undo" = 0,
          "network.http.use-cache" = FALSE,
          #"network.cookie.cookieBehavior" = 2,
          #"network.cookie.lifetimePolicy" = 2,
          "webgl.disabled" = TRUE,
          #"geo.enabled" = FALSE,
          #"privacy.resistFingerprinting" = TRUE,
          'permissions.default.image'= 2,
          "privacy.clearOnShutdown.cache" = TRUE,
          "privacy.clearOnShutdown.cookies" = TRUE,
          "privacy.trackingprotection.enabled"=TRUE)
        #"privacy.donottrackheader.enabled"=TRUE)
      )
      )
    }
    if(firecaps=="dtrackprotectnofinger"){
      eCaps = list(`moz:firefoxOptions` = list(
        prefs =list(
          "browser.cache.disk.enable" = FALSE,
          "browser.cache.memory.enable" = FALSE,
          "browser.cache.offline.enable" = FALSE,
          "browser.sessionstore.max_tabs_undo" = 0,
          "network.http.use-cache" = FALSE,
          #"network.cookie.cookieBehavior" = 2,
          #"network.cookie.lifetimePolicy" = 2,
          "webgl.disabled" = TRUE,
          #"geo.enabled" = FALSE,
          "privacy.resistFingerprinting" = TRUE,
          'permissions.default.image'= 2,
          "privacy.clearOnShutdown.cache" = TRUE,
          "privacy.clearOnShutdown.cookies" = TRUE,
          "privacy.trackingprotection.enabled"=TRUE)
        #"privacy.donottrackheader.enabled"=TRUE)
      )
      )
    }
    if(firecaps=="dtrackprotectdonot"){
      eCaps = list(`moz:firefoxOptions` = list(
        prefs =list(
          "browser.cache.disk.enable" = FALSE,
          "browser.cache.memory.enable" = FALSE,
          "browser.cache.offline.enable" = FALSE,
          "browser.sessionstore.max_tabs_undo" = 0,
          "network.http.use-cache" = FALSE,
          #"network.cookie.cookieBehavior" = 2,
          #"network.cookie.lifetimePolicy" = 2,
          "webgl.disabled" = TRUE,
          #"geo.enabled" = FALSE,
          #"privacy.resistFingerprinting" = TRUE,
          'permissions.default.image'= 2,
          "privacy.clearOnShutdown.cache" = TRUE,
          "privacy.clearOnShutdown.cookies" = TRUE,
          "privacy.trackingprotection.enabled"=TRUE,
          "privacy.donottrackheader.enabled"=TRUE)
      )
      )
    }
    if(firecaps=="dtrackprotectdonotnofinger"){
      eCaps = list(`moz:firefoxOptions` = list(
        prefs =list(
          "browser.cache.disk.enable" = FALSE,
          "browser.cache.memory.enable" = FALSE,
          "browser.cache.offline.enable" = FALSE,
          "browser.sessionstore.max_tabs_undo" = 0,
          "network.http.use-cache" = FALSE,
          #"network.cookie.cookieBehavior" = 2,
          #"network.cookie.lifetimePolicy" = 2,
          "webgl.disabled" = TRUE,
          #"geo.enabled" = FALSE,
          "privacy.resistFingerprinting" = TRUE,
          'permissions.default.image'= 2,
          "privacy.clearOnShutdown.cache" = TRUE,
          "privacy.clearOnShutdown.cookies" = TRUE,
          "privacy.trackingprotection.enabled"=TRUE,
          "privacy.donottrackheader.enabled"=TRUE)
      )
      )
    }
  }else{ #else noimage TRUE
    if(firecaps=="default"){
      eCaps = list(`moz:firefoxOptions` = list(
        prefs =list(
          "browser.cache.disk.enable" = FALSE,
          "browser.cache.memory.enable" = FALSE,
          "browser.cache.offline.enable" = FALSE,
          "browser.sessionstore.max_tabs_undo" = 0,
          "network.http.use-cache" = FALSE,
          #"network.cookie.cookieBehavior" = 2,
          #"network.cookie.lifetimePolicy" = 2,
          "webgl.disabled" = TRUE,
          #"geo.enabled" = FALSE,
          #"privacy.resistFingerprinting" = TRUE,
          #'permissions.default.image'= 2,
          "privacy.clearOnShutdown.cache" = TRUE,
          "privacy.clearOnShutdown.cookies" = TRUE)
        #"privacy.trackingprotection.enabled"=TRUE,
        #"privacy.donottrackheader.enabled"=TRUE)
      )
      )
    }
    if(firecaps=="dtrackprotect"){
      eCaps = list(`moz:firefoxOptions` = list(
        prefs =list(
          "browser.cache.disk.enable" = FALSE,
          "browser.cache.memory.enable" = FALSE,
          "browser.cache.offline.enable" = FALSE,
          "browser.sessionstore.max_tabs_undo" = 0,
          "network.http.use-cache" = FALSE,
          #"network.cookie.cookieBehavior" = 2,
          #"network.cookie.lifetimePolicy" = 2,
          "webgl.disabled" = TRUE,
          #"geo.enabled" = FALSE,
          #"privacy.resistFingerprinting" = TRUE,
          #'permissions.default.image'= 2,
          "privacy.clearOnShutdown.cache" = TRUE,
          "privacy.clearOnShutdown.cookies" = TRUE,
          "privacy.trackingprotection.enabled"=TRUE)
        #"privacy.donottrackheader.enabled"=TRUE)
      )
      )
    }
    if(firecaps=="dtrackprotectnofinger"){
      eCaps = list(`moz:firefoxOptions` = list(
        prefs =list(
          "browser.cache.disk.enable" = FALSE,
          "browser.cache.memory.enable" = FALSE,
          "browser.cache.offline.enable" = FALSE,
          "browser.sessionstore.max_tabs_undo" = 0,
          "network.http.use-cache" = FALSE,
          #"network.cookie.cookieBehavior" = 2,
          #"network.cookie.lifetimePolicy" = 2,
          "webgl.disabled" = TRUE,
          #"geo.enabled" = FALSE,
          "privacy.resistFingerprinting" = TRUE,
          #'permissions.default.image'= 2,
          "privacy.clearOnShutdown.cache" = TRUE,
          "privacy.clearOnShutdown.cookies" = TRUE,
          "privacy.trackingprotection.enabled"=TRUE)
        #"privacy.donottrackheader.enabled"=TRUE)
      )
      )
    }
    if(firecaps=="dtrackprotectdonot"){
      eCaps = list(`moz:firefoxOptions` = list(
        prefs =list(
          "browser.cache.disk.enable" = FALSE,
          "browser.cache.memory.enable" = FALSE,
          "browser.cache.offline.enable" = FALSE,
          "browser.sessionstore.max_tabs_undo" = 0,
          "network.http.use-cache" = FALSE,
          #"network.cookie.cookieBehavior" = 2,
          #"network.cookie.lifetimePolicy" = 2,
          "webgl.disabled" = TRUE,
          #"geo.enabled" = FALSE,
          #"privacy.resistFingerprinting" = TRUE,
          #'permissions.default.image'= 2,
          "privacy.clearOnShutdown.cache" = TRUE,
          "privacy.clearOnShutdown.cookies" = TRUE,
          "privacy.trackingprotection.enabled"=TRUE,
          "privacy.donottrackheader.enabled"=TRUE)
      )
      )
    }
    if(firecaps=="dtrackprotectdonotnofinger"){
      eCaps = list(`moz:firefoxOptions` = list(
        prefs =list(
          "browser.cache.disk.enable" = FALSE,
          "browser.cache.memory.enable" = FALSE,
          "browser.cache.offline.enable" = FALSE,
          "browser.sessionstore.max_tabs_undo" = 0,
          "network.http.use-cache" = FALSE,
          #"network.cookie.cookieBehavior" = 2,
          #"network.cookie.lifetimePolicy" = 2,
          "webgl.disabled" = TRUE,
          #"geo.enabled" = FALSE,
          "privacy.resistFingerprinting" = TRUE,
          #'permissions.default.image'= 2,
          "privacy.clearOnShutdown.cache" = TRUE,
          "privacy.clearOnShutdown.cookies" = TRUE,
          "privacy.trackingprotection.enabled"=TRUE,
          "privacy.donottrackheader.enabled"=TRUE)
      )
      )
    }
  }
 
  rd <<- rsDriver( browser = "firefox", 
                  extraCapabilities = eCaps, port=as.integer(port))
  randsleep <- sample(seq(waitsecmin, waitsecmax, by = 0.001), 1)
  Sys.sleep(randsleep)
  remDr <<- rd$client
}
