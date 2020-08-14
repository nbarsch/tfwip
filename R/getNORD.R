#' getNORD
#'
#' Get Nord server for specified location
#' @param state Name of state as two char capital eg "FL" or first character capital state name ie "Florida"
#' @export
getNORD <- function(state){
  library(rvest)
  library(httr)
  library(jsonlite)
  gr <- GET(url="https://api.nordvpn.com/server")
  gt <- content(gr,"text")
  gf <- fromJSON(gt, flatten = TRUE)
  gf <- gf[gf$country=="United States",]
  library(geojsonio)
  library(sp)

  # get usa polygon data
  # http://eric.clst.org/tech/usgeojson/
  usa <- geojson_read(
    "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json",
    what = "sp"
  )

  gf$state <- NA

  # compare points
  for (i in 1:nrow(gf)) {
    coords <- c(gf$location.long[i], gf$location.lat[i])
    if(any(is.na(coords))) next
    point <- sp::SpatialPoints(
      matrix(
        coords,
        nrow = 1
      )
    )
    sp::proj4string(point) <- sp::proj4string(usa)
    polygon_check <- sp::over(point, usa)
    gf$state[i] <- as.character(polygon_check$NAME)
  }
  gf %>% mutate_all(as.character)->gf
  gf$state_abb <- sapply(seq_along(gf$state),FUN=function(x){state.abb[grep(gf$state[x], state.name)]})
  gf$state_abb[gf$state=="Virginia"] <- "VA"
  gf$state_abb[gf$state=="West Virginia"] <- "WV"

  if(nchar(state)==2){
    tgf <- gf[gf$state_abb==state,]
  }else{
    tgf <- gf[gf$state==state,]
  }
  if(nrow(tgf)>0){
    tsamp <- tgf[sample(nrow(tgf), 1), ]
  }else{tsamp <- gf[sample(nrow(gf), 1), ]}
  nord_domain <- as.character(tsamp$domain[1])
  nord_server <- gsub("nordvpn.com","",nord_domain)
  nord_server <- gsub('[[:punct:]]',"",nord_server)
  print(tsamp$state[1])
  return(nord_server)
}





