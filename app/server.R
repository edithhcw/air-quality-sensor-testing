# server.R

library(maps)
library(mapproj)
library(leaflet)
library(data.table)
library(httr)
library(foreign)
zip <- read.csv("data/zipcode.csv")
#statefull <- read.csv("data/stateabb.csv")
sites <- read.csv('data/aqs_sites.csv')
aqicolor <- read.csv('data/legend.csv')
sites_small <- sites[,c("Latitude", "Longitude", 'City.Name', 'State.Name')]

sites_small <- na.omit(sites_small)
sites_small$readings <- sample(c(0:500), nrow(sites_small), replace = T)
#sites_ny <- sites_small[sites_small$State.Name == 'New York',]
top = 49.3457868 # north lat
left = -124.7844079 # west long
right = -66.9513812 # east long
bottom =  24.7433195 # south lat

w=c(zip$zip_code)
dt = data.table(w, val = w)
setattr(dt, "sorted", "w") 

getIndex <- function(zcode){
  index = dt[J(zcode), .I, roll = "nearest", by = .EACHI][[2]]
  return(index)
  }

getZipData <- function(zcode){
  url <- "https://airnowapi.org/aq/observation/zipCode/current/?"
  format <- "text/csv"
  zipCode <- zcode
  api_key <- "3DF88662-6375-4BCE-A7EC-68DED28CEE2A"
  req <- GET(url, query = list(format=format, zipCode = zipCode, api_key=api_key))
  result1 <- content(req, "text")
  writeBin(result1, "location.csv")
  reading <- read.csv("location.csv")
  reading <- na.omit(reading)
  aqi <- reading$AQI
  #print((length(aqi) == 0) && (typeof(aqi) == "logical"))
  if((length(aqi) == 0) && (typeof(aqi) == "logical")) {
    url <- "http://www.airnowapi.org/aq/forecast/zipCode/?"
    format <- "text/csv"
    zipCode <- zcode
    api_key <- "3DF88662-6375-4BCE-A7EC-68DED28CEE2A"
    distance <- 25
    req <- GET(url, query = list(format=format, zipCode = zipCode, api_key=api_key, distance = distance))
    result1 <- content(req, "text")
    writeBin(result1, "pred.csv")
    pred <- read.csv("pred.csv")
    pm25 <- pred[pred$ParameterName == 'PM2.5']
    print(pm25$AQI)
    if((length(aqi) == 0) && (typeof(aqi) == "logical")) {
      return(-1)
    }
    return(pm25$AQI)
  }
  #print(aqi)
  return(aqi)
  #print(head(reading))
}
getStateData <- function(state, lat, lng){
  url <- "https://airnowapi.org/aq/data/"
  parameters <- "pm25"
  #left <- lng - 5
  #right <- lng + 5
  #up <- lat + 5
  #down <- lat - 5
  bbox <- paste(toString(left), toString(bottom), toString(right), toString(top), sep = ',')
  #"-90.806632,24.634217,-71.119132,45.910790"
  #print(bbox)
  data_type <- "a"
  format <- "text/csv"
  ext <- "csv"
  api_key <- "3DF88662-6375-4BCE-A7EC-68DED28CEE2A"
  #REQUEST_URL <- paste(url, "?&parameters=" , parameters , "&bbox=" , bbox, "&datatype=", data_type, "&format=" , format, "&api_key=" ,api_key, sep="")
  req <- GET(url, query = list(parameters= parameters , bbox= bbox, datatype= data_type, format=format, api_key=api_key))
  #r <- GET(REQUEST_URL)
  result1 <- content(req, "text")
  result1 <- paste("latitude, longtitude, data, type, value, color", result1, sep = '\n')
  writeBin(result1, "state.csv")
}
shinyServer(
  function(input, output) {

    pal <- colorFactor(c("chartreuse", "yellow", "orange", "red", "Purple", "Maroon"), domain = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"))
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)
      
    })
    observe({
    leafletProxy("map", data = aqicolor) %>% addLegend(position = "bottomright",
                                                   title = " ", pal = pal,values = ~color)
    })

    pal <- colorFactor(c("chartreuse", "yellow", "orange", "red", "Purple", "Maroon"), domain = c(1, 2, 3, 4, 5, 6))
    #getStateData(0, 0, 0)
    site <- read.csv('state.csv');
    site <- na.omit(site)
    site <- site[site$value > 0,]
    leafletProxy("map", data = site) %>% addTiles() %>%
      addCircles(
        lng = ~longtitude, lat = ~latitude, weight = 4,
        radius = 10000, popup = ~paste("The value is", as.character(value), sep = ' '), color = ~pal(color), fillOpacity = 0.6
      )
    observe({
      zcode <- input$num
      if(!is.na(zcode)){
        if(zcode > 500){
            index <- getIndex(zcode)
            lat <- zip$latitude[index]
            lng <- zip$longitude[index]
            stateabb <- zip[index,]$state
            #aqi <- getZipData(zcode)
            aqi <- -1
            print(paste("the aqi is", aqi, sep = ' '))
            
            popup <- "not available"
            if(0 > aqi) {
              popup <- "not available"
            } else if (aqi <= 50) {
              popup <- "Good"
            } else if(aqi <= 100) {
              popup <- "Moderate"
            } else if(aqi <= 150) {
              popup <- "Unhealthy for Sensitive Groups"
            } else if(aqi <= 200) {
              popup <- "Unhealthy"
            } else if(aqi <= 300) {
              popup <- "Very Unhealthy"
            } else if(aqi <= 500) {
              popup <- "Hazardous"
            }
            
            leafletProxy("map") %>% setView(lng, lat, zoom = 7)
            leafletProxy("map") %>% clearPopups
            leafletProxy("map") %>% addPopups(lng, lat, paste("The air quality in your area is" , popup, sep = ' '))
            
        } else {
          leafletProxy("map") %>% setView(lng = -93.85, lat = 37.45, zoom = 4)
        }
      } else {
        leafletProxy("map") %>% setView(lng = -93.85, lat = 37.45, zoom = 4)
      }
    })
    
  }
)

