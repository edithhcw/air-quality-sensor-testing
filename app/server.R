# server.R

library(maps)
library(mapproj)
library(leaflet)
library(data.table)
counties <- readRDS("data/counties.rds")
zip <- read.csv("data/zipcode.csv")
sites <- read.csv('data/aqs_sites.csv')
source("helpers.R")
top = 49.3457868 # north lat
left = -124.7844079 # west long
right = -66.9513812 # east long
bottom =  24.7433195 # south lat
x=94702
w=c(zip$zip_code)
#dt = data.table(zip$zip_code, val = zip$zip_code)
#setattr(dt, "sorted", "zip$zip_code")
dt = data.table(w, val = w)
setattr(dt, "sorted", "w") 
#print(dt[J(x), .I, roll = "nearest", by = .EACHI])


cities <- read.csv(textConnection("City,Lat,Long,Pop
                                  Boston,42.3601,-71.0589,645966
                                  Hartford,41.7627,-72.6743,125017
                                  New York City,40.7127,-74.0059,8406000
                                  Philadelphia,39.9500,-75.1667,1553000
                                  Pittsburgh,40.4397,-79.9764,305841
                                  Providence,41.8236,-71.4222,177994
                                  "))


getIndex <- function(zcode){
  index = dt[J(zcode), .I, roll = "nearest", by = .EACHI][[2]]
  return(index)
  }
shinyServer(
  function(input, output) {

    
    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    colorpal <- reactive({
      colorNumeric(input$colors, quakes$mag)
    })
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)
      leaflet(cities) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 30, popup = ~City
        )
    })
    
    
    observe({
      zcode <- input$num
      if(!is.na(zcode)){
        if(zcode > 500){
            index <- getIndex(zcode)
            lat <- zip$latitude[index]
            lng <- zip$longitude[index]
            leafletProxy("map") %>% setView(lng, lat, zoom = 18)
            leafletProxy("map") %>% clearPopups
            leafletProxy("map") %>% addPopups(lng, lat, "Here I am")
        } else {
          leafletProxy("map") %>% setView(lng = -93.85, lat = 37.45, zoom = 4)
        }
      } else {
        leafletProxy("map") %>% setView(lng = -93.85, lat = 37.45, zoom = 4)
      }
    })
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.

    
    # Use a separate observer to recreate the legend as needed.
    observe({
      proxy <- leafletProxy("map", data = quakes)
      
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      if (input$legend) {
        pal <- colorpal()
        proxy %>% addLegend(position = "bottomright",
                            pal = pal, values = ~mag
        )
      }
    })
  }
)

