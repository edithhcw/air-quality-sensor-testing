# ui.R
library(leaflet)
library(RColorBrewer)
shinyUI(
  bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  numericInput("num", label = h3("Zip Code"), value = 10000)
    )
  )
)
