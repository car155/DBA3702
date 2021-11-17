library("shiny")
library("leaflet")

# Define UI
ui = fluidPage(sliderInput(inputId="zoomlevel", 
                           label="Map Zooming Level",
                           value=11,
                           min=1, 
                           max=20),
               leafletOutput(outputId="plotMap"))

# Define server logic
server = function(input, output){
  output$plotMap = renderLeaflet({
    leaflet() %>% setView(lng=103.835381, lat=1.339660,
                          zoom=input$zoomlevel) %>%
                            addTiles() %>%
                            addMarkers(lat=1.239660, lng=103.835381,
                                       popup="Sentosa Cove")
  })
}
# Instantiate app
shinyApp(ui=ui, server=server)