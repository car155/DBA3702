library("ggmap")
library("ggplot2")
library("jsonlite")
library("leaflet")
library("leaflet.extras")
library("shiny")
library("shinyTime")

getTaxiAvailabilityData = function(date, time) {
    datestr = paste0(date,"T")
    querystr = paste0(datestr, time)
    
    url <- paste0("https://api.data.gov.sg/v1/transport/taxi-availability?date_time=", 
                  querystr)
    data <- fromJSON(url)
    
    taxi.availability <- as.data.frame(data$features$geometry$coordinates)
    return(taxi.availability)
}

# Using leaflet

plotMapView = function(date, time, option) {
  taxi_data = getTaxiAvailabilityData(date, time)
  m = leaflet()
  if (option == "Point") {
    m = m %>% addTiles() %>% addCircleMarkers(data=taxi_data, lng=~X1, lat=~X2, 
                                              radius=4, fillOpacity=0.7, fillColor="red", stroke=F)
  } else {
    m = m %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      addWebGLHeatmap(data=taxi_data, lng=~X1, lat=~X2, size=1200)
  }
  return(m)
}

ui = pageWithSidebar(
  headerPanel("Singapore Taxi Availability"), 
  sidebarPanel(dateInput(inputId="date", 
                         label="Date", 
                         value=Sys.Date()),
               timeInput(inputId="time",
                         label="Time",
                         value=Sys.time()),
               radioButtons(inputId="map",
                          label="Plot Type",
                          choices=c("Point", "Heatmap"))), 
  mainPanel(leafletOutput("taxiPlot")) 
)

server = function(input, output) {
  output$taxiPlot = renderLeaflet({
    plotMapView(input$date, strftime(input$time, format="%H:%M:%S"), input$map)
  })
}

# Using ggmap and reactive



ui_ggmap = pageWithSidebar(
  headerPanel("Singapore Taxi Availability"), 
  sidebarPanel(dateInput(inputId="date", 
                         label="Date", 
                         value=Sys.Date()),
               timeInput(inputId="time",
                         label="Time",
                         value=Sys.time()),
               radioButtons(inputId="map",
                            label="Plot Type",
                            choices=c("Point", "Heatmap"))), 
  mainPanel(plotOutput("taxiPlot")))

server_ggmap = function(input, output) {
  
  # lazy base map
  getBaseMap = reactive({
    m = get_map("Singapore", zoom=11, source="google")
    # print("map gotten!")
    return(m)
  })
  
  # lazy get function
  getTaxiAvailabilityDataReactive = reactive({
    datestr = paste0(input$date,"T")
    querystr = paste0(datestr, strftime(input$time, format="%H:%M:%S"))
    
    url <- paste0("https://api.data.gov.sg/v1/transport/taxi-availability?date_time=", 
                  querystr)
    data <- fromJSON(url)
    
    taxi.availability <- as.data.frame(data$features$geometry$coordinates)
    # print("data gotten!")
    return(taxi.availability)
  })
  
  # plot function
  plotMapView_ggmap = function(viewOption) {
    df = getTaxiAvailabilityDataReactive()
    map = getBaseMap()
    
    if (viewOption == "Point") {
      view.map <- ggmap(map, base_layer = ggplot(data=df, aes(x=X1, y=X2))) + geom_point()
    } else {
      view.map <- ggmap(map) + stat_density2d(data=df, 
                                              aes(x=X1, y=X2,  fill=..level.., alpha=..level..), 
                                              size=2, geom='polygon') +
        scale_fill_gradient(low="blue", high="pink", guide=FALSE) + 
        scale_alpha(range=c(0, 1), guide=FALSE)
    }
    return(view.map)
  }
  
  output$taxiPlot = renderPlot({
    plotMapView_ggmap(input$map)
  })
}

shinyApp(ui=ui_ggmap, server=server_ggmap)