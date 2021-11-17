library(dplyr)
library(erer)
library(geosphere)
library(ggmap)
library(ggplot2)
library(htmltools)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(maptools)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(sf)
library(shiny)
library(shiny.router)
library(shinyTime)
library(tidyverse)

# Helper function Parse API Date
parse_api_date = function(api, input_date = "", summary) {
  # No date specified
  if (input_date == "") {
    return(paste0("https://api.data.gov.sg/v1/",
                  api))
    
    # Checks if the date is in the date_time format
  } else if (stringr::str_detect(input_date, pattern = "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}T[:digit:]{2}:[:digit:]{2}:[:digit:]{2}$") &
             summary == FALSE) {
    input_date = gsub(":", "%3A", input_date)
    return(paste0("https://api.data.gov.sg/v1/",
                  api,
                  "?date_time=",
                  input_date))
    
    # Checks if the date is in the date format
  } else if (stringr::str_detect(input_date, pattern = "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$") &
             summary == TRUE) {
    input_date = gsub(":", "%3A", input_date)
    return(paste0("https://api.data.gov.sg/v1/",
                  api,
                  "?date=",
                  input_date))
    
  } else {
    # Returns an error that the date is not in the correct format
    stop("Check that the date / date_time parameter is in the right format.")
  }
}

# Helper function Parse API Output
parse_api_output = function(inputcontent) {
  
  if (!httr::http_error(inputcontent)) {
    return(httr::content(inputcontent))
    
  } else if (httr::status_code(inputcontent) >= 400 &
             httr::status_code(inputcontent) < 500) {
    stop("Client error has occured. Check your code.")
    
  } else if (httr::status_code(inputcontent) >= 500 &
             httr::status_code(inputcontent) < 600) {
    stop("Server Error has occured.")
    
  }
}

# Helper function to get carpark availability
carpark_availability = function(date_time = "") {
  
  url = parse_api_date(api = "transport/carpark-availability",
                       input_date = date_time,
                       summary = FALSE)
  if (curl::has_internet()) {
    output = httr::GET(url)
  } else {
    message("No internet connection found.")
    return(NULL)
  }
  
  content.output = parse_api_output(output)
  message("Closest timestamp: ", content.output$items[[1]]$timestamp)
  
  carpark_availability = lapply(1:length(content.output$items[[1]]$carpark_data), function(x){
    data.frame(id = content.output$items[[1]]$carpark_data[x][[1]]$carpark_number,
               type = content.output$items[[1]]$carpark_data[x][[1]]$carpark_info[[1]]$lot_type,
               last_update = content.output$items[[1]]$carpark_data[x][[1]]$update_datetime,
               total_lots = as.integer(content.output$items[[1]]$carpark_data[x][[1]]$carpark_info[[1]]$total_lots),
               availability_lots = as.integer(content.output$items[[1]]$carpark_data[x][[1]]$carpark_info[[1]]$lots_available),
               stringsAsFactors = FALSE)
  })
  carpark_availability = dplyr::bind_rows(carpark_availability)
  return(carpark_availability)
}

# Helper function to convert coord to long and lat
convertGeom <- function(cp){
  carparks_latlon = st_as_sf(cp, coords=c("x_coord", "y_coord"))
  carparks_latlon = st_set_crs(carparks_latlon, 3414) # read as svy21
  carparks_latlon = st_transform(carparks_latlon, crs=st_crs(4326)) # convert to wgs84
  # convert from svy21 to wgs84
  carparks_latlon = as.data.frame(carparks_latlon)
  carparks_latlon = carparks_latlon %>%
    mutate(lon = unlist(map(geometry,1)),
           lat = unlist(map(geometry,2)))
  # extract coordinates to columns
  carparks_latlon = subset(carparks_latlon, select=-c(geometry))
  return(carparks_latlon)
}

# Helper function to get the previous week's date
getlastdate <- function(day) {
  dates <- seq((Sys.Date()-21), (Sys.Date()-1), by="days")
  dates[wday(dates, label=T)==day]
}

# Helper function to get car park availability for a specified carpark id
getweekavail <- function(carpark_id) {
  # Get list of every 6 hours for past week
  end_date = Sys.time()
  start_date = as.Date(end_date - days(6))
  hour(start_date) = 3
  times = seq(start_date, end_date, by='hours')
  times = times[seq(1, length(times), by=6)]
  
  avail = NULL
  
  for (i in 1:length(times)){
    t = format(times[i], "%Y-%m-%dT%H:%M:%S")
    avail = carpark_availability(t) %>%
      filter(id == carpark_id) %>%
      mutate(time = times[i]) %>%
      rbind(avail)
  }
  
  return(avail)
}

# Helper function to get weather data
getWeatherData <- function(date,time){
  
  url = "https://api.data.gov.sg/v1/environment/2-hour-weather-forecast?date_time="
  url <- paste0(url,date,"T",time)
  data <- fromJSON(url)
  weather <- as.data.frame(data$items$forecasts)
  colnames(weather) <- c("area","forecast")
  weather$forecast <- unlist(strsplit(weather$forecast, " \\(.*\\)"))
  location <- as.data.frame(data$area_metadata$label_location)
  weather <- cbind(weather,location)
  weathertypes <- data.frame(forecast = c("Light Rain","Moderate Rain","Rain","Light Showers",
                                          "Moderate Showers","Showers","Thundery Showers","Fair",
                                          "Hazy","Partly Cloudy","Cloudy","Overcast"))
  data <- merge(weather,weathertypes,by.x="forecast",by.y="forecast")
  data <- as.data.frame(data)
  data$area <- toupper(data$area)
  return (data)
}

####################################################################

# Define pages
search_page = div(
  
  # Title ----
  titlePanel("Find the nearest carpark to any location in SG!"),
  
  br(),
  
  sidebarLayout(
    
    # Sidebar ----
    sidebarPanel(
      textInput("location", 
                "Location", 
                placeholder = "Please type the location name and end with Singapore"),
      dateInput("date", "Date", value = Sys.Date()),
      timeInput("time", "Time", value = Sys.time(), seconds = F),
      sliderInput("cutoff",
                  "Ignore carparks with fewer lots than",
                  value = 10,
                  min = 1,
                  max = 50),
      sliderInput("limited",
                  "Warn against carparks with fewer lots than",
                  value = 25,
                  min = 1,
                  max = 50),
      sliderInput("n",
                  "Number of recommendations to display",
                  value = 20,
                  min = 1,
                  max = 50),
      actionButton("go", "Find carpark!", class = "btn-primary"),
    ),
    
    # Main Panel ----
    mainPanel(tabsetPanel(type = "tabs",
                          tabPanel("Map", leafletOutput("map")),
                          tabPanel("Table", tableOutput("table"))
    )),
    
  ),
)

details_page = div(
  # Back Button ----
  br(),
  a(href = route_link("#!/"), 
    actionButton("back", "Back to Search Page", class = "btn-primary"),
    style="text-decoration:none"),
  br(),
  
  # Title ----
  titlePanel("Carpark Details: "),
  
  # Main Panel ----
  mainPanel(
    h3(textOutput("carpark_name")),
    br(),
    tableOutput("details_table"),
    br(),
    h3("Availability trend"),
    plotOutput("plot"),
  ),
)

####################################################################

# Define page servers
search_server = function(input, output, session){
  
  # Checker since tab render is lazy
  m <- leaflet() %>%
    addTiles(group="Default") %>%
    addProviderTiles("Esri.WorldImagery", group = "Esri") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>% 
    addMapPane("weather", zIndex = 400) %>% 
    addMapPane("markers", zIndex = 420) %>%
    addLayersControl(
      overlayGroups =c("Add Weather"),
      baseGroups = c("Default","Esri","CartoDB"),
      options = layersControlOptions(collapsed=FALSE)) %>% 
    setView(103.819,1.35,zoom = 11)
  
  # Generate base map
  output$map <- renderLeaflet({
    m
  })
  
  # On click of 'Find Location' button
  observeEvent(input$go, {
    if (format(input$time, "%H:%M") == "00:00") {
      time <- input$time + minutes(1)
    } else {
      time <- input$time
    }
    
    datetime = paste(input$date, strftime(time, "%T"))
    
    if (difftime(as.POSIXct(datetime), Sys.time(), units="hours") > 2) {
      showModal(modalDialog(title = "Sorry!", 
                            tags$p("We can only predict up to 2 hours ahead."),
                            tags$p("Try a different time!")))
    }
    else {
    # Renders a progress bar with 6 progress parts
      withProgress(
        message = 'Fetching data...',
        value = 1/5, {

        #last week's time
        if ((as.POSIXct(datetime) - Sys.time()) < 0 ){ #if less than zero means is historical time - just need 1 result
          t <- gsub(" ","T", datetime)
        } else {
          g <- weekdays(as.Date(datetime)) %>% 
            substr(1, 3) %>%
            getlastdate() %>%
            paste0("T")
          
          for (i in 1:3){
            g[i] = gsub(".* ", g[i], datetime)
          }
          t = g
        }
        
        incProgress(1/5)
        
        # Extract data from database
        tryCatch({
          a <- "https://data.gov.sg/api/action/datastore_search?resource_id=139a3035-e624-4f56-b63f-89ae28d4ae4c&limit=2500"
          cp <- fromJSON(a)
          cp <- cp$result$records
          hdbs <- convertGeom(cp)
        })
        
        incProgress(1/5)
        
        # Call the API first then combine the data together 
        tryCatch({
          cutoff = input$cutoff
          limited = input$limited
          
          if ((as.POSIXct(datetime) - Sys.time()) < 0) {
            cp_avail <- carpark_availability(t)
            df <- left_join(cp_avail,hdbs[,c("car_park_no","address","lon","lat","car_park_type","short_term_parking","free_parking","night_parking")],by=c("id"="car_park_no"))
            df <- subset(df,!is.na(df$address))
            df <- df[!df$availability_lots <= cutoff,]
            df$Status <- ifelse(df$availability_lots > limited,"Available","Limited lots")
            locate <- paste(input$location, "Singapore")
            loc <- geocode(locate)
            options(scipen=999)
            df$Distance <- signif(distHaversine(loc,df[,7:8]),4)/1000 
            #top 20 nearest
            df1 <- df[order(df$Distance), ][1:input$n, ]
          } else {
            #here is kinda hardcoded for 1,2,3 weeks data 
            cp_avail1 <- carpark_availability(t[1])
            cp_avail2 <- carpark_availability(t[2])
            cp_avail3 <- carpark_availability(t[3])
            
            cp_a <- merge(cp_avail1,cp_avail2,by="id")
            new <- merge(cp_a,cp_avail3,by="id") %>% select(id,total_lots,availability_lots.x,availability_lots.y,availability_lots)
            new$availability_lots = round(rowMeans(new[,c(3,4,5)]), digits=0)
            cp_avail <- new %>% select(id,total_lots,availability_lots)
            
            #from here just copy paste from the top
            df <- left_join(cp_avail,hdbs[,c("car_park_no","address","lon","lat","car_park_type","short_term_parking","free_parking","night_parking")],by=c("id"="car_park_no"))
            df <- subset(df,!is.na(df$address))
            df <- df[!df$availability_lots <= cutoff,]
            df$Status <- ifelse(df$availability_lots > limited,"Available","Limited lots")
            locate <- paste(input$location, "Singapore")
            loc <- geocode(locate)
            options(scipen=999)
            df$Distance <- signif(distHaversine(loc,df[,5:6]),4)/1000 
            
            #top 20 nearest
            df1 <- df[order(df$Distance), ][1:input$n, ]
            df1$availability_lots <- round(df1$availability_lots,1)
          }
        },
        error = function(e){
          loc = NULL
          df = NULL
        })
        
        incProgress(1/5)

        tryCatch({
          setting <- colorFactor(pal = c("green", "orange"), domain = c("Available", "Limited lots"))
          values <- c("Available","Limited lots")
          
          df1 <- mutate(df1, cntnt = paste0("<strong>Address: </strong>",address,
                                            "<br><strong>Carpark Type: </strong>",car_park_type,
                                            "<br><strong>Distance: </strong>",Distance,"km",
                                            "<br><strong>No. of lots available: </strong>",availability_lots,
                                            "<br><a href='#!/details?id=", id,"'>See Details</a>"))
          
          # Handle weather
          date <- as.character(input$date)
          time <- strftime(input$time, "%T")
          data <- getWeatherData(date,time)
          
          #read shapefile 
          sgmap <- readOGR(dsn = "./Data", 
                           layer = "Planning_Area_Census2010")
          
          sgmap@data$PLN_AREA_N <- as.character(sgmap@data$PLN_AREA_N)
          
          #merge shape file and weather data
          sgmap@data <- merge(sgmap@data,data,by.y='area',by.x='PLN_AREA_N',all.x=T)
          
          #filling up NA data with corresponding areas
          ifelse("YISHUN" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="SIMPANG",c(13:15)] <- data[data$area=="YISHUN",c(1,3,4)],NA)
          ifelse("PULAU UBIN" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="NORTH-EASTERN ISLANDS",c(13:15)] <- data[data$area=="PULAU UBIN",c(1,3,4)],NA)
          ifelse("CITY" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="DOWNTOWN CORE",c(13:15)] <- data[data$area=="CITY",c(1,3,4)],NA)
          ifelse("CITY" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="ORCHARD",c(13:15)] <- data[data$area=="CITY",c(1,3,4)],NA)
          
          ifelse("CITY" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="MARINA SOUTH",c(13:15)] <- data[data$area=="CITY",c(1,3,4)],NA)
          ifelse("CITY" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="OUTRAM",c(13:15)] <- data[data$area=="CITY",c(1,3,4)],NA)
          ifelse("CITY" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="MARINA EAST",c(13:15)] <- data[data$area=="CITY",c(1,3,4)],NA)
          ifelse("CITY" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="NEWTON",c(13:15)] <- data[data$area=="CITY",c(1,3,4)],NA)
          ifelse("CITY" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="MUSEUM",c(13:15)] <- data[data$area=="CITY",c(1,3,4)],NA)
          ifelse("CITY" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="ROCHOR",c(13:15)] <- data[data$area=="CITY",c(1,3,4)],NA)
          ifelse("CITY" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="SINGAPORE RIVER",c(13:15)] <- data[data$area=="CITY",c(1,3,4)],NA)
          ifelse("CITY" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="RIVER VALLEY",c(13:15)] <- data[data$area=="CITY",c(1,3,4)],NA)
          ifelse("CITY" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="STRAITS VIEW",c(13:15)] <- data[data$area=="CITY",c(1,3,4)],NA)
          ifelse("CHANGI" %in% data$area,sgmap@data[sgmap@data$PLN_AREA_N=="CHANGI BAY",c(13:15)] <- data[data$area=="CHANGI",c(1,3,4)],NA)
          
          #rearrange according to ascending OBJECTID order (else plot will be wrong)
          sgmap@data <- sgmap@data[order(sgmap@data$OBJECTID),]
          
          sgmapData <- spTransform(sgmap, CRS("+proj=longlat +datum=WGS84"))
          
          pal <- colorFactor(palette = c("#c7e6f0","#81c5db","#4fa7c4","#4395c4",
                                         "#217cb0","#769fdb","#3769b3","#dcf7d2",
                                         "#dcf7d2","#dcf7d2","#dcf7d2","#bdd1f0"), 
                             levels =  c("Light Rain","Moderate Rain","Rain",
                                         "Light Showers","Moderate Showers","Showers",
                                         "Thundery Showers","Fair","Hazy",
                                         "Partly Cloudy","Cloudy","Overcast"))
          
          m <- m %>% clearShapes() %>% 
            addPolygons(data=sgmapData, 
                        weight = 2, 
                        stroke = TRUE,
                        smoothFactor = 0.1, 
                        fillOpacity = 0.5, 
                        color = ~pal(forecast),
                        group = "Add Weather",
                        options = pathOptions(pane = "weather")) %>% 
            addLegend("bottomright", 
                      pal = pal, 
                      values = sgmapData$forecast, 
                      labels = c("rain","no rain","overcast"), 
                      title = "Weather Forecast",opacity = 1,
                      group = "Add Weather")
          
          m = m %>% 
            addMarkers(data = loc,~lon,~lat, options = pathOptions(pane = "markers"))%>%
            setView(loc$lon,loc$lat, zoom = 16) %>% 
            addCircleMarkers(data = df1,
                             color = ~setting(Status),
                             popup = ~as.character(cntnt),
                             stroke = FALSE, fillOpacity = 0.8,label = ~htmlEscape(address),
                             options = pathOptions(pane = "markers")) %>%
            addLegend("topleft",pal = setting, values = values ,title = "Carpark Availability Status")
          
          # Update map
          output$map <- renderLeaflet({
            m
          })
          
        },
        error = function(e) {
          showModal(modalDialog(title = "Sorry!", 
                                tags$p("We couldn't find any carparks for that location."),
                                tags$p("Give another one a try!")))
        })
        
        tryCatch({
          df1 <- df1 %>% mutate(Link = paste0("<a href='#!/details?id=", id,"'>See Details</a>"),
                                Distance = paste(Distance, "km"))
          df1 = df1 %>% subset(select=c(Link, id, address, Distance, Status, 
                                        total_lots, availability_lots, car_park_type))
          output$table <- renderTable(df1, sanitize.text.function=function(x){x})
        })
        incProgress(1/5)
        
      })
    }
  })
}

details_server = function(input, output, session) {
  observe({
    id = NULL
    
    if (get_page() == "details") {
      withProgress(
        message = 'Fetching data...',
        value = 1/5, {
          
          # Get carpark id
          id = get_query_param()[['id']]
          
          # Extract data from database
          tryCatch({
            a <- "https://data.gov.sg/api/action/datastore_search?resource_id=139a3035-e624-4f56-b63f-89ae28d4ae4c&limit=2500"
            cp <- fromJSON(a)
            cp <- cp$result$records
            hdbs <- convertGeom(cp)
          },
          error = function(e) {
            showModal(modalDialog(title = "Sorry!",
                                  tags$p("We couldn't retrieve the carpark data at this time."),
                                  tags$p("Give another one a try!")))
          })
          incProgress(1/5)
          
          # Match id of carpark
          tryCatch({
            hdbs <- hdbs %>% filter(car_park_no == id)
            if (nrow(hdbs) == 0) {
              stop("Cannot find carpark")
            }
          },
          error = function(e) {
            showModal(modalDialog(title = "Sorry!",
                                  tags$p("We couldn't find this carpark."),
                                  tags$p("Please try another!")))
          })
          output$carpark_name = renderText(hdbs$address[1])
          incProgress(1/5)
          
          # Get overall availability data
          tryCatch({
            carParkId = id
            #trend data starts here 
            #last 24 hours
            past12 <- c(1:12)
            q <- data.frame(past12)
            
            #returns the last 24 hours date and time
            q <- q %>% mutate(past12 = floor_date(Sys.time(), "15 minutes") - hours(past12))
            #add in another column with just the time for plotting purposes 
            q <- q %>% mutate(time = substr(q$past12,12,16))
            #convert the past24 into the format for API calls
            q <- q %>% mutate(dt = gsub(" ","T",past12))
            
            #for loop to get the data into the dataframe
            for (i in 1:length(q[,1])){
              g <- carpark_availability(q[i,3]) # get availability for that hour
              hdbs$total_lots <- g$total_lots[1]
              j <- g %>% filter(id ==carParkId) %>% select(availability_lots)
              j <- j[[1]]
              q$available_lots[i] <- j 
              incProgress(1/60)
            }
            
            x_labels <- q$time
            names(x_labels) <- q$past12
            
            output$plot <- renderPlot({
              ggplot(q,aes(x=past12,y=q$available_lots,group =1)) + 
                geom_area(fill="red", alpha=0.3) +
                geom_line(color="red") + 
                geom_point() + 
                scale_x_time(breaks=q$past12, labels=q$time) +
                geom_text(label=q$available_lots,vjust=-.5) + 
                labs(title="Last 12 hours Carpark Availability") + 
                ylab("Number of available lots") + xlab("Time")
            })
            
          },
          error = function(e) {
            showModal(modalDialog(title = "Sorry!",
                                  tags$p("We couldn't find this carpark."),
                                  tags$p("Please try another!")))
          })
          
          # Render
          tryCatch({
            hdbs = hdbs %>%
              subset(select = c(car_park_no, address, type_of_parking_system,
                                car_park_type, total_lots, short_term_parking, 
                                free_parking, night_parking))
            output$details_table = renderTable(hdbs)
            output$carpark_name = renderText(hdbs$address[1])
          },
          error = function(e) {
            showModal(modalDialog(title = "Sorry!",
                                  tags$p("We couldn't retrieve the carpark data at this time."),
                                  tags$p("Give another one a try!")))
          })
          incProgress(1/5)
        })
    }
  })
}

####################################################################

# Define app routing
router <- make_router(
  route("/", search_page, search_server),
  route("details", details_page, details_server)
)

# Define UI for app
ui <- fluidPage(
  router$ui
)

#Define server logic
server <- shinyServer(function(input, output, session) {
  router$server(input, output, session)
})

shinyApp(ui=ui, server=server)