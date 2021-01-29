library(MAS6005)
library(shiny)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(tidyverse)
library(dplyr)
library(shinythemes)
library(colorspace)
library(scales)
library(plyr)
options(width = 250)

# Define the user interface
ui <- fluidPage(theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("Change & Explore:"), # comma before the next element (sidebarLayout())
  
  # Sidebar with two controls ("widgets"): interval type, and confidence/prediction level
  sidebarLayout(
    sidebarPanel(align="center",
      # First control: a widget to select the axis type
      selectInput(inputId = "where", 
                   label = "City", 
                   choices = list("Sheffield" = "Sheffield",
                                  "Yeovilton" = "Yeovilton",
                               "Durham" = "Durham",
                               "Heathrow" = "Heathrow", 
                               "Newtonrigg" = "Newtonrigg", 
                               "Cambridge" = "Cambridge",
                               "Bradford" = "Bradford", 
                               "Oxford" = "Oxford", 
                               "Suttonbonington"= "Suttonbonington",
                               "Waddington" = "Waddington",
                               "Manston" ="Manston", 
                               "Shawbury" = "Shawbury",
                               "Rossonwyne" = "Rossonwye"),
                  selected = 1), # comma before the next widget
      
      
      selectInput(inputId = "month", 
                   label = "Month of the year", 
                  choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4 , "May" = 5, "June"= 6,
                                 "July"=7, "August" = 8 , "September" = 9, "October" = 10, "Novemeber" = 11, 
                                 "December" = 12),
                   selected = 1), # comma before the next widget
      
      numericInput(inputId ="year", label = "Choose what year to view:",
                  min = 1853, max = 2020, value = 1965, step = 1),
      
      

    
      selectInput("Measure", label = "What do you want to measure on the map?", 
                choices = list("Maximum Temperature (Degrees Celcius)" = "tmax", 
                               "Minimum Temperature (Degrees Celcius)" = "tmin", 
                               "Days of Frost (Days)" = "af",
                               "Total Rainfall (Millimeters)" = "rain",
                               "Total sunshine duration (Hours)" = "sun"), 
                selected = "Maximum Temperature"),
      hr(),
    ),
    # Results to be displayed in the app
    mainPanel(
      h1("Exploring weather in different UK cities", align="center"),
    
      tabsetPanel(
        tabPanel(align="center", "Comparing Temperature", h4("By selecting and changing the city and month from the interactive side panel
                                              we can explore the maximum (hottest) and minimum (coldest) temperatures each month."), plotOutput("scatterPlot")), 
        tabPanel(align="center", "What's the weather like in your city?", h3("What's the weather like in the cities of the UK?", align = "center"), strong("Drag, zoom and explore the map.
                                                                                                                     Click a coloured circle to reavel the place name.
                                                                                                                                           The circles change size depending on what the weather is like."),
                 leafletOutput("UKmap"), p(size=18, "- Can you find which UK city is the furthest North?"), p("-Can you find which UK city is the furthest Souht?"),
                 p("-Have you ever visited any of these cities?")),
        tabPanel(align="center", "Amount of Rainfall", h3("Has the amount of rainfall changed over the years?", align = "center"),
                 strong("Use the interactive side panel to change the year and the month to explore."),
                 plotOutput("Rainfall"), p("Which months were the rainiest?")),
        tabPanel(align="center", "Sunshine and Frost", h3("Can you see a relationship between the hours of sunsine and the days of airfrost?", align = "center"),
                 strong("Use the interactive side panel to change the year and month."), strong("What month are hours of sunshine the most?"),
                        strong("What month are air frost days the most?"), strong("Does this tell you which might be the coldest and warmest months?"),
                 tableOutput("sunfrost")),
        tabPanel(align="center", "Questions", h3("Lets see what we have learnt...", align = "center", h4("Work by yourself to answer the following questions using the app. Check and compare your answers with a partner."),
                                                 align = "center"),
                 p("1. What year(s) did we see the hottest temperature on our record in Newtonrigg in June?"),
                 p("2. What year(s) did we see the coldest temperature on our record in Sheffield in December?"),
                 p("3. What city had the most rain in September 1973?"),
                 p("4. What city had the least rain in April 2010?"),
                 p("5. Which city was the sunniest in July 1988?"),
                 p("6. Which city was the frostiest in February 1990?"),
                 textOutput("Questions"))
        
      )
      
    )
  )
)


server <- function(input, output) {
  
  ######
  #Ryan's code that sets up the data for the map - doubt there there will be some duplication here but for speed, the duplication is tolerated!
  
  
  
  locations <- c("Sheffield", 
                 "Yeovilton",
                 "Durham", 
                 "Heathrow",
                 "NewtonRigg",
                 "Cambridge",
                 "Bradford",
                 "Oxford",
                 "Suttonbonington",
                 "Waddington",
                 "Manston",
                 "Shawbury",
                 "RossonWye")
  
  
  
  LocationData <- vector(mode = "list", length = length(locations))
  SpatialData <- vector(mode = "list", length = length(locations))
  
  
  (filePaths <- paste0("http://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/", tolower(locations), "data.txt"))
  
  for(i in 1:length(LocationData)){
    data <- read_lines(filePaths[i],
                       skip_empty_rows = TRUE)
    
    dataclean <- data  %>%
      str_remove_all(pattern = "\\*") %>%
      str_remove_all(pattern = "#") %>%
      str_replace_all(pattern = "---",
                      replacement = "NA") %>%
      str_trim()
    
    header <- str_which(dataclean, pattern = "yyyy")
    endLine <- length(dataclean)
    spatline <- str_which(dataclean, pattern = "Location")
    
    LocationData[[i]] <- 
      read_table2(dataclean[header:endLine])[-c(1), ] %>%
      mutate(location = locations[i])
    
    #spatial data
    SpatialData[[i]] <- 
      paste(str_trim(dataclean[2]), locations[i], sep= " ")
  }
  #above loop gets all data,
  test <- do.call(rbind, SpatialData)
  #this is misaligned but can still get location data for each region.
  test2 <- read_table2(test, col_names = FALSE)
  #replace missing locations in x11, this fixes above
  test2$X11[is.na(test2$X11)] <- test2$X10[is.na(test2$X11)]
  #1 make big table of all tibbles
  alldatatib <- do.call(rbind, LocationData)
  all_df <- as.data.frame(alldatatib)
  all_df$x <- paste(all_df$yyyy, "-", all_df$mm)
  test <- do.call(rbind, SpatialData)
  #this is misaligned but can still get location data for each region.
  allspat <- read_table2(test, col_names = FALSE)
  #replace missing locations in x11, this fixes above
  allspat$X11[is.na(allspat$X11)] <- allspat$X10[is.na(allspat$X11)]
  
  
  
  ######
  #Ryan's code end
  
  
  

  
  
  
  
  
  places <- c("sheffield", "yeovilton", "durham", "heathrow", "newtonrigg", "cambridge",
              "bradford", "oxford", "suttonbonington", "waddington", "manston", "shawbury", "rossonwye")
  filePaths <- paste0("https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/", places, "data.txt")
  
  placesresults <- vector(mode = "list", length = length(places))
  
  for(i in 1:13){
    
    placesTextRaw <- read_lines(filePaths[i])
    
    placesTextClean <- placesTextRaw %>%
      str_replace_all(pattern = "---",
                      replacement = "NA") %>%
      str_remove_all(pattern = "\\*") %>%
      str_remove_all("Provisional") %>%
      str_trim()
    
    placesTextClean <- placesTextClean[-c(1,2,3,4,5,7)]
    placesTextClean <- placesTextClean %>%
      str_replace("sun", "sun  sun_recorder")
    placesTextClean <- placesTextClean %>%
      str_replace("#", "  1")
    
    
    placesresults[[i]] <- 
      read_table2(placesTextClean) %>%
      mutate(City = places[i])
    
  }
  weather_data <- do.call(rbind.data.frame, placesresults)
  weather_data$sun_recorder <- as.integer(weather_data$sun_recorder)

  spat_results <- vector(mode = "list", length = length(places))
  
  for(i in 1:13){
    placesTextRaw <- read_lines(filePaths[i])
    
    spat_info <- unlist(regmatches(placesTextRaw[2], gregexpr('\\(?[-0-9.]+', placesTextRaw[2])))
    spat_info <- data.frame(matrix(parse_number(spat_info), ncol = 5, byrow = T))
    colnames(spat_info) <- c("E", "N", "lat", "long", "alt")

    spat_info <- spat_info %>%
      select("lat", "long", "alt")
    spat_results[[i]] <- cbind(places[i], spat_info)
    
  }
  
  spat_info <- do.call(rbind.data.frame, spat_results)
  alldata <- vector("list", length = length(places))
  names(alldata) <- places
  
  for(i in 1:13){
    alldata[[i]] <- cbind(spat_info[i, ], weather_data %>% filter(City == places[i]))
  }
  alldata <- do.call(rbind, alldata)
  
  alldata <- alldata %>% 
    mutate(City =
             recode_factor(City, 
                           `durham`="Durham", `yeovilton`="Yeovilton", `cambridge`="Cambridge", `manston`="Manston",
                           `oxford`="Oxford", `waddington`="Waddington", `heathrow`="Heathrow", `bradford`="Bradford",
                           `suttonbonington`="Suttonbonington", `newtonrigg`="Newtonrigg", `rossonwye`="Rossonwye", 
                           `shawbury`="Shawbury", `sheffield`= "Sheffield"))
  
  altereddata <- imputeTS::na_interpolation(alldata)
  
  altereddata <- altereddata %>% 
    mutate(City =
             recode_factor(City, 
                           `durham`="Durham", `yeovilton`="Yeovilton", `cambridge`="Cambridge", `manston`="Manston",
                           `oxford`="Oxford", `waddington`="Waddington", `heathrow`="Heathrow", `bradford`="Bradford",
                           `suttonbonington`="Suttonbonington", `newtonrigg`="Newtonrigg", `rossonwye`="Rossonwye", 
                           `shawbury`="Shawbury", `sheffield`= "Sheffield"))
  
  attach(alldata)
  attach(altereddata)
  
  output$scatterPlot <- renderPlot({
    
    altereddata %>% 
      filter(City == input$where, mm == input$month)%>% 
      ggplot(aes(x=yyyy)) +
      labs(x= "Year", y = "Temperature (oC)",
           title = "Maximum (hottest) and Minimum (coldest) temperatures in chosen month")+
      geom_line(aes(y = tmax, color = "red")) +
      geom_line(aes(y=tmin, color = "blue")) +
      scale_color_identity(name = "", breaks= c("blue","red"),
                           labels = c("Minimum Temperature", "Maximum Temperature"),
                           guide = "legend")+
      theme_grey(base_size = 22)
    
  })
  
  output$UKmap <- renderLeaflet({
    
    
    #create dataframe of stat, need to do this to take into account of missing values#
    
    # if (input$Measure=='af' | input$Measure == 'rain' | input$Measure == 'tmin') {
    #   mmf <- 1
    # } else {
    #   mmf <- 6
    # }
    
    
    distinct_df = all_df %>% distinct(location)
    metric <- filter(all_df, yyyy == input$year & mm == input$month )
    metric2 <- join(x = distinct_df, y = metric, by = "location"
                    #, all.x = TRUE
    )
    
    #define vars
    latitude <- allspat[["X5"]]
    longitude <- allspat[["X7"]]
    placeNames <- allspat[["X11"]]
    test <- allspat[["X5"]]
    metric3 <- as.numeric(metric2[[input$Measure]])
    
    
    if(input$Measure == 'tmax') {
      metric3lvl <- cut(as.numeric(metric3), c(-100,10, 12, 14, 16, 18, 20, 22, 24, 100), include.lowest = T)
      calc <- metric3 
      pal <- 'RdYlGn'
    } else if (input$Measure == "tmin") {
      metric3lvl<- cut(as.numeric(metric3), c(-100, -5, -1, 0, 1, 2, 3, 4, 5, 100), include.lowest = T)
      pal <- 'GnBu'
      calc <- metric3*2
    } else if (input$Measure == "af") {
      metric3lvl<- cut(as.numeric(metric3), c(0, 3, 6, 8, 10, 12, 15, 100), include.lowest = T)
      pal <- 'Blues'
      calc <- metric3*2
    } else if (input$Measure == "rain") {
      metric3lvl<- cut(as.numeric(metric3), c(0, 30, 40, 50, 60, 72, 82, 1000), include.lowest = T)
      pal <- 'PRGn'
      calc <- (metric3)^0.7
    } else {
      metric3lvl<- cut(as.numeric(metric3), c(0, 60, 100, 130, 160, 190, 210, 1000), include.lowest = T)
      pal <- 'RdYlGn'
      calc <- metric3^0.6
    }
    
    
    beatCol <- colorFactor(palette = pal , metric3lvl, reverse = TRUE)
    beatCol(metric3lvl)
    
    
    
    
    leaflet() %>%
      setView(lng = -1.47, lat = 52.3, zoom = 6) %>%
      addTiles() %>%
      addCircleMarkers(lat = latitude,
                       lng = longitude,
                       popup = placeNames,
                       radius = calc,
                       color = beatCol(metric3lvl),
                       #clusterOptions = markerClusterOptions()
                       #stroke = FALSE,
                       label = metric3,
                       opacity = 100,
                       fillOpacity = 0.4,
                       labelOptions = labelOptions(noHide = TRUE, 
                                                   offset=c(0,0),
                                                   #offset=c(0,0),
                                                   textOnly = FALSE,
                                                   style = list(
                                                     "color" = "black",
                                                     "font-family" = "arial",
                                                     "font-style" = "bold",
                                                     "direction" = "right",
                                                     "box-shadow" = "2px 2px rgba(0,0,0, 0.02)",
                                                     "font-size" = "11px",
                                                     "border-color" = "rgba(0, 0, 0, 0.02)") 
                       )
      )
  })
  
 output$Rainfall <- renderPlot({
   
   altereddata %>%
     filter(mm == input$month, yyyy == input$year) %>%
     select(rain, City) %>%
     ggplot(aes(x = reorder(as.factor(City), -rain), y= rain))+
     geom_col(fill="light blue", color= "navy")+
     coord_flip()+
     labs(x = "UK City", y = "Amount of monthly rainfall (mm)", title = "Typical amount of rainfall every month")+
     theme_minimal(base_size = 22)
 })
 
 
 output$sunfrost <- renderTable({
   
   
   sun_frost <- data.frame(
     
     Hours_of_sunshine = c(alldata %>%
                                          filter(mm == input$month, yyyy==input$year) %>%
                                          select(City, sun)),
     Days_of_frost = c(alldata %>%
                                        filter(mm == input$month, yyyy==input$year) %>%
                                        select(af))
   )
   
   names(sun_frost)[names(sun_frost)=="Hours_of_sunshine.sun"] <- "Hours of sunshine"
   names(sun_frost)[names(sun_frost)=="Hours_of_sunshine.City"] <- "City"
   names(sun_frost)[names(sun_frost)=="af"] <- "Days of frost"
   
   sun_frost
   
 })
 
 
}
# Run the application 
shinyApp(ui = ui, server = server)