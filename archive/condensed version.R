
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