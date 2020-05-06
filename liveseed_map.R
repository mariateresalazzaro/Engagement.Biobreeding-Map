
setwd("C:/Users/mariateresa.lazzaro/data_pc_only_Teresa/ENGAGEMENT_BIOBREED_pc_only/01 MAPPING Plant Breeding Initiatives/R map biobreeding wv")

#Call the libraries
library(leaflet)
library(sp)
library(rgdal)
library(RColorBrewer)
library(leaflet.extras)
library(leaflet.minicharts)
library(htmlwidgets)
library(raster)
library(mapview)
library(leafem)
library(leafpop)
library(sf)
library(htmltools)
library(dplyr)


## PART 1 - IN THIS PART THE CODE READS THE FILES AND ATTRIBUTES COLORS AND ICONS TO ELEMENTS

## Read the shapefile
countries <- readOGR('Europe/Europe.shp')
countries$NUMBER<-c(0,0,5,2,1,0,0,2,1,2,6,15,0,1,0,1,1,0,6,0,1,0,0,0,1,0,0,0,8,0,1,3,0,0,0,0,13,0,9,2,0,0,0,0,0,0,0,0,0,2,0,1,0,0)
## Create the palette of colors for the shapefiles
palette_countries <- colorNumeric(palette = "YlOrRd", domain = countries$CNTR_BN_ID)

## Read the csv
#data <- read.csv("pizzamap.csv")
data<-read.delim("liveseed_data.txt", header = T)

#html hyperlink
data <- data %>% 
  mutate(tag = paste0("Web Link: <a href=", link,">", link, "</a>"))

## Create a html popup
content <- paste(sep = "<br/>",
                        paste0("<div class='leaflet-popup-scrolled' style='max-width:200px;max-height:200px'>"),
                        paste0("<b>", data$name),
                        paste0("<b>", "Breeding type is ", data$type),
                        paste0(data$tag),
                        paste0("</div>"))



## PART 2 - IN THIS PART THE CODE ADDS ELEMENT ON THE MAP LIKE POLYGONS, POINTS AND IMAGES.

m <- leaflet() %>%
  ## Basemap
  ##addTiles(tile)        %>%
  addProviderTiles(providers$CartoDB.Positron)  %>%
  
  ## Add a zoom reset button
  addResetMapButton() %>%
  ## Add a Minimap to better navigate the map
  addMiniMap() %>%
  ## Add a coordinates reader
  leafem::addMouseCoordinates() %>%
  ## define the view
  setView(lng = 3.721387, 
          lat = 45.546099, 
          zoom = 3 ) %>%
  
  ## Add the polygons of the shapefiles
  addPolygons(data = countries,
             stroke = FALSE, 
            smoothFactor = 0.2, 
              fillOpacity = 1,
             color = ~palette_countries(NUMBER),
            group = "Countries",
            label = ~paste("In", NAME, "there are", NUMBER, " initiatives", sep = " ")) %>%
 
   ## Add a legend with the occurrences of the toponyms according to the macro areas
  addLegend("bottomleft", 
          pal = palette_countries, 
           values = countries$NUMBER,
           title = "Organc Breeding initiatives by country:",
            labFormat = labelFormat(suffix = "initiatives"),
            opacity = 1,
            group = "Countries") %>%
  
  ## Add Markers with clustering options
  addAwesomeMarkers(data = data, 
                    lng = ~long,
                    lat = ~lat, 
                    popup = c(content), 
                    group = "Organic breeding Organizations",
                    options = popupOptions(maxWidth = 100, maxHeight = 150), 
                    clusterOptions = markerClusterOptions())%>%
  
  ## Add Markers for OPB
  addAwesomeMarkers(data = data[which(data$typeOPB=="OPB"),], 
                    lng = ~long,
                    lat = ~lat, 
                    popup = c(content), 
                    group = "Organic Plant Breeding (OPB)",
                    options = popupOptions(maxWidth = 100, maxHeight = 150), 
                    )%>%
  
  ## Add Markers with clustering optionsfor DOPB
  addAwesomeMarkers(data = data[which(data$typeDOPB=="DOPB"),], 
                    lng = ~long,
                    lat = ~lat, 
                    popup = c(content), 
                    group = "Decentralised Organic Plant Breeding (DOPB)",
                    options = popupOptions(maxWidth = 100, maxHeight = 150), 
                    )%>%
  ## Add Markers for BfO
  addAwesomeMarkers(data = data[which(data$typeBfO=="BfO"),], 
                    lng = ~long,
                    lat = ~lat, 
                    popup = c(content), 
                    group = "Breeding for Organic (BfO)",
                    options = popupOptions(maxWidth = 100, maxHeight = 150), 
                   )%>%
  
  ## Add Markers for CB_OS
  addAwesomeMarkers(data = data[which(data$typeCB_OS=="CB_OS"),], 
                    lng = ~long,
                    lat = ~lat, 
                    popup = c(content), 
                    group = "Conventiona lbreeding, organic seed production (CBOS)",
                    options = popupOptions(maxWidth = 100, maxHeight = 150), 
                   )%>%
  
  ## Add a legend with the credits
  addLegend("topright", 
            
            colors = c("trasparent"),
            labels=c("This map is based on data by ENGAGEMENT.BIOBREEDING EUROPE and LIVESEED (H2020 Grant Agreement No 727230)"),
            
            title="Initiatives related to Organic Breeding in Europe: ")%>%
  
 
  ## PART 3 - IN THIS PART THE CODE MANAGE THE LAYERS' SELECTOR
  
  ## Add the layer selector which allows you to navigate the possibilities offered by this map
  
  addLayersControl(baseGroups = c("Organic breeding Organizations",
                                  "Organic Plant Breeding (OPB)",
                                  "Decentralised Organic Plant Breeding (DOPB)",
                                  "Breeding for Organic (BfO)",
                                  "Conventional breeding, organic seed production (CBOS)",
                                "Empty layer"),
                   
                   overlayGroups = c("Countries"),
                   
                   options = layersControlOptions(collapsed = F)) %>%
  
  ## Hide the layers that the users can choose as they like
  hideGroup(c("Empty"))

## Show the map  
m

