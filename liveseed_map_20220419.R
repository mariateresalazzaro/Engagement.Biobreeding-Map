

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
#countries <- readOGR('Europe/Europe.shp')
#countries$NUMBER<-c(0,0,5,2,1,0,0,2,1,2,6,15,0,1,0,1,1,0,6,0,1,0,0,0,1,0,0,0,8,0,1,3,0,0,0,0,13,0,9,2,0,0,0,0,0,0,0,0,0,2,0,1,0,0)
## Create the palette of colors for the shapefiles
#palette_countries <- colorNumeric(palette = "YlOrRd", domain = countries$CNTR_BN_ID)

## Read the csv
data<-read.delim("liveseed_data.txt", header = T, fileEncoding = "UTF-8")
data$long<-as.numeric(data$long)
#remove lines with no lat,long
data<-data[!is.na(data$lat),]

#html hyperlink
data <- data %>% 
  mutate(tag = paste0("Web Link: <a href=", link," target=_blank>",  link, "</a>"))

#subset
dataOPB<-data[which(data$typeOPB=="OPB"),]
dataDOPB<-data[which(data$typeDOPB=="DOPB"),]
dataBfO<-data[which(data$typeBfO=="BfO"),]
dataCB_OS<-data[which(data$typeCB_OS=="CB_OS"),]
dataother<-data[which(data$type_other=="other"),]




## Create a html popup all
content <- paste(sep = "<br/>",
                 paste0("<div class='leaflet-popup-scrolled' style='max-width:300px;max-height:400px'>"),
                 paste0("<b>", data$name),
                 paste0(data$tag),
                 paste0("Description:"),
                 paste0(data$info),
                 paste0("</div>"))

## Create a html popup OPB
contentOPB <- paste(sep = "<br/>",
                    paste0("<div class='leaflet-popup-scrolled' style='max-width:300px;max-height:400px'>"),
                    paste0("<b>", dataOPB$name),
                    paste0(dataOPB$tag),
                    paste0("Description:"),
                    paste0(dataOPB$info),
                    paste0("</div>"))

## Create a html popup DOPB
contentDOPB <- paste(sep = "<br/>",
                     paste0("<div class='leaflet-popup-scrolled' style='max-width:300px;max-height:400px'>"),
                     paste0("<b>", dataDOPB$name),
                     paste0(dataDOPB$tag),
                     paste0("Description:"),
                     paste0(dataDOPB$info),
                     paste0("</div>"))

## Create a html popup BfO
contentBfO <- paste(sep = "<br/>",
                    paste0("<div class='leaflet-popup-scrolled' style='max-width:300px;max-height:400px'>"),
                    paste0("<b>", dataBfO$name),
                    paste0(dataBfO$tag),
                    paste0("Description:"),
                    paste0(dataBfO$info),
                    paste0("</div>"))

## Create a html popup CB_OS
contentCB_OS <- paste(sep = "<br/>",
                      paste0("<div class='leaflet-popup-scrolled' style='max-width:300px;max-height:400px'>"),
                      paste0("<b>", dataCB_OS$name),
                      paste0(dataCB_OS$tag),
                      paste0("Description:"),
                      paste0(dataCB_OS$info),
                      paste0("</div>"))

## Createother a html popup other
contentother <- paste(sep = "<br/>",
                      paste0("<div class='leaflet-popup-scrolled' style='max-width:300px;max-height:400px'>"),
                      paste0("<b>", dataother$name),
                      paste0(dataother$tag),
                      paste0("Description:"),
                      paste0(dataother$info),
                      paste0("</div>"))
## Create icons
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'white',
  library = 'ion',
  markerColor="darkgreen" ) 

## PART 2 - IN THIS PART THE CODE ADDS ELEMENT ON THE MAP LIKE POLYGONS, POINTS AND IMAGES.

m <- leaflet() %>%
  ## Basemap
  addTiles()%>%
  addProviderTiles(providers$Esri)  %>%
  
  ## Add a zoom reset button
  addResetMapButton() %>%
  ## Add a Minimap to better navigate the map
  addMiniMap() %>%
  ## Add a coordinates reader
  leafem::addMouseCoordinates() %>%
  ## define the view
  setView(lng = 3, 
          lat = 60, 
          zoom = 3 ) %>%
  
  ## Add the polygons of the shapefiles
  # addPolygons(data = countries,
  #             stroke = FALSE, 
  #          smoothFactor = 0.2, 
  #              fillOpacity = 1,
  #           color = ~palette_countries(NUMBER),
  #          group = "Number by Country",
  #          label = ~paste("In", NAME, "there are", NUMBER, " initiatives", sep = " ")) %>%
  
  ## Add a legend with the occurrences of the toponyms according to the macro areas
#  addLegend("bottomleft", 
#         pal = palette_countries, 
#            values = countries$NUMBER,
#            title = "N (all types) by country:",
#             labFormat = labelFormat(suffix = " Organisations"),
#             opacity = 1,
#             group = "Number by Country") %>%

## Add Markers with clustering options


addAwesomeMarkers(data = data,
                  icon = icons,
                  lng = ~long,
                  lat = ~lat, 
                  popup = c(content), 
                  group = "All Organisations mapped",
                  options = popupOptions(maxWidth = 100, maxHeight = 150), 
                  clusterOptions = markerClusterOptions())%>%
  
  ## Add Markers for OPB
  addAwesomeMarkers(data = dataOPB,
                    icon = icons,
                    lng = ~long,
                    lat = ~lat, 
                    popup = c(contentOPB), 
                    group = "Organic Plant Breeding (OPB)",
                    options = popupOptions(maxWidth = 100, maxHeight = 150), 
  )%>%
  
  ## Add Markers with clustering optionsfor DOPB
  addAwesomeMarkers(data = dataDOPB,
                    icon = icons,
                    lng = ~long,
                    lat = ~lat, 
                    popup = c(contentDOPB), 
                    group = "Decentralised Organic Plant Breeding (DOPB)",
                    options = popupOptions(maxWidth = 100, maxHeight = 150), 
  )%>%
  ## Add Markers for BfO
  addAwesomeMarkers(data = dataBfO,
                    icon = icons,
                    lng = ~long,
                    lat = ~lat, 
                    popup = c(contentBfO), 
                    group = "Breeding for Organic (BfO)",
                    options = popupOptions(maxWidth = 100, maxHeight = 150), 
  )%>%
  
  ## Add Markers for CB_OS
  # addAwesomeMarkers(data = dataCB_OS, 
  #                   lng = ~long,
  #                   lat = ~lat, 
  #                   popup = c(contentCB_OS), 
  #                   group = "Conventional breeding, organic seed production (CBOS)",
  #                   options = popupOptions(maxWidth = 100, maxHeight = 150), 
  #                 )%>%
  
  
## Add Markers for other
addAwesomeMarkers(data = dataother,
                  icon = icons,
                  lng = ~long,
                  lat = ~lat, 
                  popup = c(contentother), 
                  group = "Type of breeding activity not specified",
                  options = popupOptions(maxWidth = 100, maxHeight = 150), 
)%>%
  
  ## Add a legend with the credits
  addLegend("topright", 
            
            colors = c("trasparent"),
            labels=c("Plant Breeding Initiatives"),
            
            title="")%>%
  
  
  ## PART 3 - IN THIS PART THE CODE MANAGE THE LAYERS' SELECTOR
  
  ## Add the layer selector which allows you to navigate the possibilities offered by this map
  
  addLayersControl(baseGroups = c("All Organisations mapped",
                                  "Organic Plant Breeding (OPB)",
                                  "Decentralised Organic Plant Breeding (DOPB)",
                                  "Breeding for Organic (BfO)",
                                  #"Conventional breeding, organic seed production (CBOS)",
                                  "Type of breeding activity not specified",
                                  "Empty layer"),
                   
                   #overlayGroups = c("Number by Country"),
                   
                   options = layersControlOptions(collapsed = F)) %>%
  
  ## Hide the layers that the users can choose as they like
  hideGroup(c("Empty"))

## Show the map  
m
