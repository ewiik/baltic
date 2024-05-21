## plot spp/habitat type across the study area

## load libraries
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(leaflet)

## FIXME: something is bugging with terra and it produced empty raster on project()
## had to restart R and run project() one at a time.... So now manually rerunning code
##    by reading in the reprojected files that I had to save

## read data
expo <- rast("../dat-private/rauma/dat-mod/tahko-exp.tiff")
sea <- readRDS("../dat-private/rauma/dat-mod/depth-raster.rds")
ext(expo) <- ext(sea) # need this because python processing removed all projection knowledge from the tiff
crs(expo) <- "epsg:3067"

expo <- readRDS("../dat-private/rauma/dat-mod/tahko-exp-leaf.rds")


expos <- rast("../dat-private/rauma/dat-mod/tahko-exp-six.tiff")
seas <- readRDS("../dat-private/rauma/dat-mod/depth-raster-six.rds")
ext(expos) <- ext(seas) # need this because python processing removed all projection knowledge from the tiff
crs(expos) <- "epsg:3067"

expos <- readRDS("../dat-private/rauma/dat-mod/tahko-exp-leaf-six.rds")

rockies <- readRDS("../dat-private/rauma/dat-mod/rockies.rds")
rocks <- readRDS("../dat-private/rauma/dat-mod/rocks.rds")
beach <- readRDS("../dat-private/rauma/dat-mod/beach.rds")

rockiess <- readRDS("../dat-private/rauma/dat-mod/rockies-six.rds")
rockss <- readRDS("../dat-private/rauma/dat-mod/rocks-six.rds")
rockies <- rbind(rockies, rockiess)
rocks <- rbind(rocks, rockss)

welline <- st_read("../dat-private/rauma/rekaapelireititallecolle/VEA_VEE_centerline_Wellamo.gpkg")
well <- st_read("../dat-private/rauma/rekaapelireititallecolle/VEA_VEE_Wellamo.gpkg")
navline <- st_read("../dat-private/rauma/rekaapelireititallecolle/VEA_VEG_centerline_Navakka.gpkg")
nav <- st_read("../dat-private/rauma/rekaapelireititallecolle/VEA_VEG_Navakka.gpkg")

basedat <- readRDS("../dat-private/rauma/dat-mod/sukellus-pisteet.rds")

## make crs to leaflet for all
linez <- list(welline=welline, well=well, navline=navline, nav=nav)
linez <- lapply(linez, st_transform, crs="epsg:4326")
list2env(linez, envir = globalenv())

expo <- project(expo, "epsg:4326")
expos <- project(expos, "epsg:4326")

basedat <- project(basedat, "epsg:4326")

## make a binary palette based on whether expo more than a desired thing
binexpo <- expo > 50000
binexpos <- expos > 50000

# Convert logical (TRUE/FALSE) to numeric (1/0)
binexpo <- ifel(binexpo, 1,0)
binexpos <- ifel(binexpos, 1,0)

## =============================================================================
## plot leaflet
## ===============================================================================
## palette for rocks
rockpal <- colorFactor(palette = c('#80cdc1','#018571','#a6611a'),
                       levels = c("Vedenalainen",'Pinnassa',"Vedenpaallinen"))
## define colour scheme for beach
beachpal <- colorFactor(palette = c('#d01c8b','#4dac26','#b8e186'), 
                        levels = c("Keinotekoinen",'Yksikasitteinen',"Epamaarainen"))
## palette for exposure
seapal <- colorNumeric(palette="BuPu", values(expos),
                       na.color = "transparent")
bipal <- colorFactor(palette=c('transparent','#2166ac'), domain=c(0,1), na.color = '#969696')

## palette for lutu
lutupal <- colorFactor(palette=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f',
                                 '#ff7f00','#cab2d6','#969696'),
                       levels=c("I1.01 Haurupohjat", "I4.01 Sinisimpukkapohjat", "I02.02 Vitapohjat",
                                "I2.05 Ärviäpohjat", "I04.03 Merirokkopohjat","I1.02 Punaleväpohjat",
                                "I5.03 Yksivuotisten rihmalevien luonnehtimat pohjat ",
                                "I1.03 Monivuotisten rihmalevien luonnehtimat pohjat",
                                "I5.02 Kultajouhi- ja jouhileväpohjat","ei luontotyyppiä"  ))

leaflet() %>% 
  addTiles() %>%
  setView(lng = 21.51127, lat = 61.62724, zoom = 10) %>%
  addProviderTiles(providers$OpenTopoMap, options=providerTileOptions(minZoom=11)) %>% 
  #addProviderTiles("CartoDB.Positron", options=providerTileOptions(maxZoom=10, apikey=apithunder)) %>% # , options = providerTileOptions(opacity = 0)
  addPolylines(data=beach, weight=3, color=~beachpal(Ranta), opacity = 0.6, label=~kartografinenluokka) %>%
  addPolygons(data=rockies, weight=3, color='black', fillColor = '#80cdc1', label="Kivikko", fillOpacity = 0.7) %>%
  addCircles(data=rocks, weight=2, color=~rockpal(Kivi), fillColor = ~rockpal(Kivi), fillOpacity = 0.4) %>%
  #addCircles(data=basedat, weight=5, color=~lutupal(LuTu.final), fillColor = ~lutupal(LuTu.final)) %>%
  addPolygons(data=well, color="#045a8d", fillColor = "#045a8d", 
              highlightOptions = highlightOptions(color="white", fillColor = "white", 
                                                  opacity = 0.1, fillOpacity = 0.1), group = 'Wellamo') %>%
  addPolylines(data=welline, color="#045a8d", group = 'Wellamo',
              highlightOptions = highlightOptions(color="white", 
                                                  opacity = 0.1)) %>%
  addPolygons(data=nav, color="#762a83", fillColor = "#762a83", group='Navakka',
              highlightOptions = highlightOptions(color="white", fillColor = "white", 
                                                  opacity = 0.1, fillOpacity = 0.1)) %>%
  addPolylines(data=navline, color="#762a83", group='Navakka',
              highlightOptions = highlightOptions(color="white", 
                                                  opacity = 0.1)) %>%
  #addRasterImage(expos, colors = seapal, opacity = 0.6, maxBytes = 5900823,
   #             group='Ekspositio', ) %>%
  addRasterImage(binexpos, colors = bipal, opacity = 0.3, maxBytes = 5900823,
                 group='Ekspositio' ) %>%
  addRasterImage(binexpo, colors = bipal, opacity = 0.3, maxBytes = 5900823,
                 group='Ekspositio' ) %>%
  addLayersControl(overlayGroups = c('Navakka', 'Wellamo', 'Ekspositio')) %>%
  hideGroup(c('Ekspositio','Navakka','Wellamo')) %>%
  addLegend("bottomleft", pal=beachpal, values=beach$Ranta, title="Rantaviiva") %>%
  addLegend("bottomleft", pal=rockpal, values=rocks$Kivi, title="Vesikivi") %>%
  addLegend('bottomleft', pal=bipal, values=c(0,1), title='Ekspositio > 50000',
         group = 'Ekspositio') %>%
  addScaleBar(position="bottomright", options = scaleBarOptions(imperial=F)) #%>%
  #addLegend('bottomleft', pal=seapal, values=values(expos), title='Ekspositio',
  #group = 'Ekspositio')
  

