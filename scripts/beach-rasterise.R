## rasterise sea
## NOTE we do not have depth data for the sea here, so defaulting to depth -10 for sea, 
##    and 0 for not sea.
## load libraries
library(sf)
library(sp)
library(terra)
library(leaflet)

## read data (recall all epsg 4326 to start)
sea <- readRDS("../dat-private/rauma/dat-mod/sea.rds")
notsea1 <- readRDS("../dat-private/rauma/dat-mod/rockies.rds")
notsea2 <- readRDS("../dat-private/rauma/dat-mod/rocks.rds")

datlist <- list(sea=sea, notsea1=notsea1, notsea2=notsea2)

## convert to terra objects for rasterisation
datlist <- lapply(datlist, vect) 

## convert to UTM to get uniform cell size in terms of meters
datlist <- lapply(datlist, project, "EPSG:3067")

## return to environment
list2env(datlist, envir = globalenv())
# release list
rm(datlist)

## Create a raster template
rast_template <- rast(ext(sea), res=10) # This should be 10m cells

## assign a Z value for sea and not sea
sea_rast <- rasterize(sea, rast_template, field=-10, background=0)

## patch the fake land at the western edges of the sea 
#(xmin, xmax, ymin, ymax)
topleft <- c(186803,196092,6825728, 6863000) 
midleft <- c(186803,190844,6800050, 6825728)
bottomleft <- c(186803,188844, 6784020.7366594,6800050) 

## Create a SpatExtent then terra vect object from the bounding box
sealeft <- list(bottomleft, midleft, topleft)
sealeft <- lapply(sealeft,  ext)
sealeft <- lapply(sealeft, vect, crs="epsg:3067")
sealeft <- do.call(rbind, sealeft)

## replace raster values
sea_rast <- rasterize(sealeft, sea_rast, field=-10, update=TRUE)

## now update the notsea1
sea_rast <- rasterize(notsea1, sea_rast, field=0, update=TRUE)

## create a depth proxy for rock
notsea2$Depth <- ifelse(notsea2$Kivi=="Vedenalainen", -1, ifelse(notsea2$Kivi=="Pinnassa", -0.5, 0))

## rasterise notsea2 to sea_rast and take the mean of all rocks existing within a cell
notsea2 <- rasterize(notsea2, sea_rast, field="Depth", fun=mean)
non_na_mask <- !is.na(notsea2) # all other vals are NA - so take just the nonNA
# Then, use these identified cells to update values in 'sea_rast'
sea_rast[non_na_mask] <- notsea2[non_na_mask]

## now for leaflet, return crs of sea_rast
crs(sea_rast) <- "epsg:3067"
searast <- project(sea_rast, "epsg:4326")

## look what it is
seapal <- colorNumeric(palette="BuPu", values(searast),
                    na.color = "transparent")

leaflet() %>% 
  addTiles() %>%
  setView(lng = 21.51127, lat = 61.62724, zoom = 10) %>%
  addProviderTiles(providers$OpenTopoMap, options=providerTileOptions(minZoom=11)) %>% 
  addLegend("bottomleft", pal=seapal, values=values(searast), title="Depth") %>%
  addScaleBar(position="bottomright", options = scaleBarOptions(imperial=F)) %>%
  addRasterImage(searast, colors = seapal, opacity = 0.6) 

## save raster
saveRDS(searast, "../dat-private/rauma/dat-mod/depth-raster-leaflet.rds")
saveRDS(sea_rast, "../dat-private/rauma/dat-mod/depth-raster.rds")
# and for python script
writeRaster(sea_rast, "../dat-private/rauma/dat-mod/depth-raster-py.tiff", 
            datatype="FLT4S", overwrite=TRUE)
