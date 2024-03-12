## CC BY 4.0! sisältää Maanmittauslaitoksen Maastokartan 03/2024 aineistoa
## NOTE rocks in water classes: Vedenalainen (38511) Pinnassa (38512) 
##    Vedenpäällinen (38513) Vesikivikko (38600) 
## FIXME from maasto2, maastokuvionreuna uniquely epamaarainen reunaviiva ja yksikasitteinen
##    see email mml 11.3. may wish to grab matalikko 38600 and maatuva vesialue 38300 -
##    not doing this now for simplicity but may need to discuss; inspected visually and it seems
##    unnecessary for our purposes

## libraries
library(reshape2)
library(plyr)
library(dplyr)
library(sf)
library(sp)
library(terra)
library(leaflet)

## ==================================================================================
## define functions needed later
## ======================================================================================
## function that combines the classes we want from ter and selects accordingly
## FIXME there is still some rantaviiva missing by e.g. Preiviikki, so some category is still needed
##    though could also try to buffer so that they get joined in any case.
## I attempted to find the necessary classes but nothing uniquely coastal came up.
tersub <- function(dat) {
  dat$luokka <- paste(dat$kohdeluokka, dat$kartografinenluokka, sep = ":")
  # define wantclasses
  want <- c("30211:36211","30212:36211", "30100:36211")
  dat$ranta <- "ei"
  dat$ranta[which(dat$luokka %in% want)] <- "joo"
  dat$ranta <- as.factor(dat$ranta)
  print(length(which(dat$ranta =='joo')))
  dat <- dat[which(dat$ranta == "joo"),]
  return(dat)
}

## function that takes position information of duplicates
dupcheck <- function(idlist) {
  outlist <- list() 
  outlist[[1]] <- NA
  for (i in 2:length(idlist)) {
    baseid <- idlist[[i-1]]
    toRm <- idlist[[i]][which(idlist[[i]] %in% baseid)]
    if (length(toRm)>0) {outlist[[i]] <- toRm} else {outlist[[i]] <- NA}
  }
  return(outlist)
}

## function that removes the duplicated information
removedups <- function(ids, dat) {
  if (is.na(ids[1])) {return(dat)} else {
    dat <- dat[-which(dat$mtk_id %in% ids),]
  }
  return(dat)
}

## =======================================================================================
## read and preprocess data, grab as list
## =======================================================================================
## define situation
tlaywant <- "maastokuvionreuna" 
hlaywant <- c("vesikivi","vesikivikko")
slaywant <- "meri" # for processing, h will be subtracted from s
nfiles <- 5

tfiles <- paste("../dat-private/rauma/maasto-", c(1:nfiles), ".gpkg", sep="")
hfiles <- paste("../dat-private/rauma/hydro-", c(1:nfiles), ".gpkg", sep="")
sfiles <- paste("../dat-private/rauma/hydro-", c(1:nfiles), ".gpkg", sep="")

# rep for Map
hfiles <- rep(hfiles, times=length(hlaywant))
hlaywant <- rep(hlaywant, each=nfiles)

## get all available layers
tlay <- lapply(tfiles, st_layers) # could specify $name after st_layers for name only
hlay <- lapply(hfiles, st_layers)

## take in layer(s) we want, recall hydro needs Map
tdatz <- lapply(tfiles, st_read, layer=tlaywant)
names(tdatz) <- paste("tdat",seq_along(tdatz), sep="") # probs no need for informative names

hdatz <- Map(st_read, dsn=hfiles, layer=hlaywant) # first all vesikivi, then vesikivikko
names(hdatz) <- paste("hdat",seq_along(hdatz), sep="") # probs no need for informative names

sdatz <- lapply(sfiles, st_read, layer=slaywant)
names(sdatz) <- paste("sdat",seq_along(sdatz), sep="") # probs no need for informative names

## make maasto smaller by grabbing the rows we need
## NOTE: initially done by inspection through leaflet, confirmed with mml via email 11.3.
tdatz <- lapply(tdatz, tersub)

## change crs to leaflet
tdatz <- lapply(tdatz, st_transform, crs = 4326)
hdatz <- lapply(hdatz, st_transform, crs = 4326)
sdatz <- lapply(sdatz, st_transform, crs = 4326)

## since had to manually draw download boxes, need to check for duplicated objects
## Note not doing this for sea as will dissolve it all
tidz <- lapply(tdatz, function(x) {ids <- c(x$mtk_id)}) # recall still named tdat[n]!
hidz <- lapply(hdatz, function(x) {ids <- c(x$mtk_id)}) # recall still named tdat[n]!

tdups <- dupcheck(tidz)
hdups <- dupcheck(hidz) ## note this is a naughty way to use the function, as kivikko and kivi
##    will have no ids in common so the first entry of kivikko is protected from having non-NA entries

tdatz <- Map(removedups, tdups, tdatz)
hdatz <- Map(removedups, hdups, hdatz)

## time to combine the list objects of similar type
tdatz <- do.call(rbind, tdatz)
hpoints <- do.call(rbind, hdatz[1:nfiles] )
hpolys <- do.call(rbind, hdatz[nfiles+1:length(hdatz)])
sdatz <- do.call(rbind, sdatz)

## dissolve sea
sdatz <- st_union(sdatz)

## ===============================================================================================
## create descriptive values for the different classes of map object
## ==============================================================================================
beach <- data.frame(kohdeluokka=unique(tdatz$kohdeluokka), Ranta=NA)
beach <- beach[order(beach$kohdeluokka),] # protect from mismatches in Ranta def
beach$Ranta <- c("Keinotekoinen",'Yksikasitteinen',"Epamaarainen")

rock <- data.frame(kohdeluokka=unique(hpoints$kohdeluokka), Kivi=NA)
rock <- rock[order(rock$kohdeluokka),] # ibid
rock$Kivi <- c("Vedenalainen",'Pinnassa',"Vedenpaallinen")

## NOTE: kivikko is only kivikko

## merge into parent
tdatz <- sp::merge(tdatz, beach)
hpoints <- sp::merge(hpoints, rock)

## ===========================================================================================
## plots on leaflet checking stuff out
## ====================================================================================
## define colour scheme for beach
beachpal <- colorFactor(palette = c('#d01c8b','#4dac26','#b8e186'), 
                        levels = c("Keinotekoinen",'Yksikasitteinen',"Epamaarainen"))
## define colour scheme for rock
rockpal <- colorFactor(palette = c('#80cdc1','#018571','#a6611a'),
                       levels = c("Vedenalainen",'Pinnassa',"Vedenpaallinen"))

## look what it is
leaflet() %>% 
  addTiles() %>%
  setView(lng = 21.51127, lat = 61.62724, zoom = 10) %>%
  addProviderTiles(providers$OpenTopoMap, options=providerTileOptions(minZoom=11)) %>% 
  #addProviderTiles("CartoDB.Positron", options=providerTileOptions(maxZoom=10, apikey=apithunder)) %>% # , options = providerTileOptions(opacity = 0)
  addPolylines(data=tdatz, weight=4, color=~beachpal(Ranta), opacity = 0.6, label=~kartografinenluokka) %>%
  addPolygons(data=hpolys, weight=3, color='black', fillColor = '#80cdc1', label="Kivikko", fillOpacity = 0.7) %>%
  addCircles(data=hpoints, weight=3, color=~rockpal(Kivi), fillColor = ~rockpal(Kivi)) %>%
  addLegend("bottomleft", pal=beachpal, values=tdatz$Ranta, title="Rantaviiva") %>%
  addLegend("bottomleft", pal=rockpal, values=hpoints$Kivi, title="Vesikivi") %>%
  addScaleBar(position="bottomright", options = scaleBarOptions(imperial=F)) %>%
  addPolygons(data=sdatz, color="#045a8d", fillColor = "#045a8d", 
              highlightOptions = highlightOptions(color="white", fillColor = "white", 
                                                  opacity = 0.1, fillOpacity = 0.1))

## ============================================================================================
## save data
## =============================================================================================
savedat <- F

if(savedat) {
  saveRDS(tdatz, "../dat-private/rauma/dat-mod/beach.rds")
  saveRDS(hpoints, "../dat-private/rauma/dat-mod/rocks.rds")
  saveRDS(hpolys, "../dat-private/rauma/dat-mod/rockies.rds")
  saveRDS(sdatz, "../dat-private/rauma/dat-mod/sea.rds")
}
