## load libraries
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(sp)
library(sf)
library(terra)

## read in data
dat <- read.csv("../dat-private/rauma/sukellus-gen.csv", stringsAsFactors = F)
lutu <- read.csv("../dat-private/rauma/sukellus-lutu.csv", stringsAsFactors = F)

expo <- rast("../dat-private/rauma/dat-mod/tahko-exp.tiff")
sea <- readRDS("../dat-private/rauma/dat-mod/depth-raster.rds")
ext(expo) <- ext(sea) # need this because python processing removed all projection knowledge from the tiff
crs(expo) <- "epsg:3067"

rockies <- readRDS("../dat-private/rauma/dat-mod/rockies.rds")
rocks <- readRDS("../dat-private/rauma/dat-mod/rocks.rds")

beach <- st_read("../dat-private/rauma/RantaviivaEmmalle.shp")

## =======================================================
## Create sediment index
## ============================================================
## select columns we need
sedcols <- c("Kallio", "Lohkare....3000.mm", "Lohkare.1200.3000.mm", "Lohkare.600.1200.mm",
             "Iso.kivi.100.600.mm", "Pieni.kivi.60.100.mm", "Sora.2.0.60.mm", "Hiekka.0.06.2.0.mm",
             "Siltti.0.002.0.06", "Muta..0.002.mm","Konkreetiot","Keinotekoinen.alusta", "Puun.rungot.oksat")
ocols <- c("Kohteen.nimi.","Arviointiruudun.syvyys")

## melt for these cols
sedat <- melt(dat[-which(duplicated(dat$Kohteen.nimi.)),], #since spp data in long format
              id.vars = ocols, measure.vars = sedcols, variable.name = "Sediment", value.name = "Prop")
# take out the lines, we just want the points
sedat <- sedat[-which(is.na(sedat$Arviointiruudun.syvyys)),] # now all NA are in prop
sedat[is.na(sedat)] <- 0 # confirmed that NA values can and should be zero

## create summary data for sediment softness based on aggregate info
# create summary category for all different sediments
sedcat <- data.frame(Sediment=unique(sedat$Sediment)[order(as.character(unique(sedat$Sediment)))], Type=NA, Megasoft=NA)
# note that forcing into alphabetic, so easier to control for differences in other data sets
sedcat$Type <- c("Soft",rep("Hard", times=7),"Soft","Hard","Hard","Soft","Soft")
sedcat$Megasoft <- c(rep("Hard", times=8),"Megasoft","Hard","Hard","Megasoft","Hard")
sedat <- merge(sedat, sedcat)
# calculate 
sedsum <- ddply(sedat, .(Kohteen.nimi., Type), summarise, TotProp=sum(Prop))
# let's choose to use pSoft as variable
sedsoft <- sedsum[which(sedsum$Type=="Soft"),]
names(sedsoft)[names(sedsoft)=="TotProp"] <- "pSoft"

sedsum2 <- ddply(sedat, .(Kohteen.nimi., Megasoft), summarise, TotProp=sum(Prop))
# let's choose to use pMegaSoft as variable
sedsoft2 <- sedsum2[which(sedsum2$Megasoft=="Megasoft"),]
names(sedsoft2)[names(sedsoft2)=="TotProp"] <- "pMegaSoft"

sedsoft <- merge(sedsoft, sedsoft2)

## take pSoft and pMegaSoft into main data frame
## NOTE this is now an inner join because default all=F. So now we also drop the line data
dat <- merge(dat, sedsoft[,c("Kohteen.nimi.","pSoft", "pMegaSoft")], sort = F)

## ==============================================================================================
## take out data that don't repeat over rows into their own data set, make into spatial
## =================================================================================================
## select columns we want
colwant <- c("Kohteen.nimi.", "Arviointiruudun.N.koordinaatti..aste.desimaali.", 
             "Arviointiruudun.E.koordinaatti..aste.desimaali.","Kartoituspvm.", "Arviointiruudun.syvyys",
             "pSoft", 'pMegaSoft', 'Secchi.syvyys.m')

basedat <- dat[,colwant]

## take out duplicated
basedat <- basedat[-which(duplicated(basedat)),]

## make date into date
basedat$Kartoituspvm. <- as.POSIXct(basedat$Kartoituspvm., format = "%Y-%m-%d", tz="")

## merge with lutu
basedat <- merge(basedat, lutu, by.x="Kohteen.nimi.", by.y="Plot.ID", sort=F)

## make into spatial
basespat <- SpatialPointsDataFrame(coords = basedat[,c("Arviointiruudun.E.koordinaatti..aste.desimaali.",
                                                       "Arviointiruudun.N.koordinaatti..aste.desimaali.")], 
                                   proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"),
                                   data=basedat )
basevect <- vect(basespat)
basevect <- project(basevect, crs("epsg:3067")) # for calculating distances

## ==============================================================================
## biological data
## ==============================================================================
## select cols we want
biocols <- c("HAVAITTU.LAJI.lajiryhmä.","Peittävyysprosentti")

## select spp we want:
## syvyyden optimiarvot avainlajielle (Fucus sp., Furcellaria lumbricalis, Polysiphonia fucoides, 
##    Rhodomela confervoides sekä Mytilus trossulus 
spwant <- c("Battersia arctica","Fucus", "Fucus radicans", "Furcellaria lumbricalis", "Polysiphonia fucoides",
            "Rhodomela confervoides","Mytilus trossulus", "Ajelehtiva Fucus (elossa)" )

## subset dat accordingly
biodat <- dat[,c(ocols,biocols)]
biodat <- biodat[which(biodat$HAVAITTU.LAJI.lajiryhmä. %in% spwant),]

## combine the fucus sp into one 
biodat$HAVAITTU.LAJI.lajiryhmä.[which(biodat$HAVAITTU.LAJI.lajiryhmä.=="Ajelehtiva Fucus (elossa)")] <- "Fucus"
biodat <- ddply(biodat, .(Kohteen.nimi.,HAVAITTU.LAJI.lajiryhmä.), summarise, Cover=sum(Peittävyysprosentti))
  
## hack empty observations in
# take ajelehtiva out as now all is fucus
spwant <- spwant[-length(spwant)]
notsp <- list()
for (i in 1:length(spwant)) {
  laji <- spwant[i]
  notit <- data.frame(Kohteen.nimi.=unique(dat$Kohteen.nimi.), HAVAITTU.LAJI.lajiryhmä. = laji, Cover=0)
  it <- biodat$Kohteen.nimi.[which(biodat$HAVAITTU.LAJI.lajiryhmä.== laji)]
  notsp[[i]] <- notit[-which(notit$Kohteen.nimi. %in% it),]
}
notsp <- do.call(rbind, notsp)
biodat <- rbind(biodat, notsp)

## bring back syvyys and other desired explanatory variables
biodat <- merge(biodat, unique(dat[,c('Kohteen.nimi.','Arviointiruudun.syvyys', 'pSoft',
                                      'pMegaSoft', 'Secchi.syvyys.m')]))


## ==============================================================================
## put in exposure values from raster
## ==============================================================================
# Extract the raster values at the point locations
extracted_values <- extract(expo, basevect)

basevect$Exposure <- extracted_values$`tahko-exp`

## one point gets zero because it maps onto kivikko, give it its neighbour's value
basevect$Exposure[basevect$Kohteen.nimi.=='W001_2,5_8'] <- basevect$Exposure[basevect$Kohteen.nimi.=='W001_2,7_18']

## add to biodat too
biodat <- merge(biodat, basevect[,c("Kohteen.nimi.", "Exposure")], sort=F)

## =============================================================================
## get shortest distance to beach 
## =============================================================================
distances <- st_distance(st_as_sf(basevect), beach)

# To get a simple vector of distances when there's only one line or polygon in 'vector_sf':
distance_vector <- as.vector(distances)

basevect$Dist <- distance_vector

## add to biodat too
biodat <- merge(biodat, basevect[,c("Kohteen.nimi.", "Dist")], sort=F)

## =======================================================================================
## plotting
## =================================================================================
## create right epsg for leaflet for basevect
baseleaf <- project(basevect, crs("epsg:4326"))
beachleaf <- st_transform(beach, crs("epsg:4326"))

## define colour scheme for rock
rockpal <- colorFactor(palette = c('#80cdc1','#018571','#a6611a'),
                       levels = c("Vedenalainen",'Pinnassa',"Vedenpaallinen"))
lutupal <- colorFactor(palette=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f',
                                 '#ff7f00','#cab2d6','#969696'),
                       levels=c("I1.01 Haurupohjat", "I4.01 Sinisimpukkapohjat", "I02.02 Vitapohjat",
                                "I2.05 Ärviäpohjat", "I04.03 Merirokkopohjat","I1.02 Punaleväpohjat",
                                "I5.03 Yksivuotisten rihmalevien luonnehtimat pohjat ",
                                "I1.03 Monivuotisten rihmalevien luonnehtimat pohjat",
                                "I5.02 Kultajouhi- ja jouhileväpohjat","ei luontotyyppiä"  ))

leaflet() %>%
  addProviderTiles(provider = providers$OpenTopoMap) %>% # doesn't show nature areas as well as open
  addCircles(data=baseleaf, color=~lutupal(LuTu.final),
             fillColor = ~lutupal(LuTu.final), radius=~Secchi.syvyys.m*10,
             label=~Dist) %>%
  addPolygons(data=rockies, weight=3, color='black', fillColor = '#80cdc1', label="Kivikko", fillOpacity = 0.7) %>%
  addCircles(data=rocks, weight=3, color=~rockpal(Kivi), fillColor = ~rockpal(Kivi)) %>%
  addPolylines(data=beachleaf, color="black") %>%
  addScaleBar(position="bottomright", options = scaleBarOptions(imperial=F))

ggplot(biodat, aes(x=Secchi.syvyys.m, y=Arviointiruudun.syvyys, group= HAVAITTU.LAJI.lajiryhmä.,
                   color=Cover, size=Cover)) +
  theme_bw() +
  scale_color_viridis_c() +
  geom_point(alpha=0.7) +
  facet_wrap(~HAVAITTU.LAJI.lajiryhmä., ncol=1, scales = "free_y")

ggplot(biodat, aes(x=pMegaSoft, y=pSoft, group= HAVAITTU.LAJI.lajiryhmä.,
                   color=Cover>0)) +
  theme_bw() +
  geom_point(alpha=0.7) +
  facet_wrap(~HAVAITTU.LAJI.lajiryhmä., ncol=1, scales = "free_y")

## ====================================================================================
## save dat
## ===================================================================================
saveRDS(basevect, "../dat-private/rauma/dat-mod/sukellus-pisteet.rds")
saveRDS(biodat, "../dat-private/rauma/dat-mod/sukellus-bio.rds")
