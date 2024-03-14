## load libraries
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
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


## =======================================================
## Create sediment index
## ============================================================
## select columns we need
sedcols <- c("Kallio", "Lohkare....3000.mm", "Lohkare.1200.3000.mm", "Lohkare.600.1200.mm",
             "Iso.kivi.100.600.mm", "Pieni.kivi.60.100.mm", "Sora.2.0.60.mm", "Hiekka.0.06.2.0.mm",
             "Siltti.0.002.0.06", "Muta..0.002.mm","Konkreetiot","Keinotekoinen.alusta", "Puun.rungot.oksat")
ocols <- c("Kohteen.nimi.","Arviointiruudun.syvyys")
## fIXME: add siltti ja muta megapehmea, ana have as another predictor in addition to psoft.

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

## take pSoft into main data frame
## NOTE this is now an inner join because default all=F. So now we also drop the line data
dat <- merge(dat, sedsoft[,c("Kohteen.nimi.","pSoft", "pMegaSoft")], sort = F)

## ==============================================================================================
## take out data that don't repeat over rows into their own data set, make into spatial
## =================================================================================================
## select columns we want
colwant <- c("Kohteen.nimi.", "Arviointiruudun.N.koordinaatti..aste.desimaali.", 
             "Arviointiruudun.E.koordinaatti..aste.desimaali.","Kartoituspvm.", "Arviointiruudun.syvyys",
             "pSoft", 'pMegaSoft')

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

## define colour scheme for rock
rockpal <- colorFactor(palette = c('#80cdc1','#018571','#a6611a'),
                       levels = c("Vedenalainen",'Pinnassa',"Vedenpaallinen"))

leaflet() %>%
  addProviderTiles(provider = providers$OpenTopoMap) %>% # doesn't show nature areas as well as open
  addCircles(data=basespat, label = ~Kohteen.nimi.) %>%
  addPolygons(data=rockies, weight=3, color='black', fillColor = '#80cdc1', label="Kivikko", fillOpacity = 0.7) %>%
  addCircles(data=rocks, weight=3, color=~rockpal(Kivi), fillColor = ~rockpal(Kivi)) %>%
addScaleBar(position="bottomright", options = scaleBarOptions(imperial=F))
  #addRasterImage(searast, colors = seapal, opacity = 0.6) 

## ==============================================================================
## biological data
## ==============================================================================
## select cols we want
biocols <- c("HAVAITTU.LAJI.lajiryhmä.","Peittävyysprosentti","Lukumäärä", "Määrän.yksikkö",
             "Lajin.keskimääräinen.korkeus", "Biomassa")

## select spp we want:
## syvyyden optimiarvot avainlajielle (Fucus sp., Furcellaria lumbricalis, Polysiphonia fucoides, 
##    Rhodomela confervoides sekä Mytilus trossulus 
spwant <- c("Fucus", "Fucus radicans", "Furcellaria lumbricalis", "Polysiphonia fucoides",
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
biodat <- merge(biodat, unique(dat[,c('Kohteen.nimi.','Arviointiruudun.syvyys', 'pSoft','pMegaSoft')]))

ggplot(biodat, aes(x=pMegaSoft, y=Cover, group= HAVAITTU.LAJI.lajiryhmä.,
                   color=Cover>0)) +
  theme_bw() +
  geom_point(alpha=0.7) +
  facet_wrap(~HAVAITTU.LAJI.lajiryhmä., ncol=1, scales = "free_y")

ggplot(biodat, aes(x=pMegaSoft, y=pSoft, group= HAVAITTU.LAJI.lajiryhmä.,
                   color=Cover>0)) +
  theme_bw() +
  geom_point(alpha=0.7) +
  facet_wrap(~HAVAITTU.LAJI.lajiryhmä., ncol=1, scales = "free_y")

## ==============================================================================
## put in exposure values from raster
## ==============================================================================
## change crs of basevect to match
baserast <- project(basevect, "epsg:3067")

# Extract the raster values at the point locations
extracted_values <- extract(expo, baserast)

basevect$Exposure <- extracted_values$`tahko-exp`
