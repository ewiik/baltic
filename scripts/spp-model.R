## load libraries
library(mgcv)
library(sf)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(terra)
#library(raster)
#library(ggspatial)
#library(leaflet) # list of providertiles https://www.kaggle.com/code/devzohaib/interactive-maps-with-leaflet-in-r


## read dat
basedat <- readRDS("../dat-private/rauma/dat-mod/sukellus-pisteet.rds")
biodat <- readRDS("../dat-private/rauma/dat-mod/sukellus-bio.rds")

## For random effects, create grouping based on first 4 characters of kohde (indicates linja)
biodat$group <- substr(biodat$Kohteen.nimi., 1, 4)
basedat$group <- substr(basedat$Kohteen.nimi., 1, 4)

## make a data frame only version of basedat
basedf <- as.data.frame(basedat)

## make function for inverse of logit
invlogit <- function(x) {exp(x)/(1+exp(x))}

## model what we want
spplist <- unique(biodat$HAVAITTU.LAJI.lajiryhmä.)
spwant <- "Mytilus trossulus"

## select data modelable for spwant
moddat <- biodat[which(biodat$HAVAITTU.LAJI.lajiryhmä.==spwant),]
moddat$group <- as.factor(moddat$group)

## plot correlations between vars
plotcorrs <- F
if(plotcorrs) {
  pairs(basedf[,c("Arviointiruudun.syvyys", "pSoft", "pMegaSoft", "Secchi.syvyys.m",
                  "Exposure", "Dist")])
  
  meltdf <- melt(basedf, id.vars = c("group","Kohteen.nimi."), 
                 measure.vars = c("Arviointiruudun.syvyys", "pSoft", "pMegaSoft", "Secchi.syvyys.m",
                                  "Exposure", "Dist"))
  
  ggplot(meltdf, aes(y=group, x=value, group=variable)) +
    theme_bw() +
    geom_point(alpha=0.5) +
    facet_wrap(~variable, scales = 'free')
}

## Dist correlates strongly with group at the high end, so for safety taking mean per
##    group so as not to violate model assumptions
##    (see Bafumi and Gelman 2006) (tested one thing with mExposure too)
distsum <- ddply(moddat, .(group), summarise, mDist=mean(Dist), mExposure=mean(Exposure))
moddat <- merge(moddat, distsum, sort=F)

## model
## NOTE that secchi is correlated strongly with group, so it can also kind of account for the clumped nature
##    of the response; but how dirty is it as a predictor? how much does it fluctuate in the study area?
##    does it integrate multiyear info, or in any way be sensible to predict plant cover?
## Foregoing it for group grouping, visually inspecting secchi vs arviointisyvyys vs cover doesn't suggest it helps
## NOTE residuals etc better with tweedie than poisson
## NOTE for Fucus, the tweedie made some preds up to 180 for W002...
## NOTE inspecting residuals and stuff, the best could be to try tweedie when lots of zeroes and not very high
##    coverage, and using quasibinomial for the others. recall no AIC for quasi
## NOTE that for plants, we want Dist x syvyys, but not for animals (secchi ~ dist recall)
propmodtw <- gam(Cover ~ te(Arviointiruudun.syvyys, mDist) + s(pSoft) + 
                 s(pMegaSoft) + s(Exposure) + s(group, bs = 're'), data = moddat,
                 method = "P-ML", 
               family = "tw", select = T)

propmodtwless <- gam(Cover ~ te(Arviointiruudun.syvyys, mDist) + s(pSoft) + 
                   s(pMegaSoft) + s(Exposure), data = moddat,method = "P-ML", 
                 family = "tw", select = T)

AIC(propmodtw, propmodtwless)
## just for curiosity, see here https://stats.stackexchange.com/questions/189021/model-selection-for-random-effects-can-unselected-random-effects-be-used-as-fix
## https://stats.stackexchange.com/questions/298078/what-are-the-consequences-of-including-unnecessary-random-effects
## https://stackoverflow.com/questions/24019807/how-to-compare-a-model-with-no-random-effects-to-a-model-with-a-random-effect-us

propmodq <- gam(Cover/100 ~ te(Arviointiruudun.syvyys, mDist, k=3) + s(pSoft, k=4) + 
                   s(pMegaSoft) + s(Exposure) + s(group, bs = 're'), data = moddat,
                method = "P-ML", 
                 family = "quasibinomial", select = T)

propmodqless <- gam(Cover/100 ~ te(Arviointiruudun.syvyys,mDist, k=3) + s(pSoft, k=4) + 
                  s(pMegaSoft,k=4) + s(Exposure,k=4), data = moddat,method = "P-ML", 
                family = "quasibinomial", select = T)

reswant <- propmodq
plot(predict(reswant, type="response"), residuals(reswant, type="pearson"))

mytmodq <- gam(Cover/100 ~ s(Arviointiruudun.syvyys) + s(mDist) + s(pSoft) + 
                  s(pMegaSoft) + s(Exposure, k=4) + s(group, bs = 're'), data = moddat,
                method = "P-ML", 
                family = "quasibinomial", select = T)
mytmodqless <- gam(Cover/100 ~ s(Arviointiruudun.syvyys, k=3) + s(mDist, k=3) + s(pSoft) + 
                 s(pMegaSoft) + s(Exposure, k=3), data = moddat,
               method = "P-ML", 
               family = "quasibinomial", select = T)

propmodq <- mytmodq

## save winning mod
winmod <- propmodq
basepath <- "../dat-private/rauma/dat-mod/"
filename <- "mod-myt.rds"

saveRDS(winmod, paste0(basepath,filename))

## ==================================================================================================
## run predictions for feigned data
## ===================================================================================================
N <- 50 # how many data points do we want to fiction in to get a smooth curve/plane
colwant <- c("Exposure", "pSoft", "pMegaSoft", "mDist", "Arviointiruudun.syvyys")
meanz <- as.data.frame(t(colMeans(moddat[,colwant]))) # get mean values of all vars for keeping fixed

## generate all combs with the te terms
interactions <-F
if (interactions) {
  teterms <- with(moddat, expand.grid(mDist = seq(min(mDist), max(mDist), length = N), 
                                      Arviointiruudun.syvyys = seq(min(Arviointiruudun.syvyys), max(Arviointiruudun.syvyys), 
                                                                   length = N)))
  teterms <- cbind(teterms, meanz[,-which(names(meanz)%in%names(teterms))]) # repeat means over all N^ teterms
  teterms$obj <- 1:nrow(teterms)
  test <- expand.grid(teterms$obj, levels(moddat$group))
  names(test) <- c("obj", "group")
  teterms <- merge(teterms, test, sort=F)
  teterms <- teterms[,-which(names(teterms)=="obj")]
}

## generate dfs with means of all vars
vargen <- c("Exposure", "pSoft", "pMegaSoft", "mDist", "Arviointiruudun.syvyys") 
dflist <- list()
for(i in 1:length(vargen)) {
  colind <- which(names(moddat)==vargen[i])
  inddf <-data.frame(vari = seq(min(moddat[,colind], na.rm = TRUE),
                                          max(moddat[,colind], na.rm = TRUE),
                                          length = N))
  names(inddf) <- vargen[i]
  inddf <- cbind(inddf, meanz[,-which(names(meanz)==vargen[i])])
  
  inddf$obj <- 1:nrow(inddf)
  test <- expand.grid(inddf$obj, levels(moddat$group))
  names(test) <- c("obj", "group")
  inddf <- merge(inddf, test, sort=F)
  inddf <- inddf[,-which(names(inddf)=="obj")]
  
  dflist[[i]] <- inddf
}
names(dflist) <- c(vargen)
#list2env(dflist, envir = globalenv())

## predict with fiction 
## see also mod$family$linkinv to know what transformation is needed for the response family
##    but recall not suitable alone with tweedie
predlist <- list()
for (i in 1:length(dflist)) {
  inddf <- dflist[[i]]
  indcol <- vargen[i]
  
  ## at scale of linear predictor
  spred <- predict(propmodtw, newdata = inddf, type = "link", se.fit = TRUE) 
  # grab what is needed from the different predicts and add to inddf
  sresp <- cbind(inddf, Fitted = spred$fit, se.Fitted = spred$se.fit)
  # add standard errors to estimates for lower and upper bounds
  sresp <- with(sresp, transform(sresp, Fittedplus = Fitted + se.Fitted))
  sresp <- with(sresp, transform(sresp, Fittedminus = Fitted - se.Fitted))
  
  sresp <- with(sresp, transform(sresp, Fitted=exp(Fitted), Fittedplus = exp(Fittedplus),
                                 Fittedminus= exp(Fittedminus)))
  
  sresp$Vari <- rep(indcol)
  sresp$values <- sresp[,indcol]
  
  predlist[[i]] <- sresp
}
names(predlist) <- vargen
#list2env(predlist, envir = globalenv())

predlistq <- list()
for (i in 1:length(dflist)) {
  inddf <- dflist[[i]]
  indcol <- vargen[i]
  
  ## at scale of linear predictor
  spred <- predict(propmodq, newdata = inddf, type = "link", se.fit = TRUE) 
  # grab what is needed from the different predicts and add to inddf
  sresp <- cbind(inddf, Fitted = spred$fit, se.Fitted = spred$se.fit)
  # add standard errors to estimates for lower and upper bounds
  sresp <- with(sresp, transform(sresp, Fittedplus = Fitted + se.Fitted))
  sresp <- with(sresp, transform(sresp, Fittedminus = Fitted - se.Fitted))
  
  sresp <- with(sresp, transform(sresp, Fitted=invlogit(Fitted), Fittedplus = invlogit(Fittedplus),
                                 Fittedminus= invlogit(Fittedminus)))
  
  sresp$Vari <- rep(indcol)
  sresp$values <- sresp[,indcol]
  
  predlistq[[i]] <- sresp
}
names(predlistq) <- paste(vargen, "q", sep="")
#list2env(predlistq, envir = globalenv())


if(interactions) {
  ## tweedie
  tpred <- predict(propmodtw, newdata = teterms, type = "link", se.fit = TRUE)
  # grab what is needed from the different predicts and add to newdata
  tresp <- cbind(teterms, Fitted = tpred$fit, se.Fitted = tpred$se.fit)
  # add standard errors to estimates for lower and upper bounds
  tresp <- with(tresp, transform(tresp, Fittedplus = Fitted + se.Fitted))
  tresp <- with(tresp, transform(tresp, Fittedminus = Fitted - se.Fitted))
  
  tresp <- with(tresp, transform(tresp, Fitted=exp(Fitted), Fittedplus = exp(Fittedplus),
                                 Fittedminus= exp(Fittedminus)))
  
  tresp$Vari <- "te-terms"
  
  ## quasibinom
  qpred <- predict(propmodq, newdata = teterms, type = "link", se.fit = TRUE)
  # grab what is needed from the different predicts and add to newdata
  qresp <- cbind(teterms, Fitted = qpred$fit, se.Fitted = qpred$se.fit)
  # add standard errors to estimates for lower and upper bounds
  qresp <- with(qresp, transform(qresp, Fittedplus = Fitted + se.Fitted))
  qresp <- with(qresp, transform(qresp, Fittedminus = Fitted - se.Fitted))
  
  qresp <- with(qresp, transform(qresp, Fitted=invlogit(Fitted), Fittedplus = invlogit(Fittedplus),
                                 Fittedminus= invlogit(Fittedminus)))
  
  qresp$Vari <- "te-terms"
}

## predict for original data
moddat$Predicted <-predict(propmodtw, newdata=moddat, type="response")
#moddat$Predictedless <- predict(propmodtwless, newdata=moddat, type="response")
moddat$Predictedq <- predict(propmodq, newdata=moddat, type="response")

## =========================================================================================
## plots
## ========================================================================================
sigvars <- c('Arviointiruudun.syvyys','Exposure') # which vars want to plot

alldat <- do.call(bind_rows, predlist)
alldatq <- do.call(bind_rows, predlistq)

## determine demonstrative groups
maxes <- ddply(moddat, .(group), summarize, MaxGroup = max(Predictedq))
first <- maxes$group[which(maxes$MaxGroup == max(maxes$MaxGroup))]
last <- maxes$group[which(maxes$MaxGroup == min(maxes$MaxGroup))]

## choose dat
wantdat <- alldatq

## plot individual predictors
indplot <-
ggplot(droplevels(wantdat[which(wantdat$Vari %in% sigvars & wantdat$group %in% c(first, last)),]), 
       aes(x = values, y = Fitted, group=group)) +
  theme_bw() + 
  #annotate("rect", xmin=phquants[1], xmax=phquants[2], ymin=-Inf, ymax=Inf, alpha = 0.1, fill='gray60') +
  geom_line() +
  geom_ribbon(aes(ymin = Fittedminus, ymax = Fittedplus, group=group, fill=group), 
              alpha = 0.25) +  
  facet_wrap(~Vari, scales='free') +
  xlab('') + ylab("Peittävyys") +
  labs(fill="Havaintolinja") #+
 # theme(legend.position = 'bottom')

## plot interactions
interplot <-
  ggplot(droplevels(qresp[qresp$group %in% c(first,last),]), 
       aes(x=Arviointiruudun.syvyys, y=mDist, fill=Fitted, group=group)) + 
  theme_bw() +
  geom_tile() +
  facet_wrap(~group) +
  scale_fill_viridis_c("Peittävyys") #+
  #geom_point(data=moddat, aes(x=Arviointiruudun.syvyys, y=mDist, fill=NULL, group=NULL))

## plot the moddat
ggplot(moddat, aes(y=Predicted-Cover, x=Cover)) +
  theme_bw() +
  geom_point(alpha=0.6) +
  geom_smooth() +
  geom_point(aes(y=Predictedq*100-Cover, x=Cover), color="red", alpha=0.6) +
  geom_smooth(aes(y=Predictedq*100-Cover, x=Cover), color="red") 
#geom_abline(slope=1, intercept = 0, linetype="dashed")

ggplot(moddat, aes(y=Predicted, x=Predictedq*100)) +
  theme_bw() +
  geom_point(alpha=0.6) +
  geom_abline(slope=1, intercept = 0, linetype="dashed")

ggplot(moddat, aes(x =Cover, y= Predicted)) +
  theme_bw() +
  geom_point(alpha=0.6) +
  geom_point(aes(y=Predictedq*100), color="blue", alpha=0.6) +
  geom_abline(slope=1, intercept = 0)

## save figs want
basepath <- "../dat-private/rauma/figs/"
filename <- "myt-ind.jpg"
fullpath <- paste0(basepath, filename)

ggsave(filename = fullpath, plot = indplot, width = 18, height = 7, units = "cm")

## =================================================================================
## basics on data distributions
## ===============================================================================
biosum <- ddply(biodat, .(HAVAITTU.LAJI.lajiryhmä.), dplyr::summarise, 
                Tot=n(), Prop=length(which(Cover>0)))

bioclump <- ddply(biodat, .(HAVAITTU.LAJI.lajiryhmä., group), dplyr::summarise, 
                  Tot=n(), Prop=length(which(Cover>0)))

sppmelt <- melt(biodat[which(biodat$Cover>0),], measure.vars = c('Cover','Arviointiruudun.syvyys','pSoft',
                                         'pMegaSoft','Exposure','Dist'),
                id.vars = c('HAVAITTU.LAJI.lajiryhmä.'))

sppsum <- ddply(sppmelt, .(HAVAITTU.LAJI.lajiryhmä.,variable), summarize,
                minVal=min(value), maxVal=max(value), medVal=median(value),
                meanVal=mean(value), nObs=n())

## whatever was last moddat will contain all 404 data points so..
surmelt <- melt(moddat, measure.vars = c('Arviointiruudun.syvyys','pSoft',
                                         'pMegaSoft','Exposure','Dist'),
                id.vars = 'Kohteen.nimi.')

sursum <- ddply(surmelt, .(variable), summarize,
      minVal=min(value), maxVal=max(value), medVal=median(value),
      meanVal=mean(value), nObs=n())
sursum[,-1] <- apply(sursum[,-1], 2, round, 1)

surmelt <- melt(sursum, id.vars = 'variable', measure.vars = c('minVal','maxVal', 'medVal', 'meanVal'),
                variable.name = 'whichVal')

sumall <- sursum[,c('variable','minVal','maxVal')]
sumall$HAVAITTU.LAJI.lajiryhmä. <- "Reference"

sumall <- bind_rows(sumall, sppsum[-which(sppsum$variable=="Cover"),c('HAVAITTU.LAJI.lajiryhmä.','variable','minVal','maxVal')])
sumall$HAVAITTU.LAJI.lajiryhmä. <- factor(sumall$HAVAITTU.LAJI.lajiryhmä.,
                                          levels = c("Reference","Fucus","Fucus radicans","Furcellaria lumbricalis",
                                                     "Mytilus trossulus","Polysiphonia fucoides","Rhodomela confervoides" ))
pointsall <- sumall[which(sumall$minVal == sumall$maxVal),]
pointsall <- rbind(pointsall, sumall[which(sumall$maxVal/sumall$minVal < 1.1),])

ggplot(sumall) +
  geom_segment(aes(y=HAVAITTU.LAJI.lajiryhmä., yend=HAVAITTU.LAJI.lajiryhmä., x=minVal, xend=maxVal),
               lineend = 'butt', linewidth=4) +
  geom_point(data=pointsall, aes(y=HAVAITTU.LAJI.lajiryhmä., x=minVal)) +
  theme_bw() +
  facet_grid(~variable, scales='free') +
  ylab("") +xlab("Arvovälit") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

## ================================================================================
## save what want
## ==============================================================================
write.csv(sppsum, "../dat-private/rauma/dat-mod/spp-table.csv", row.names = F)
write.csv(sursum, "../dat-private/rauma/dat-mod/survey-table.csv", row.names = F)
