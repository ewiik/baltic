library(mgcv)
library(sf)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(reshape2)
library(terra)
#library(raster)
#library(ggspatial)
#library(leaflet) # list of providertiles https://www.kaggle.com/code/devzohaib/interactive-maps-with-leaflet-in-r

## lest we forget
## https://stats.stackexchange.com/questions/33327/confidence-interval-for-gam-model

## read dat
basedat <- readRDS("../dat-private/rauma/dat-mod/sukellus-pisteet.rds")

## For random effects, create grouping based on first 4 characters of kohde (indicates linja)
basedat$group <- substr(basedat$Kohteen.nimi., 1, 4)

## make a data frame only version of basedat
basedf <- as.data.frame(basedat)

## function for link inv
## make function for inverse of logit
invlogit <- function(x) {exp(x)/(1+exp(x))}

## summarise the data as appropriate
lutu <- basedf[,-c(which(names(basedf) =='Kohteen.nimi.'))] %>%
  group_by(group,LuTu.final) %>%
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))

## check freqs of types
freqtab <- ddply(lutu, .(LuTu.final), summarize, Tot=n())
#write.csv(freqtab, "../dat-private/rauma/dat-mod/lutu-frekvenssi.csv", row.names=F)

## choose what want to model
llist <- unique(lutu$LuTu.final)
lwant <- "I1.03 Monivuotisten rihmalevien luonnehtimat pohjat"

moddat <- as.data.frame(lutu)
#write.csv(moddat, "../dat-private/rauma/dat-mod/lutu.csv", row.names = F) # for python work
moddat$response <- ifelse(moddat$LuTu.final == lwant, 1,0)

if(lwant=='I4.01 Sinisimpukkapohjat') {
  lmod <- gam(response ~ s(Arviointiruudun.syvyys_mean,k=3) + s(Dist_mean, k=3) + s(pSoft_mean, k=4) + 
                s(pMegaSoft_mean,k=4) + s(Exposure_mean,k=4), data = moddat,method = "REML", 
              family = "binomial", select = T)
} else {lmod <- gam(response ~ te(Arviointiruudun.syvyys_mean, Dist_mean, k=3) + s(pSoft_mean, k=4) + 
                      s(pMegaSoft_mean,k=4) + s(Exposure_mean,k=4), data = moddat,method = "REML", 
                    family = "binomial", select = T)
}


## =================================================================================
## predict
## =============================================================================
N <- 50 # how many data points do we want to fiction in to get a smooth curve/plane
colwant <- c("Exposure_mean", "pSoft_mean", "pMegaSoft_mean", "Dist_mean", "Arviointiruudun.syvyys_mean")
meanz <- as.data.frame(t(colMeans(moddat[,colwant]))) # get mean values of all vars for keeping fixed

## generate all combs with the te terms
interactions <-T
if (interactions) {
  teterms <- with(moddat, expand.grid(Dist_mean = seq(min(Dist_mean), max(Dist_mean), length = N), 
                                      Arviointiruudun.syvyys_mean = seq(min(Arviointiruudun.syvyys_mean), 
                                                                        max(Arviointiruudun.syvyys_mean), 
                                                                   length = N)))
  teterms <- cbind(teterms, meanz[,-which(names(meanz)%in%names(teterms))]) # repeat means over all N^ teterms
  }

## generate dfs with means of all vars
vargen <- c("Exposure_mean", "pSoft_mean", "pMegaSoft_mean", "Dist_mean", "Arviointiruudun.syvyys_mean") 
dflist <- list()
for(i in 1:length(vargen)) {
  colind <- which(names(moddat)==vargen[i])
  inddf <-data.frame(vari = seq(min(moddat[,colind], na.rm = TRUE),
                                max(moddat[,colind], na.rm = TRUE),
                                length = N))
  names(inddf) <- vargen[i]
  inddf <- cbind(inddf, meanz[,-which(names(meanz)==vargen[i])])
  
  dflist[[i]] <- inddf
}
names(dflist) <- c(vargen)

## predict
predlist <- list()
for (i in 1:length(dflist)) {
  inddf <- dflist[[i]]
  indcol <- vargen[i]
  
  ## at scale of linear predictor
  spred <- predict(lmod, newdata = inddf, type = "link", se.fit = TRUE) 
  # grab what is needed from the different predicts and add to inddf
  sresp <- cbind(inddf, Fitted = spred$fit, se.Fitted = spred$se.fit)
  # add standard errors to estimates for lower and upper bounds
  sresp <- with(sresp, transform(sresp, Fittedplus = Fitted + se.Fitted))
  sresp <- with(sresp, transform(sresp, Fittedminus = Fitted - se.Fitted))
  
  sresp <- with(sresp, transform(sresp, Fitted=invlogit(Fitted), Fittedplus = invlogit(Fittedplus),
                                 Fittedminus= invlogit(Fittedminus)))
  
  sresp$Vari <- rep(indcol)
  sresp$values <- sresp[,indcol]
  
  predlist[[i]] <- sresp
}
names(predlist) <- vargen


if(interactions) {
qpred <- predict(lmod, newdata = teterms, type = "link", se.fit = TRUE)
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
moddat$Predicted <-predict(lmod, newdata=moddat, type="response")

## =========================================================================================
## plots
## ========================================================================================
sigvars <- c('pSoft_mean') # which vars want to plot
labvar <- "pSoft"

alldat <- do.call(bind_rows, predlist)

## plot individual predictors
if(length(sigvars)>1) {
  indplot <-
    ggplot(droplevels(alldat[which(alldat$Vari %in% sigvars),]), 
           aes(x = values, y = Fitted)) +
    theme_bw() + 
    #annotate("rect", xmin=phquants[1], xmax=phquants[2], ymin=-Inf, ymax=Inf, alpha = 0.1, fill='gray60') +
    geom_line() +
    geom_ribbon(aes(ymin = Fittedminus, ymax = Fittedplus), 
                alpha = 0.25) +  
    facet_wrap(~Vari, scales='free') +
    xlab('') + ylab("Todennäköisyys") +
    labs(fill="Havaintolinja") } else {
      indplot <-
        ggplot(droplevels(alldat[which(alldat$Vari == sigvars),]), 
             aes(x = values, y = Fitted)) +
        theme_bw() + 
        #annotate("rect", xmin=phquants[1], xmax=phquants[2], ymin=-Inf, ymax=Inf, alpha = 0.1, fill='gray60') +
        geom_line() +
        geom_ribbon(aes(ymin = Fittedminus, ymax = Fittedplus), 
                    alpha = 0.25) +  
        xlab(labvar) + ylab("Todennäköisyys") +
        labs(fill="Havaintolinja") +
        theme(legend.position = "bottom")
    }


## plot interactions
interplot <-
  ggplot(qresp, 
         aes(x=Arviointiruudun.syvyys_mean, y=Dist_mean, fill=Fitted)) + 
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c("Todennäköisyys") +
  xlab("Syvyys (m)") + ylab ("Dist (m)") +
  theme(legend.position = 'bottom') #+
#geom_point(data=moddat, aes(x=Arviointiruudun.syvyys, y=mDist, fill=NULL, group=NULL))

allplots <- grid.arrange(indplot, interplot, ncol=2)
  
## save figs want
basepath <- "../dat-private/rauma/figs/"
filename <- "I4.01-all.jpg"
fullpath <- paste0(basepath, filename)

ggsave(filename = fullpath, plot = indplot, width = 9, height = 7, units = "cm")

## generic data dist
lutumelt <- melt(lutu, measure.vars = c('Arviointiruudun.syvyys_mean','pSoft_mean',
                                        'pMegaSoft_mean','Exposure_mean','Dist_mean'),
                id.vars = c('LuTu.final'))

lutusum <- ddply(lutumelt, .(LuTu.final, variable), summarize,
                minVal=min(value), maxVal=max(value), medVal=median(value),
                meanVal=mean(value), nObs=n())

linesum <- ddply(lutu, .(group), summarize, Total = n())

write.csv(lutusum, "../dat-private/rauma/dat-mod/lutu-table.csv", row.names = F)

