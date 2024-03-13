## prepare data for exposure analysis
## data hourly mean and direction from https://www.ilmatieteenlaitos.fi/havaintojen-lataus
## target last 3 yrs minus jan-march and so time interval March 2021-Dec 2023
## downloaded with local time not utc

## read data
dat1 <- read.csv("../dat-private/rauma/tahkoluoto.csv", stringsAsFactors = F) # WGS84 61.63	21.38
# Asema sijaitsee Tahkoluodon saaren länsikärjessä, avokalliolla. Kallion notkoissa kasvaa heinää ja matalaa pensaikkoa. Tahkoluodon ja sen sataman vaikutus tuntuu sektorissa 45° - 120° ja Tahkoluodon voimalarakennukset sijaitsevat idässä 1,5 km päässä. Koko itäpuolen sektorissa on runsasta saaristoa ja manner n. 10 km päässä. Länsisektorissa on avomerta tai joitakin pieniä saaria. Porin kaupunkikeskusta on kaakossa noin 28 km etäisyydellä.
dat2 <- read.csv("../dat-private/rauma/kylmapihlaja.csv", stringsAsFactors = F) # WGS84 61.14	21.3
# Asema sijaitsee Selkämeren eteläosassa Rauman edustalla olevassa Kylmäpihlajan saaressa aivan ulkosaariston reunalla. Lähimpiin saariin on matkaa noin kilometri, mantereelle 9 km ja Rauman kaupunkikeskustaan 10 km. Sääasema on saaren pohjoisosassa. Saari on suhteellisen tasainen sääaseman ollessa sen korkeimmalla alueella n. 4m mpy. Saari on pääosin avokalliota, mutta erityisesti saaren kaakkoisosassa kasvaa matalahkoa pensaikkoa ja jokunen puu. Tuulta mitataan majakan huipun tutkatornista omassa tuulimastossa 38m metrin korkeudelta maasta ja 43m merestä. Saari on paikkana hyvä tuulenmittaukselle ulkosaariston reunalla, mutta koska mittaus tehdään korkean majakan huipulta, ovat tuulet selvästi liian kovia. Lämpötilaoloiltaan asema on edustava.

## make into list
datz <- list(dat1, dat2)

## take out remaining jan march
datz <- lapply(datz, function(x) {x <- x[-which(x$Kuukausi %in% c(1:3)),]})

## name columns so they get read by python script and are easy to access
datz <- lapply(datz, function(x) {
names(x)[names(x) %in% c("Päivä","Aika..Paikallinen.aika.","Keskituulen.nopeus..m.s.",
                             "Tuulen.suunnan.keskiarvo....")] <- 
  c("Paiva",'Aika','Tuulen nopeus (m/s)','Tuulen suunta (deg)');
return(x)
})

## check basics (empty data, completeness of data)
lapply(datz, nrow) # more in tahko so there must be some missings in kylma
lapply(datz, summary) # no NA
# but ha, noticed when trying to run the script that wind deg has dashes!! failed to notice class character in summary for tahko
names(datz) <- c('dat1','dat2')
list2env(datz, envir = globalenv())

dat1$`Tuulen suunta (deg)`[which(dat1$`Tuulen suunta (deg)` == "-")] <- NA
dat1 <- na.omit(dat1)
dat1$`Tuulen suunta (deg)` <- as.numeric(dat1$`Tuulen suunta (deg)`)

## save csvs
write.csv(dat1, '../dat-private/rauma/dat-mod/tahko-clean.csv',
          row.names = F)
write.csv(dat2, '../dat-private/rauma/dat-mod/kylma-clean.csv',
          row.names = F)
