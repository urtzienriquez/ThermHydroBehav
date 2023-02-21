


####
# TO DO:
# add strategies: fixed depths

setwd('~/Dropbox/1_papers/CAS/I_alpestris/0_living_underground')
rm(list=ls())

library(NicheMapR)
library(microclima)
library(raster)

#################################
# Check microclimate modelling 

# # 1 year (2010)
# dstart <- "01/08/2010"
# dfinish <- "31/07/2011"

# 10 year (2010-2019)
dstart <- "01/08/2009"
dfinish <- "31/07/2018"

minshade <- 0
maxshade <- 90
DEP <- c(0, 2,  3,  5,  8,  12,  20,  30,  50,  100)
Thcond <- 2.5
SpecHeat <- 870
Density <- 2.56
BulkDensity <- 1.3
windfac <- 1
REFL <- 0.2
cap <- FALSE
SLE <- 0.95
warm <- 0
Usrhyt <- 0.01
clearsky <- FALSE
lon <- 16.057985
lat <- 49.224752


dem <- microclima::get_dem(r = NA, lat = lat, lon = lon, resolution = 30, zmin = -20, xdims = 100, ydims = 100)

elev <- raster::extract(dem, cbind(lon, lat))[1]        
xy <- data.frame(x = lon, y = lat)
sp::coordinates(xy) = ~x + y
sp::proj4string(xy) = "+init=epsg:4326"
xy <- as.data.frame(sp::spTransform(xy, raster::crs(dem)))
slope <- raster::terrain(dem, unit = "degrees")
slope <- raster::extract(slope, xy)
aspect <- raster::terrain(dem, opt = "aspect", unit = "degrees")
aspect <- raster::extract(aspect, xy)
ha36 <- 0
for (i in 0:35) {
  har <- microclima::horizonangle(dem, i * 10, raster::res(dem)[1])
  ha36[i + 1] <- atan(raster::extract(har, xy)) * (180/pi)
}
hori <- spline(x = ha36, n = 24, method =  'periodic')$y
hori[hori < 0] <- 0
hori[hori > 90] <- 90

soilgrids <- 0
spatial <- '/Volumes/urdintxu/ncep_time'
ERR <- 1.5


micro <- micro_ncep(SLE = SLE, warm = warm, soilgrids = soilgrids, dstart = dstart, dfinish = dfinish,
                    Usrhyt = Usrhyt, slope = slope, aspect = aspect, REFL = REFL,
                    hori = hori, minshade = minshade, maxshade = maxshade,
                    loc = c(lon, lat), runshade = 1, run.gads = 1, snowmodel = 1, runmoist = 1,
                    BulkDensity =  BulkDensity, cap = cap,
                    Density = Density, Thcond = Thcond, SpecHeat = SpecHeat,
                    windfac = windfac, spatial = spatial, ERR = ERR, dem = dem, DEP=DEP)

metout <- data.frame(micro$metout)
micro_time_finish <- Sys.time()




##########################
#
# Ectotherm

tpref <- read.table('~/Dropbox/1_papers/CAS/I_alpestris/data/doi_10.5061_dryad.860ks__v1/Gvozdik_Kristin_TP.txt', 
                    header=T, sep='\t')
str(tpref)

m_tpref <- mean(tpref$MeanTp[tpref$Treatment == 'Food'])
lbt <- mean(tpref$LBTp[tpref$Treatment == 'Food'])
ubt <- mean(tpref$UBTp[tpref$Treatment == 'Food'])

# # at fixed soil depths
# ecto <- ectotherm(mindepth = 2, maxdepth=2, diurn = 0, nocturn = 0, crepus = 0)


behav <- c(1,2,3) # 1 = cool shelters; 2 = warm shelters; 3 = pasive
bwater <- c(0,1) # following moist shelters

maxshades <- micro$maxshade
minshades <- micro$maxshade # change this depending on sun/shade

# first thermoregulating "freely"
for(i in 1:length(behav)){
  for(j in 1:length(bwater)){
    ecto <- ectotherm(Ww_g =3.5, shape = 3, burrow=1,
                      T_F_min = 4, T_F_max = ubt, T_pref = m_tpref,
                      T_B_min = 4, T_RB_min = 4,
                      CT_max = 36, CT_min = -2,
                      diurn = 0, nocturn = 1, crepus = 1, shade_seek = 0, maxdepth = 10,
                      pct_wet = 100, burrowtmp = behav[i], burrowwtr = bwater[j],
                      maxshade=maxshades, minshade=minshades, shdburrow = 2)
    # shdburrow = 2 to simulate conditions in the shade
    assign(paste('ecto_b',behav[i],'w',bwater[j],sep=''), ecto) 
  }
}

# then thermoregulating to a maximum of 50 cm
for(i in 1:length(behav)){
  for(j in 1:length(bwater)){
    ecto <- ectotherm(Ww_g =3.5, shape = 3, burrow=1,
                      T_F_min = 4, T_F_max = ubt, T_pref = m_tpref,
                      T_B_min = 4, T_RB_min = 4,
                      CT_max = 36, CT_min = -2,
                      diurn = 0, nocturn = 1, crepus = 1, shade_seek = 0, maxdepth = 9,
                      pct_wet = 100, burrowtmp = behav[i], burrowwtr = bwater[j],
                      maxshade=maxshades, minshade=minshades, shdburrow = 2)
    assign(paste('ecto_b',behav[i],'w',bwater[j],'_50cm',sep=''), ecto) 
  }
}


environ_b1w0 <- data.frame(ecto_b1w0$environ)
environ_b1w1 <- data.frame(ecto_b1w1$environ)
environ_b2w0 <- data.frame(ecto_b2w0$environ)
environ_b2w1 <- data.frame(ecto_b2w1$environ)
environ_b3w0 <- data.frame(ecto_b3w0$environ)
environ_b3w1 <- data.frame(ecto_b3w1$environ)
environ_b1w0_50cm <- data.frame(ecto_b1w0_50cm$environ)
environ_b1w1_50cm <- data.frame(ecto_b1w1_50cm$environ)
environ_b2w0_50cm <- data.frame(ecto_b2w0_50cm$environ)
environ_b2w1_50cm <- data.frame(ecto_b2w1_50cm$environ)
environ_b3w0_50cm <- data.frame(ecto_b3w0_50cm$environ)
environ_b3w1_50cm <- data.frame(ecto_b3w1_50cm$environ)


environ_b1w0 <- cbind(environ_b1w0,metout$SOLR) # add solar radiation for activity window plots
colnames(environ_b1w0)[ncol(environ_b1w0)] <- "Solar"
environ_b1w1 <- cbind(environ_b1w1,metout$SOLR) # add solar radiation for activity window plots
colnames(environ_b1w1)[ncol(environ_b1w1)] <- "Solar"
environ_b2w0 <- cbind(environ_b2w0,metout$SOLR) # add solar radiation for activity window plots
colnames(environ_b2w0)[ncol(environ_b2w0)] <- "Solar"
environ_b2w1 <- cbind(environ_b2w1,metout$SOLR) # add solar radiation for activity window plots
colnames(environ_b2w1)[ncol(environ_b2w1)] <- "Solar"
environ_b3w0 <- cbind(environ_b3w0,metout$SOLR) # add solar radiation for activity window plots
colnames(environ_b3w0)[ncol(environ_b3w0)] <- "Solar"
environ_b3w1 <- cbind(environ_b3w1,metout$SOLR) # add solar radiation for activity window plots
colnames(environ_b3w1)[ncol(environ_b3w1)] <- "Solar"

environ_b1w0_50cm <- cbind(environ_b1w0_50cm,metout$SOLR) # add solar radiation for activity window plots
colnames(environ_b1w0_50cm)[ncol(environ_b1w0_50cm)] <- "Solar"
environ_b1w1_50cm <- cbind(environ_b1w1_50cm,metout$SOLR) # add solar radiation for activity window plots
colnames(environ_b1w1_50cm)[ncol(environ_b1w1_50cm)] <- "Solar"
environ_b2w0_50cm <- cbind(environ_b2w0_50cm,metout$SOLR) # add solar radiation for activity window plots
colnames(environ_b2w0_50cm)[ncol(environ_b2w0_50cm)] <- "Solar"
environ_b2w1_50cm <- cbind(environ_b2w1_50cm,metout$SOLR) # add solar radiation for activity window plots
colnames(environ_b2w1_50cm)[ncol(environ_b2w1_50cm)] <- "Solar"
environ_b3w0_50cm <- cbind(environ_b3w0_50cm,metout$SOLR) # add solar radiation for activity window plots
colnames(environ_b3w0_50cm)[ncol(environ_b3w0_50cm)] <- "Solar"
environ_b3w1_50cm <- cbind(environ_b3w1_50cm,metout$SOLR) # add solar radiation for activity window plots
colnames(environ_b3w1_50cm)[ncol(environ_b3w1_50cm)] <- "Solar"

environs <- list(b1w0 = environ_b1w0, b2w0 = environ_b2w0, b3w0 = environ_b3w0,
                 b1w1 = environ_b1w1, b2w1 = environ_b2w1, b3w1 = environ_b3w1)

environs_50cm <- list(b1w0_50cm = environ_b1w0_50cm, b2w0_50cm = environ_b2w0_50cm, b3w0_50cm = environ_b3w0_50cm,
                      b1w1_50cm = environ_b1w1_50cm, b2w1_50cm = environ_b2w1_50cm, b3w1_50cm = environ_b3w1_50cm)


######################
## metabolic rates

mrs <- read.table('~/Dropbox/1_papers/CAS/I_alpestris/data/doi_10.5061_dryad.860ks__v1/Gvozdik_Kristin_AS.txt', 
                  header=T, sep='\t')
str(mrs)
colnames(mrs) <- c('temp', 'sex', 'mass', 'smr', 'mmr', 'scope', 'fscope')


# predicting smr from body temperature
smr_model <- lm(smr ~ poly(temp, 2), data=mrs)
smr_model2 <- nls(smr ~ exp(a + b * temp), data = mrs, start = list(a = 0, b = 0))
new <- data.frame(temp=seq(10,25))
new$predicted <- predict(smr_model, newdata=new)
new$predicted2 <- predict(smr_model2, newdata=new)

plot(smr ~ temp, data=mrs, pch=19, cex=.5,
     xlab='Temperature (ºC)', 
     ylab=expression(paste('SMR (in mL ', O[2], ' ', h^-1, ')')))
# lines(new$temp, new$predicted)
lines(new$temp, new$predicted2)
# points(cbind(new$temp, new$predicted), pch=19, col='red')
# points(cbind(new$temp, new$predicted2), pch=19)


# predicting mmr from body temperature ?
mmr_model <- lm(mmr ~ poly(temp, 2), data=mrs)
mmr_model2 <- nls(mmr ~ exp(a + b * temp), data = mrs, start = list(a = 0, b = 0))
new <- data.frame(temp=seq(10,25))
new$predicted <- predict(mmr_model, newdata=new)
new$predicted2 <- predict(mmr_model2, newdata=new)

plot(mmr ~ temp, data=mrs)
lines(new$temp, new$predicted)
lines(new$temp, new$predicted2)
points(cbind(new$temp, new$predicted), pch=19, col='red')
points(cbind(new$temp, new$predicted2), pch=19)


# thermal sensitivity of MRs

new <- data.frame(temp=seq(0,36))
new$predicted_mmr <- predict(mmr_model, newdata=new)
new$predicted_mmr2 <- predict(mmr_model2, newdata=new)
new$predicted_smr <- predict(smr_model, newdata=new)
new$predicted_smr2 <- predict(smr_model2, newdata=new)
new$predicted_fscope <- new$predicted_mmr / new$predicted_smr
new$predicted_fscope2 <- new$predicted_mmr2 / new$predicted_smr2

op <- par()
par(mfrow=c(1,2))
with(new, plot(predicted_mmr ~ temp ,type = "l", ylim=c(0, 1.2), ylab='oxygen consumption'))
with(new, lines(predicted_mmr2 ~ temp, lty=2))
with(new, lines(predicted_smr ~ temp, col='red'))
with(new, lines(predicted_smr2 ~ temp, col='red', lty=2))


with(new, plot(predicted_fscope ~ temp ,type = "l", ylab='factorial scope'))
with(new, lines(predicted_fscope2 ~ temp , lty=2))
par(op)



########################
# compute metabolic rates from estimated Tbs
#
# also metabolic rates with seasonal acclimation (70%: 30% reduction)



for(i in 1:length(environs)){
  tcs <- data.frame(temp=environs[[i]]$TC)
  environs[[i]]$MMR <- predict(mmr_model2, newdata=tcs)
  environs[[i]]$SMR <- predict(smr_model2, newdata=tcs)
  environs[[i]]$MMR_acc <- predict(mmr_model2, newdata=tcs) * 0.7
  environs[[i]]$SMR_acc <- predict(smr_model2, newdata=tcs) * 0.7
  
  # and 50 cm
  tcs <- data.frame(temp=environs_50cm[[i]]$TC)
  environs_50cm[[i]]$MMR <- predict(mmr_model2, newdata=tcs)
  environs_50cm[[i]]$SMR <- predict(smr_model2, newdata=tcs)
  environs_50cm[[i]]$MMR_acc <- predict(mmr_model2, newdata=tcs) * 0.7
  environs_50cm[[i]]$SMR_acc <- predict(smr_model2, newdata=tcs) * 0.7
}




# save(micro, file='microclimate.RData')
# save(environs, file='environs_shade.RData')
# save(environs_50cm, file='environs_shade_50cm.RData')




####################################################################################
#           CLIMATE CHANGE
#

rcp <- c('rcp45', 'rcp85')
gcm <- c('CCSM4', 'GFDL-CM3', 'HadGEM2-CC')


# empty lists to save results
environs_cc <- list()
environs_cc_50cm <- list()

for(r in rcp){
  
  for(g in gcm){
    
    ## microclimate modelling
    folder.tmax <- paste('/Volumes/Urdintxu/bioclim_layers/bioclim_future_10_min/2070/',r,'/',g,'/tmax/dif',
                         sep='')
    folder.prec <- paste('/Volumes/Urdintxu/bioclim_layers/bioclim_future_10_min/2070/',r,'/',g,'/prec/dif',
                         sep='')
    
    
    tmax.dif.files <- list.files(folder.tmax, pattern='*.tif', full.names=T)
    tmax.dif <- stack(tmax.dif.files)
    prec.dif.files <- list.files(folder.prec, pattern='*.tif', full.names=T)
    prec.dif <- stack(prec.dif.files)
    
    
    tx.dif.local <- c(raster::extract(tmax.dif, cbind(lon,lat)))
    prec.dif.local <- c(raster::extract(prec.dif, cbind(lon,lat)))
    
    
    juldays12 <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
    
    tx.dif.daily <- suppressWarnings(spline(juldays12, tx.dif.local, 
                                            n = 365, xmin = 1, xmax = 365, method = "periodic"))
    prec.dif.daily <- suppressWarnings(spline(juldays12, prec.dif.local, 
                                              n = 365, xmin = 1, xmax = 365, method = "periodic"))
    
    
    warm <- c(tx.dif.daily$y[212:365], rep(tx.dif.daily$y, 8), tx.dif.daily$y[1:213])
    rainoff <- c(prec.dif.daily$y[212:365], rep(prec.dif.daily$y, 8), prec.dif.daily$y[1:213])
    ERR <- 3
    
    micro <- micro_ncep(SLE = SLE, soilgrids = soilgrids, dstart = dstart, dfinish = dfinish,
                        Usrhyt = Usrhyt, slope = slope, aspect = aspect, REFL = REFL,
                        hori = hori, minshade = minshade, maxshade = maxshade,
                        loc = c(lon, lat), runshade = 1, run.gads = 1, snowmodel = 1, runmoist = 1,
                        BulkDensity =  BulkDensity, cap = cap,
                        Density = Density, Thcond = Thcond, SpecHeat = SpecHeat,
                        windfac = windfac, spatial = spatial, ERR = ERR, dem = dem, DEP=DEP,
                        warm = warm, rainoff = rainoff)
    metout <- data.frame(micro$metout)
    
    
    
    ## ectotherm models
    
    
    behav <- c(1,2,3) # 1 = cool shelters; 2 = warm shelters; 3 = pasive
    bwater <- c(0,1) # following moist shelters
    
    # first thermoregulating "freely"
    for(i in 1:length(behav)){
      for(j in 1:length(bwater)){
        ecto <- ectotherm(Ww_g =3.5, shape = 3, burrow=1,
                          T_F_min = 4, T_F_max = ubt, T_pref = m_tpref,
                          T_B_min = 4, T_RB_min = 4,
                          CT_max = 36, CT_min = -2,
                          diurn = 0, nocturn = 1, crepus = 1, shade_seek = 0, maxdepth = 10,
                          pct_wet = 100, burrowtmp = behav[i], burrowwtr = bwater[j],
                          maxshade=maxshades, minshade=minshades, shdburrow = 2)
        
        environ <- data.frame(ecto$environ)
        
        
        environ <- cbind(environ,metout$SOLR) # add solar radiation for activity window plots
        colnames(environ)[ncol(environ)] <- "Solar"
        
        name_model <- paste('b',behav[i],'w',bwater[j],'_',r,'_',g,sep='')
        environs_cc[[name_model]] <-  environ
        
      }
    }
    
    # then thermoregulating to a maximum of 50 cm
    for(i in 1:length(behav)){
      for(j in 1:length(bwater)){
        ecto <- ectotherm(Ww_g =3.5, shape = 3, burrow=1,
                          T_F_min = 4, T_F_max = ubt, T_pref = m_tpref,
                          T_B_min = 4, T_RB_min = 4,
                          CT_max = 36, CT_min = -2,
                          diurn = 0, nocturn = 1, crepus = 1, shade_seek = 0, maxdepth = 9,
                          pct_wet = 100, burrowtmp = behav[i], burrowwtr = bwater[j],
                          maxshade=maxshades, minshade=minshades, shdburrow = 2)
        
        environ <- data.frame(ecto$environ)
        
        
        environ <- cbind(environ,metout$SOLR) # add solar radiation for activity window plots
        colnames(environ)[ncol(environ)] <- "Solar"
        
        name_model <- paste('b',behav[i],'w',bwater[j],'_',r,'_',g,'_50cm',sep='')
        environs_cc_50cm[[name_model]] <-  environ
        
      }
    }
    
  }
  
}



########################
# compute metabolic rates from estimated Tbs
#
# also metabolic rates with seasonal acclimation (70%: 30% reduction)



for(i in 1:length(environs_cc)){
  tcs <- data.frame(temp=environs_cc[[i]]$TC)
  environs_cc[[i]]$MMR <- predict(mmr_model2, newdata=tcs)
  environs_cc[[i]]$SMR <- predict(smr_model2, newdata=tcs)
  environs_cc[[i]]$MMR_acc <- predict(mmr_model2, newdata=tcs) * 0.7
  environs_cc[[i]]$SMR_acc <- predict(smr_model2, newdata=tcs) * 0.7
  
  # and 50 cm
  tcs <- data.frame(temp=environs_cc_50cm[[i]]$TC)
  environs_cc_50cm[[i]]$MMR <- predict(mmr_model2, newdata=tcs)
  environs_cc_50cm[[i]]$SMR <- predict(smr_model2, newdata=tcs)
  environs_cc_50cm[[i]]$MMR_acc <- predict(mmr_model2, newdata=tcs) * 0.7
  environs_cc_50cm[[i]]$SMR_acc <- predict(smr_model2, newdata=tcs) * 0.7
}


# save(environs_cc, file='environs_cc_shade.RData')
# save(environs_cc_50cm, file='environs_cc_shade_50cm.RData')

