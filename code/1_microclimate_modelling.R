

rm(list=ls())

library(NicheMapR)
library(microclima)
library(raster)

#################################
# microclimate modelling 

# 10 year (2009-2018)
dstart <- "01/08/2009"
dfinish <- "31/07/2018"

minshade <- 0
maxshade <- 90
Thcond <- 2.5
SpecHeat <- 870
Density <- 2.56
BulkDensity <- 2.5
windfac <- 1
REFL <- 0.2
cap <- FALSE
SLE <- 0.95
warm <- 0
Usrhyt <- 0.01
clearsky <- FALSE
lon <- 16.057985 # Jihlava, Czechia
lat <- 49.224752 # Jihlava, Czechia


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
ERR <- 3.5


micro <- micro_ncep(SLE = SLE, warm = warm, soilgrids = soilgrids, dstart = dstart, dfinish = dfinish,
                    Usrhyt = Usrhyt, slope = slope, aspect = aspect, REFL = REFL,
                    hori = hori, minshade = minshade, maxshade = maxshade,
                    loc = c(lon, lat), runshade = 1, run.gads = 1, snowmodel = 1, runmoist = 1,
                    BulkDensity =  BulkDensity, cap = cap,
                    Density = Density, Thcond = Thcond, SpecHeat = SpecHeat,
                    windfac = windfac, spatial = spatial, ERR = ERR, dem = dem)

soil <- data.frame(micro$soil)
plot(micro$dates, soil$D0cm, type='l')

humid <- data.frame(micro$humid)
plot(micro$dates, humid$RH0cm, type='l')

# save(micro, file='../results/micro_Jihlava.Rda', compress="xz")


#############################
# microclimates under various scenarios of climate change
#

rcp <- c('rcp45', 'rcp85')
gcm <- c('CCSM4', 'GFDL-CM3', 'HadGEM2-CC')



for(r in rcp){
  for(g in gcm){
    
    ## microclimate modelling
    folder.tmax <- paste('/Volumes/Urdintxu/bioclim_layers/bioclim_future_10_min/2070/',r,'/',g,'/tmax/dif',
                         sep='')
    folder.prec <- paste('/Volumes/Urdintxu/bioclim_layers/bioclim_future_10_min/2070/',r,'/',g,'/prec/dif',
                         sep='')
    
    
    tmax.dif.files <- list.files(folder.tmax, pattern='*.tif', full.names=T)
    tmax.dif <- raster::stack(tmax.dif.files)
    prec.dif.files <- list.files(folder.prec, pattern='*.tif', full.names=T)
    prec.dif <- raster::stack(prec.dif.files)
    
    
    tx.dif.local <- c(raster::extract(tmax.dif, cbind(lon,lat)))
    prec.dif.local <- c(raster::extract(prec.dif, cbind(lon,lat)))
    
    
    juldays12 <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
    
    tx.dif.daily <- suppressWarnings(spline(juldays12, tx.dif.local, 
                                            n = 365, xmin = 1, xmax = 365, method = "periodic"))
    prec.dif.daily <- suppressWarnings(spline(juldays12, prec.dif.local, 
                                              n = 365, xmin = 1, xmax = 365, method = "periodic"))
    
    
    warm <- c(tx.dif.daily$y[212:365], rep(tx.dif.daily$y, 8), tx.dif.daily$y[1:213])
    rainoff <- c(prec.dif.daily$y[212:365], rep(prec.dif.daily$y, 8), prec.dif.daily$y[1:213])
    ERR <- 5
    
    micro <- micro_ncep(SLE = SLE, soilgrids = soilgrids, dstart = dstart, dfinish = dfinish,
                        Usrhyt = Usrhyt, slope = slope, aspect = aspect, REFL = REFL,
                        hori = hori, minshade = minshade, maxshade = maxshade,
                        loc = c(lon, lat), runshade = 1, run.gads = 1, snowmodel = 1, runmoist = 1,
                        BulkDensity =  BulkDensity, cap = cap,
                        Density = Density, Thcond = Thcond, SpecHeat = SpecHeat,
                        windfac = windfac, spatial = spatial, ERR = ERR, dem = dem,
                        warm = warm, rainoff = rainoff)
    save(micro, file=paste0('../results/micro_',g,'_',r,'.Rda'), compress="xz")
  }
}


