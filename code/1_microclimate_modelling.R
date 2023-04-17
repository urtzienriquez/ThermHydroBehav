

rm(list=ls())
setwd('./code')

library(NicheMapR)
library(microclima)
library(raster)

#################################
# microclimate modelling 

lon <- 16.057985 # Jihlava, Czechia
lat <- 49.224752 # Jihlava, Czechia
dstart <- "01/08/2009" # 10 year (2009-2018)
dfinish <- "31/07/2018" # 10 year (2009-2018)
minshade <- 0
maxshade <- 90
rainmult <- 1.1
Thcond <- 1.5 # 2.5
SpecHeat <- 870
Density <- 2.56
BulkDensity <- 2.45 # 1.3
windfac <- 1
REFL <- 0.2
cap <- FALSE
SLE <- 0.95
warm <- 0
Usrhyt <- 0.01
clearsky <- FALSE
soilgrids <- 0
spatial <- '/Volumes/igel/spatial/ncep_time'
ERR <- 1.5


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


micro <- micro_ncep(SLE = SLE, warm = warm, soilgrids = soilgrids, dstart = dstart, dfinish = dfinish,
                    Usrhyt = Usrhyt, slope = slope, aspect = aspect, REFL = REFL,
                    hori = hori, minshade = minshade, maxshade = maxshade, rainmult = rainmult,
                    loc = c(lon, lat), runshade = 1, run.gads = 1, snowmodel = 1, runmoist = 1,
                    BulkDensity =  BulkDensity, cap = cap,
                    Density = Density, Thcond = Thcond, SpecHeat = SpecHeat,
                    windfac = windfac, spatial = spatial, ERR = ERR, dem = dem)

soil <- data.frame(micro$soil)
plot(micro$dates, soil$D0cm, type='l')

humid <- data.frame(micro$humid)
plot(micro$dates, humid$RH0cm, type='l')

save(micro, file='../results/micro_Jihlava.Rda', compress="xz")


#############################
# microclimates under various scenarios of climate change
#

sce <- c("cc45", "cc85", "gf45", "gf85", "hg45", "hg85") # scenarios
load("/Volumes/sugandila/worldclim/offsets.RData")

for(s in sce){
  
  tx.dif.local <- offsets[[s]]$temp_offset
  prec.dif.local <- offsets[[s]]$prec_offset
  
  
  juldays12 <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  
  tx.dif.daily <- suppressWarnings(spline(juldays12, tx.dif.local, 
                                          n = 365, xmin = 1, xmax = 365, method = "periodic"))
  prec.dif.daily <- suppressWarnings(spline(juldays12, prec.dif.local, 
                                            n = 365, xmin = 1, xmax = 365, method = "periodic"))
  
  
  warm <- c(tx.dif.daily$y[212:365], rep(tx.dif.daily$y, 8), tx.dif.daily$y[1:213])
  rainoff <- c(prec.dif.daily$y[212:365], rep(prec.dif.daily$y, 8), prec.dif.daily$y[1:213])
  ERR <- 2.5
  
  micro <- micro_ncep(SLE = SLE, soilgrids = soilgrids, dstart = dstart, dfinish = dfinish,
                      Usrhyt = Usrhyt, slope = slope, aspect = aspect, REFL = REFL,
                      hori = hori, minshade = minshade, maxshade = maxshade, rainmult = rainmult,
                      loc = c(lon, lat), runshade = 1, run.gads = 1, snowmodel = 1, runmoist = 1,
                      BulkDensity =  BulkDensity, cap = cap,
                      Density = Density, Thcond = Thcond, SpecHeat = SpecHeat,
                      windfac = windfac, spatial = spatial, ERR = ERR, dem = dem,
                      warm = warm, rainoff = rainoff)
  save(micro, file=paste0('../results/micro_',s,'.RData'), compress="xz")
}


