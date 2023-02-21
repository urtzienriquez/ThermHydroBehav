

library(NicheMapR)
library(microclima)
library(raster)


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
lon <- 16.056838 # Institute of Vertebrate Biology, Studenec, Czechia
lat <- 49.224799 # Institute of Vertebrate Biology, Studenec, Czechia


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



# read logged data
tsun1_0cm <- read.table('./data/sun_temp_1_0.txt', header=F, sep=',')
tsun1_0cm <- data.frame(datetime = as.POSIXct(strptime(tsun1_0cm[,1], "%d.%m.%y %H:%M:%S")),
                        temp = as.numeric(paste(tsun1_0cm[,3],tsun1_0cm[,4], sep='.')))
with(tsun1_0cm, plot(datetime, temp, type='l'))
