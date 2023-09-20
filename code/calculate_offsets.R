
setwd("/Volumes/sugandila/worldclim")

library(raster)

b_temp_files <- list.files('./tmax_2-5m_bil', pattern="*.bil", full.names=T)
b_prec_files <- list.files('./prec_2-5m_bil', pattern="*.bil", full.names=T)

baseline_temp <- stack(b_temp_files) # remember this is x10
baseline_prec <- stack(b_prec_files)

loc <- cbind(16.057985, 49.224752) # Jihlava, Czechia
base_temp_loc <- extract(baseline_temp, loc)
base_prec_loc <- extract(baseline_prec, loc)

# plot(baseline_temp)
# plot(baseline_prec)

sce <- c("cc45", "cc85", "gf45", "gf85", "hg45", "hg85") # scenarios
offsets <- list() # to store results

for(i in sce){
  temp_files <- list.files(paste0("./", i, "tx70"),
                           pattern="*.tif", full.names=T)
  prec_files <- list.files(paste0("./", i, "pr70"),
                           pattern="*.tif", full.names=T)
  
  temp <- stack(temp_files) # remember this is x10
  prec <- stack(prec_files)
  
  temp_loc <- extract(temp, loc)
  prec_loc <- extract(prec, loc)
  temp_offset <- (temp_loc - base_temp_loc) / 10
  prec_offset <- (prec_loc - base_prec_loc)
  offsets[[i]] <- list(temp_offset = temp_offset,
                       prec_offset = prec_offset)
}

save(offsets, file = "offsets.RData")
