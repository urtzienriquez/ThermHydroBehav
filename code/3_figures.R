
setwd('./code')
rm(list=ls())

library(ggplot2)
library(cowplot)
library(tidyverse)
source('./aux_functions.R')

# load microclimate model results (only needed to extract dates)
load('../results/micro_Jihlava.Rda')

# load ectotherm model results for the present and subset winter and summer
# at full sun
ec_cur_sun_200cm <- load_ectos(path = "../results",
                               scenario = "current",
                               movement = "200cm",
                               shading = "sun",
                               micro = micro)
ec_cur_sun_50cm <- load_ectos(path = "../results",
                              scenario = "current",
                              movement = "50cm",
                              shading = "sun",
                              micro = micro)

ec_cur_sun_200cm_summer <- subset_env(ec_cur_sun_200cm, season = "summer")
ec_cur_sun_200cm_winter <- subset_env(ec_cur_sun_200cm, season = "winter")
ec_cur_sun_50cm_summer <- subset_env(ec_cur_sun_50cm, season = "summer")
ec_cur_sun_50cm_winter <- subset_env(ec_cur_sun_50cm, season = "winter")

# at deep shade
ec_cur_shade_200cm <- load_ectos(path = "../results",
                               scenario = "current",
                               movement = "200cm",
                               shading = "shade",
                               micro = micro)
ec_cur_shade_50cm <- load_ectos(path = "../results",
                              scenario = "current",
                              movement = "50cm",
                              shading = "shade",
                              micro = micro)

ec_cur_shade_200cm_summer <- subset_env(ec_cur_shade_200cm, season = "summer")
ec_cur_shade_200cm_winter <- subset_env(ec_cur_shade_200cm, season = "winter")
ec_cur_shade_50cm_summer <- subset_env(ec_cur_shade_50cm, season = "summer")
ec_cur_shade_50cm_winter <- subset_env(ec_cur_shade_50cm, season = "winter")


# DEPTH PLOTS: 

# Selected depths given different behaviors (without movement restriction)
# at full sun

list_ec_cur_sun_200cm_winter <- list(ec_cur_sun_200cm_winter)
#list_ec_cur_sun_50cm_winter <- list(ec_cur_sun_50cm_winter)
list_ec_cur_sun_200cm_summer <- list(ec_cur_sun_200cm_summer)
#list_ec_cur_sun_50cm_summer <- list(ec_cur_sun_50cm_summer)
list_cc_sun_200cm_winter <- list()
#list_cc_sun_50cm_winter <- list()
list_cc_sun_200cm_summer <- list()
#list_cc_sun_50cm_summer <- list()
scen <- c('cc45', 'cc85', 'gf45', 'gf85', 'hg45', 'hg85')
for(s in scen){
  ec_cc_sun_200cm <- load_ectos(path = "../results",
                                scenario = s,
                                movement = "200cm",
                                shading = "sun",
                                micro = micro)
  # ec_cc_sun_50cm <- load_ectos(path = "../results",
  #                              scenario = s,
  #                              movement = "50cm",
  #                              shading = "sun",
  #                              micro = micro)
  ec_cc_sun_200cm$model <- as.factor(substr(ec_cc_sun_200cm$model, 1, nchar(as.character(ec_cc_sun_200cm$model))-5)) # set the same name to plot them stacked
  #ec_cc_sun_50cm$model <- as.factor(substr(ec_cc_sun_50cm$model, 1, nchar(as.character(ec_cc_sun_50cm$model))-5)) # set the same name to plot them stacked
  ec_cc_sun_200cm_summer <- subset_env(ec_cc_sun_200cm, season = "summer")
  ec_cc_sun_200cm_winter <- subset_env(ec_cc_sun_200cm, season = "winter")
  # ec_cc_sun_50cm_summer <- subset_env(ec_cc_sun_50cm, season = "summer")
  # ec_cc_sun_50cm_winter <- subset_env(ec_cc_sun_50cm, season = "winter")
  list_cc_sun_200cm_summer[[s]] <- ec_cc_sun_200cm_summer
  list_cc_sun_200cm_winter[[s]] <- ec_cc_sun_200cm_winter
  # list_cc_sun_50cm_summer[[s]] <- ec_cc_sun_50cm_summer
  # list_cc_sun_50cm_winter[[s]] <- ec_cc_sun_50cm_winter
}

depth_p1 <- plot_depth(list_ec_cur_sun_200cm_winter, title='Sun', subtitle='', labels='no')
depth_p2 <- plot_depth(list_cc_sun_200cm_winter, title='Sun', subtitle='', labels='no', ylab='')
# depth_p3 <- plot_depth(list_ec_cur_sun_50cm_winter, subtitle='Winter - 50 cm', labels='no')
# depth_p4 <- plot_depth(list_cc_sun_50cm_winter, subtitle='Winter - 50 cm', labels='no', ylab='')
depth_p5 <- plot_depth(list_ec_cur_sun_200cm_summer, title='Sun', subtitle='', labels='yes')
depth_p6 <- plot_depth(list_cc_sun_200cm_summer, title='Sun', subtitle='', labels='yes', ylab='')
# depth_p7 <- plot_depth(list_ec_cur_sun_50cm_summer, subtitle='Summer - 50 cm', labels='yes')
# depth_p8 <- plot_depth(list_cc_sun_50cm_summer, subtitle='Summer - 50 cm', labels='yes', ylab='')

# plot_all_depths <- plot_grid(depth_p1, depth_p2, depth_p3, depth_p4, 
#                              depth_p5, depth_p6, depth_p7, depth_p8,
#                              labels=c("a", "b", "c", "d", "e", "f", "g", "h"),
#                              label_size = 18, ncol = 2, nrow = 4, align='v')
# 
# pdf('../figures/depths_sun.pdf', width = 6, height = 13)
# plot_all_depths
# dev.off()


# at deep shade

list_ec_cur_shade_200cm_winter <- list(ec_cur_shade_200cm_winter)
#list_ec_cur_shade_50cm_winter <- list(ec_cur_shade_50cm_winter)
list_ec_cur_shade_200cm_summer <- list(ec_cur_shade_200cm_summer)
#list_ec_cur_shade_50cm_summer <- list(ec_cur_shade_50cm_summer)
list_cc_shade_200cm_winter <- list()
#list_cc_shade_50cm_winter <- list()
list_cc_shade_200cm_summer <- list()
#list_cc_shade_50cm_summer <- list()
scen <- c('cc45', 'cc85', 'gf45', 'gf85', 'hg45', 'hg85')
for(s in scen){
  ec_cc_shade_200cm <- load_ectos(path = "../results",
                                scenario = s,
                                movement = "200cm",
                                shading = "shade",
                                micro = micro)
  # ec_cc_shade_50cm <- load_ectos(path = "../results",
  #                              scenario = s,
  #                              movement = "50cm",
  #                              shading = "shade",
  #                              micro = micro)
  ec_cc_shade_200cm$model <- as.factor(substr(ec_cc_shade_200cm$model, 1, nchar(as.character(ec_cc_shade_200cm$model))-5)) # set the same name to plot them stacked
  #ec_cc_shade_50cm$model <- as.factor(substr(ec_cc_shade_50cm$model, 1, nchar(as.character(ec_cc_shade_50cm$model))-5)) # set the same name to plot them stacked
  ec_cc_shade_200cm_summer <- subset_env(ec_cc_shade_200cm, season = "summer")
  ec_cc_shade_200cm_winter <- subset_env(ec_cc_shade_200cm, season = "winter")
  # ec_cc_shade_50cm_summer <- subset_env(ec_cc_shade_50cm, season = "summer")
  # ec_cc_shade_50cm_winter <- subset_env(ec_cc_shade_50cm, season = "winter")
  list_cc_shade_200cm_summer[[s]] <- ec_cc_shade_200cm_summer
  list_cc_shade_200cm_winter[[s]] <- ec_cc_shade_200cm_winter
  # list_cc_shade_50cm_summer[[s]] <- ec_cc_shade_50cm_summer
  # list_cc_shade_50cm_winter[[s]] <- ec_cc_shade_50cm_winter
}

depth_p2.1 <- plot_depth(list_ec_cur_shade_200cm_winter, title='Shade', subtitle='', labels='no', ylab='')
depth_p2.2 <- plot_depth(list_cc_shade_200cm_winter, title='Shade', subtitle='', labels='no', ylab='')
# depth_p2.3 <- plot_depth(list_ec_cur_shade_50cm_winter, subtitle='Winter - 50 cm', labels='no', ylab='')
# depth_p2.4 <- plot_depth(list_cc_shade_50cm_winter, subtitle='Winter - 50 cm', labels='no', ylab='')
depth_p2.5 <- plot_depth(list_ec_cur_shade_200cm_summer, title='Shade', subtitle='', labels='yes', ylab='')
depth_p2.6 <- plot_depth(list_cc_shade_200cm_summer, title='Shade', subtitle='', labels='yes', ylab='')
# depth_p2.7 <- plot_depth(list_ec_cur_shade_50cm_summer, subtitle='Summer - 50 cm', labels='yes', ylab='')
# depth_p2.8 <- plot_depth(list_cc_shade_50cm_summer, subtitle='Summer - 50 cm', labels='yes', ylab='')

# plot_all_depths <- plot_grid(depth_p2.1, depth_p2.2, depth_p2.3, depth_p2.4, 
#                              depth_p2.5, depth_p2.6, depth_p2.7, depth_p2.8,
#                              labels=c("a", "b", "c", "d", "e", "f", "g", "h"),
#                              label_size = 18, ncol = 2, nrow = 4, align='v')
# 
# pdf('../figures/depths_shade.pdf', width = 6, height = 13)
# plot_all_depths
# dev.off()

plot_all_depths <- plot_grid(depth_p1, depth_p2.1, depth_p2, depth_p2.2, 
                             depth_p5, depth_p2.5, depth_p6, depth_p2.6, 
                             labels=c("a", "b", "c", "d", 
                                      "e", "f", "g", "h"),
                             label_size = 18, ncol = 4, nrow = 2, align='hv')

pdf('../figures/depths_200cm.pdf', width = 12, height = 7)
plot_all_depths
dev.off()


# METABOLIC RATES: 

# differences in cum oxygen consumption change (delta)
# given different behaviors and the presence/absence of seasonal acclimation
# function ´plot_delta_smr()´

# at full sun

pdf('../figures/delta_smr_sun.pdf', width = 13, height = 8)
scen <- c('cc45', 'cc85', 'gf45', 'gf85', 'hg45', 'hg85')
for(s in scen){
  ec_cc_sun_200cm <- load_ectos(path = "../results",
                                scenario = s,
                                movement = "200cm",
                                shading = "sun",
                                micro = micro)
  ec_cc_sun_50cm <- load_ectos(path = "../results",
                               scenario = s,
                               movement = "50cm",
                               shading = "sun",
                               micro = micro)
  
  delta_p1 <- plot_delta_smr(ec_cur_sun_200cm, ec_cc_sun_200cm, period='all', title=paste("Whole year", s, sep=' - '))
  delta_p2 <- plot_delta_smr(ec_cur_sun_200cm, ec_cc_sun_200cm, period='summer', title="Summer", ylab='')
  delta_p3 <- plot_delta_smr(ec_cur_sun_200cm, ec_cc_sun_200cm, period='winter', title="Winter", ylab='')
  delta_p4 <- plot_delta_smr(ec_cur_sun_50cm, ec_cc_sun_50cm, period='all', xlab='year of simulation')
  delta_p5 <- plot_delta_smr(ec_cur_sun_50cm, ec_cc_sun_50cm, period='summer', ylab='', xlab='hour of simulation')
  delta_p6 <- plot_delta_smr(ec_cur_sun_50cm, ec_cc_sun_50cm, period='winter', ylab='', xlab='hour of simulation')
  
  plot_all_delta_smr <- plot_grid(delta_p1, delta_p2, delta_p3, delta_p4, delta_p5, delta_p6, 
                                  labels=c("a", "b", "c", "d", "e", "f"),
                                  label_size = 18, ncol = 3, nrow = 2, align='h')
  print(plot_all_delta_smr)
}
dev.off()


# at deep shade

pdf('../figures/delta_smr_shade.pdf', width = 13, height = 8)
scen <- c('cc45', 'cc85', 'gf45', 'gf85', 'hg45', 'hg85')
for(s in scen){
  ec_cc_shade_200cm <- load_ectos(path = "../results",
                                  scenario = s,
                                  movement = "200cm",
                                  shading = "shade",
                                  micro = micro)
  ec_cc_shade_50cm <- load_ectos(path = "../results",
                                 scenario = s,
                                 movement = "50cm",
                                 shading = "shade",
                                 micro = micro)
  
  delta_p1 <- plot_delta_smr(ec_cur_shade_200cm, ec_cc_shade_200cm, period='all', title=paste("Whole year", s, sep=' - '))
  delta_p2 <- plot_delta_smr(ec_cur_shade_200cm, ec_cc_shade_200cm, period='summer', title="Summer", ylab='')
  delta_p3 <- plot_delta_smr(ec_cur_shade_200cm, ec_cc_shade_200cm, period='winter', title="Winter", ylab='')
  delta_p4 <- plot_delta_smr(ec_cur_shade_50cm, ec_cc_shade_50cm, period='all', xlab='year of simulation')
  delta_p5 <- plot_delta_smr(ec_cur_shade_50cm, ec_cc_shade_50cm, period='summer', ylab='', xlab='hour of simulation')
  delta_p6 <- plot_delta_smr(ec_cur_shade_50cm, ec_cc_shade_50cm, period='winter', ylab='', xlab='hour of simulation')
  
  plot_all_delta_smr <- plot_grid(delta_p1, delta_p2, delta_p3, delta_p4, delta_p5, delta_p6, 
                                  labels=c("a", "b", "c", "d", "e", "f"),
                                  label_size = 18, ncol = 3, nrow = 2, align='h')
  print(plot_all_delta_smr)
}
dev.off()


# both at sun and deep shade

pdf('../figures/delta_smr_all.pdf', width = 13, height = 16)
scen <- c('cc45', 'cc85', 'gf45', 'gf85', 'hg45', 'hg85')
#scen <- c('cc45', 'gf85')
for(s in scen){
  ec_cc_sun_200cm <- load_ectos(path = "../results",
                                  scenario = s,
                                  movement = "200cm",
                                  shading = "sun",
                                  micro = micro)
  ec_cc_sun_50cm <- load_ectos(path = "../results",
                                 scenario = s,
                                 movement = "50cm",
                                 shading = "sun",
                                 micro = micro)
  ec_cc_shade_200cm <- load_ectos(path = "../results",
                                  scenario = s,
                                  movement = "200cm",
                                  shading = "shade",
                                  micro = micro)
  ec_cc_shade_50cm <- load_ectos(path = "../results",
                                 scenario = s,
                                 movement = "50cm",
                                 shading = "shade",
                                 micro = micro)
  
  delta_p1 <- plot_delta_smr(ec_cur_sun_200cm, ec_cc_sun_200cm, period='all', title=paste("Whole year - Sun", s, sep=' - '))
  delta_p2 <- plot_delta_smr(ec_cur_sun_200cm, ec_cc_sun_200cm, period='winter', title="Winter - Sun", ylab='')
  delta_p3 <- plot_delta_smr(ec_cur_sun_200cm, ec_cc_sun_200cm, period='summer', title="Summer - Sun", ylab='')
  delta_p4 <- plot_delta_smr(ec_cur_sun_50cm, ec_cc_sun_50cm, period='all', title=paste("Whole year - Sun", s, sep=' - '))
  delta_p5 <- plot_delta_smr(ec_cur_sun_50cm, ec_cc_sun_50cm, period='winter', title="Winter - Sun", ylab='')
  delta_p6 <- plot_delta_smr(ec_cur_sun_50cm, ec_cc_sun_50cm, period='summer', title="Summer - Sun", ylab='')
  delta_p2.1 <- plot_delta_smr(ec_cur_shade_200cm, ec_cc_shade_200cm, period='all', title=paste("Whole year - Shade", s, sep=' - '))
  delta_p2.2 <- plot_delta_smr(ec_cur_shade_200cm, ec_cc_shade_200cm, period='winter', title="Winter - Shade", ylab='')
  delta_p2.3 <- plot_delta_smr(ec_cur_shade_200cm, ec_cc_shade_200cm, period='summer', title="Summer - Shade", ylab='')
  delta_p2.4 <- plot_delta_smr(ec_cur_shade_50cm, ec_cc_shade_50cm, period='all', title=paste("Whole year - Shade", s, sep=' - '), xlab='year of simulation')
  delta_p2.5 <- plot_delta_smr(ec_cur_shade_50cm, ec_cc_shade_50cm, period='winter', title="Winter - Shade", ylab='', xlab='hour of simulation')
  delta_p2.6 <- plot_delta_smr(ec_cur_shade_50cm, ec_cc_shade_50cm, period='summer', title="Summer - Shade", ylab='', xlab='hour of simulation')
  
  plot_all_delta_smr <- plot_grid(delta_p1, delta_p2, delta_p3,
                                  delta_p2.1, delta_p2.2, delta_p2.3,
                                  delta_p4, delta_p5, delta_p6, 
                                  delta_p2.4, delta_p2.5, delta_p2.6, 
                                  labels=c("a", "b", "c", "d", "e", "f",
                                           "g", "h", "i", "j", "k", "l"),
                                  label_size = 18, ncol = 3, nrow = 4, align='h')
  print(plot_all_delta_smr)
}
dev.off()



# both at sun and deep shade SUBSET of panels

scen <- c('cc45', 'cc85', 'gf45', 'gf85', 'hg45', 'hg85')
for(s in scen){
  pdf(paste0('../figures/delta_smr_',s,'.pdf'), width = 17, height = 9)
  ec_cc_sun_200cm <- load_ectos(path = "../results",
                                scenario = s,
                                movement = "200cm",
                                shading = "sun",
                                micro = micro)
  ec_cc_sun_50cm <- load_ectos(path = "../results",
                               scenario = s,
                               movement = "50cm",
                               shading = "sun",
                               micro = micro)
  ec_cc_shade_200cm <- load_ectos(path = "../results",
                                  scenario = s,
                                  movement = "200cm",
                                  shading = "shade",
                                  micro = micro)
  ec_cc_shade_50cm <- load_ectos(path = "../results",
                                 scenario = s,
                                 movement = "50cm",
                                 shading = "shade",
                                 micro = micro)
  
  delta_p1 <- plot_delta_smr(ec_cur_sun_200cm, ec_cc_sun_200cm, period='winter', title="Sun")
  delta_p2 <- plot_delta_smr(ec_cur_shade_200cm, ec_cc_shade_200cm, period='winter', title="Shade", ylab='')
  delta_p3 <- plot_delta_smr(ec_cur_sun_200cm, ec_cc_sun_200cm, period='summer', title="Sun", ylab='')
  delta_p4 <- plot_delta_smr(ec_cur_shade_200cm, ec_cc_shade_200cm, period='summer', title="Shade", ylab='')
  delta_p5 <- plot_delta_smr(ec_cur_sun_50cm, ec_cc_sun_50cm, period='winter', title="Sun", xlab='hour of simulation', legend='yes')
  delta_p6 <- plot_delta_smr(ec_cur_shade_50cm, ec_cc_shade_50cm, period='winter', title="Shade", ylab='', xlab='hour of simulation')
  delta_p7 <- plot_delta_smr(ec_cur_sun_50cm, ec_cc_sun_50cm, period='summer', title="Sun", ylab='', xlab='hour of simulation')
  delta_p8 <- plot_delta_smr(ec_cur_shade_50cm, ec_cc_shade_50cm, period='summer', title="Shade", ylab='', xlab='hour of simulation')
  
  plot_all_delta_smr <- plot_grid(delta_p1, delta_p2, delta_p3, delta_p4,
                                  delta_p5, delta_p6, delta_p7, delta_p8,
                                  labels=c("a", "b", "c", "d", 
                                           "e", "f", "g", "h"),
                                  label_size = 18, ncol = 4, nrow = 2, align='hv')
  title <- ggdraw() + draw_label(s, fontface='bold')
  plot_all_delta_smr <- plot_grid(title, plot_all_delta_smr, ncol=1, rel_heights=c(0.1, 1))
  
  print(plot_all_delta_smr)
  dev.off()
}



# # Differences in cumulative oxygen consumption under current conditions 
# # given different behaviors and the presence/absence of seasonal acclimation
# # function ´plot_smr()´
# 
# p1 <- plot_smr(ec_cur_sun_200cm, period='all', title="Whole year")
# p2 <- plot_smr(ec_cur_sun_200cm_summer, period='subset', title="Summer", ylab='')
# p3 <- plot_smr(ec_cur_sun_200cm_winter, period='subset', title="Winter", ylab='')
# p4 <- plot_smr(ec_cur_sun_50cm, period='all', xlab='year of simulation')
# p5 <- plot_smr(ec_cur_sun_50cm_summer, period='subset', ylab='', xlab='hour of simulation')
# p6 <- plot_smr(ec_cur_sun_50cm_winter, period='subset', ylab='', xlab='hour of simulation')
# 
# 
# plot_all_smr <- plot_grid(p1, p2, p3, p4, p5, p6,
#                           labels=c("a", "b", "c", "d", "e", "f"),
#                           label_size = 18, ncol = 3, nrow = 2, align='h')
# 
# pdf('../figures/smr_current.pdf', width = 13, height = 8)
# print(plot_all_smr)
# dev.off()

