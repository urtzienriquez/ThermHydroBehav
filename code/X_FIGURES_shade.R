


setwd('~/Dropbox/1_papers/CAS/I_alpestris/0_living_underground')
rm(list=ls())

library(NicheMapR)
library(microclima)
library(raster)


load('microclimate.RData')
# load('environs.RData')
# load('environs_50cm.RData')
load('environs_shade.RData')
load('environs_shade_50cm.RData')


####################################
# extracting results

# temperatures underground for winter and summer
for(i in 1:length(environs)){
  environ <- environs[[i]]
  
  # TB
  # winter; 335 = 1 Dec; 59 = 28 February
  assign(paste('Tb_winter_', names(environs)[i], sep=''),
         environ$TC[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0])
  # summer; 152 = 1 June; 243 = 31 August
  assign(paste('Tb_summer_', names(environs)[i], sep=''),
         environ$TC[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0])
  
  
  # SMR
  # winter
  assign(paste('smr_winter_', names(environs)[i], sep=''),
         environ$SMR[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0])
  # summer
  assign(paste('smr_summer_', names(environs)[i], sep=''),
         environ$SMR[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0])
  
  
  # SMR with acclimation
  # winter
  assign(paste('smr_acc_winter_', names(environs)[i], sep=''),
         environ$SMR_acc[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0])
  # summer
  assign(paste('smr_acc_summer_', names(environs)[i], sep=''),
         environ$SMR_acc[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0])
  
  
  # Shelter depth
  # winter
  assign(paste('depth_winter_', names(environs)[i], sep=''),
         environ$DEP[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0])
  # summer
  assign(paste('depth_summer_', names(environs)[i], sep=''),
         environ$DEP[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0])
  
  
  
  # then if they can only retreat to shelters up to 50cm
  
  environ <- environs_50cm[[i]]
  
  # TB
  # winter; 335 = 1 Dec; 59 = 28 February
  assign(paste('Tb_winter_', names(environs_50cm)[i], sep=''),
         environ$TC[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0])
  # summer; 152 = 1 June; 243 = 31 August
  assign(paste('Tb_summer_', names(environs_50cm)[i], sep=''),
         environ$TC[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0])
  
  
  # SMR
  # winter
  assign(paste('smr_winter_', names(environs_50cm)[i], sep=''),
         environ$SMR[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0])
  # summer
  assign(paste('smr_summer_', names(environs_50cm)[i], sep=''),
         environ$SMR[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0])
  
  
  # SMR with acclimation
  # winter
  assign(paste('smr_acc_winter_', names(environs_50cm)[i], sep=''),
         environ$SMR_acc[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0])
  # summer
  assign(paste('smr_acc_summer_', names(environs_50cm)[i], sep=''),
         environ$SMR_acc[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0])
  
  
  # Shelter depth
  # winter
  assign(paste('depth_winter_', names(environs_50cm)[i], sep=''),
         environ$DEP[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0])
  # summer
  assign(paste('depth_summer_', names(environs_50cm)[i], sep=''),
         environ$DEP[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0])
}




summer <- data.frame(depth=c(depth_summer_b1w0, depth_summer_b1w1,
                             depth_summer_b2w0, depth_summer_b2w1,
                             depth_summer_b3w0, depth_summer_b3w1),
                     tb=c(Tb_summer_b1w0, Tb_summer_b1w1,
                          Tb_summer_b2w0, Tb_summer_b2w1,
                          Tb_summer_b3w0, Tb_summer_b3w1),
                     smr=c(smr_summer_b1w0, smr_summer_b1w1,
                           smr_summer_b2w0, smr_summer_b2w1,
                           smr_summer_b3w0, smr_summer_b3w1),
                     smr_acc=c(smr_acc_summer_b1w0, smr_acc_summer_b1w1,
                               smr_acc_summer_b2w0, smr_acc_summer_b2w1,
                               smr_acc_summer_b3w0, smr_acc_summer_b3w1),
                     behavior=rep(c('cold', 'cold-moist',
                                    'warm', 'warm-moist',
                                    'passive', 'passive-moist'),
                                  times=c(length(depth_summer_b1w0),length(depth_summer_b1w1),
                                          length(depth_summer_b2w0),length(depth_summer_b2w1),
                                          length(depth_summer_b3w0),length(depth_summer_b3w1))))


winter <- data.frame(depth=c(depth_winter_b1w0, depth_winter_b1w1,
                             depth_winter_b2w0, depth_winter_b2w1,
                             depth_winter_b3w0, depth_winter_b3w1),
                     tb=c(Tb_winter_b1w0, Tb_winter_b1w1,
                          Tb_winter_b2w0, Tb_winter_b2w1,
                          Tb_winter_b3w0, Tb_winter_b3w1),
                     smr=c(smr_winter_b1w0, smr_winter_b1w1,
                           smr_winter_b2w0, smr_winter_b2w1,
                           smr_winter_b3w0, smr_winter_b3w1),
                     smr_acc=c(smr_acc_winter_b1w0, smr_acc_winter_b1w1,
                               smr_acc_winter_b2w0, smr_acc_winter_b2w1,
                               smr_acc_winter_b3w0, smr_acc_winter_b3w1),
                     behavior=rep(c('cold', 'cold-moist',
                                    'warm', 'warm-moist',
                                    'passive', 'passive-moist'),
                                  times=c(length(depth_winter_b1w0),length(depth_winter_b1w1),
                                          length(depth_winter_b2w0),length(depth_winter_b2w1),
                                          length(depth_winter_b3w0),length(depth_winter_b3w1))))


summer_50cm <- data.frame(depth=c(depth_summer_b1w0_50cm, depth_summer_b1w1_50cm,
                                  depth_summer_b2w0_50cm, depth_summer_b2w1_50cm,
                                  depth_summer_b3w0_50cm, depth_summer_b3w1_50cm),
                          tb=c(Tb_summer_b1w0_50cm, Tb_summer_b1w1_50cm,
                               Tb_summer_b2w0_50cm, Tb_summer_b2w1_50cm,
                               Tb_summer_b3w0_50cm, Tb_summer_b3w1_50cm),
                          smr=c(smr_summer_b1w0_50cm, smr_summer_b1w1_50cm,
                                smr_summer_b2w0_50cm, smr_summer_b2w1_50cm,
                                smr_summer_b3w0_50cm, smr_summer_b3w1_50cm),
                          smr_acc=c(smr_acc_summer_b1w0_50cm, smr_acc_summer_b1w1_50cm,
                                    smr_acc_summer_b2w0_50cm, smr_acc_summer_b2w1_50cm,
                                    smr_acc_summer_b3w0_50cm, smr_acc_summer_b3w1_50cm),
                          behavior=rep(c('cold', 'cold-moist',
                                         'warm', 'warm-moist',
                                         'passive', 'passive-moist'),
                                       times=c(length(depth_summer_b1w0_50cm),length(depth_summer_b1w1_50cm),
                                               length(depth_summer_b2w0_50cm),length(depth_summer_b2w1_50cm),
                                               length(depth_summer_b3w0_50cm),length(depth_summer_b3w1_50cm))))


winter_50cm <- data.frame(depth=c(depth_winter_b1w0_50cm, depth_winter_b1w1_50cm,
                                  depth_winter_b2w0_50cm, depth_winter_b2w1_50cm,
                                  depth_winter_b3w0_50cm, depth_winter_b3w1_50cm),
                          tb=c(Tb_winter_b1w0_50cm, Tb_winter_b1w1_50cm,
                               Tb_winter_b2w0_50cm, Tb_winter_b2w1_50cm,
                               Tb_winter_b3w0_50cm, Tb_winter_b3w1_50cm),
                          smr=c(smr_winter_b1w0_50cm, smr_winter_b1w1_50cm,
                                smr_winter_b2w0_50cm, smr_winter_b2w1_50cm,
                                smr_winter_b3w0_50cm, smr_winter_b3w1_50cm),
                          smr_acc=c(smr_acc_winter_b1w0_50cm, smr_acc_winter_b1w1_50cm,
                                    smr_acc_winter_b2w0_50cm, smr_acc_winter_b2w1_50cm,
                                    smr_acc_winter_b3w0_50cm, smr_acc_winter_b3w1_50cm),
                          behavior=rep(c('cold', 'cold-moist',
                                         'warm', 'warm-moist',
                                         'passive', 'passive-moist'),
                                       times=c(length(depth_winter_b1w0_50cm),length(depth_winter_b1w1_50cm),
                                               length(depth_winter_b2w0_50cm),length(depth_winter_b2w1_50cm),
                                               length(depth_winter_b3w0_50cm),length(depth_winter_b3w1_50cm))))





smr_winter <- data.frame(smr=c(smr_winter_b1w0, smr_winter_b1w1,
                               smr_winter_b2w0, smr_winter_b2w1,
                               smr_winter_b3w0, smr_winter_b3w1,
                               smr_acc_winter_b1w0, smr_acc_winter_b1w1,
                               smr_acc_winter_b2w0, smr_acc_winter_b2w1,
                               smr_acc_winter_b3w0, smr_acc_winter_b3w1),
                         acc=rep(c('no acc', 'acc'),
                                 each=length(c(smr_winter_b1w0, smr_winter_b1w1,
                                               smr_winter_b2w0, smr_winter_b2w1,
                                               smr_winter_b3w0, smr_winter_b3w1))),
                         behavior=rep(rep(c('cold', 'cold-moist',
                                            'warm', 'warm-moist',
                                            'passive', 'passive-moist'),
                                          times=c(length(depth_winter_b1w0),length(depth_winter_b1w1),
                                                  length(depth_winter_b2w0),length(depth_winter_b2w1),
                                                  length(depth_winter_b3w0),length(depth_winter_b3w1))),
                                      times=2))


smr_summer <- data.frame(smr=c(smr_summer_b1w0, smr_summer_b1w1,
                               smr_summer_b2w0, smr_summer_b2w1,
                               smr_summer_b3w0, smr_summer_b3w1,
                               smr_acc_summer_b1w0, smr_acc_summer_b1w1,
                               smr_acc_summer_b2w0, smr_acc_summer_b2w1,
                               smr_acc_summer_b3w0, smr_acc_summer_b3w1),
                         acc=rep(c('no acc', 'acc'),
                                 each=length(c(smr_summer_b1w0, smr_summer_b1w1,
                                               smr_summer_b2w0, smr_summer_b2w1,
                                               smr_summer_b3w0, smr_summer_b3w1))),
                         behavior=rep(rep(c('cold', 'cold-moist',
                                            'warm', 'warm-moist',
                                            'passive', 'passive-moist'),
                                          times=c(length(depth_summer_b1w0),length(depth_summer_b1w1),
                                                  length(depth_summer_b2w0),length(depth_summer_b2w1),
                                                  length(depth_summer_b3w0),length(depth_summer_b3w1))),
                                      times=2))


smr_winter_50cm <- data.frame(smr=c(smr_winter_b1w0_50cm, smr_winter_b1w1_50cm,
                                    smr_winter_b2w0_50cm, smr_winter_b2w1_50cm,
                                    smr_winter_b3w0_50cm, smr_winter_b3w1_50cm,
                                    smr_acc_winter_b1w0_50cm, smr_acc_winter_b1w1_50cm,
                                    smr_acc_winter_b2w0_50cm, smr_acc_winter_b2w1_50cm,
                                    smr_acc_winter_b3w0_50cm, smr_acc_winter_b3w1_50cm),
                              acc=rep(c('no acc', 'acc'),
                                      each=length(c(smr_winter_b1w0_50cm, smr_winter_b1w1_50cm,
                                                    smr_winter_b2w0_50cm, smr_winter_b2w1_50cm,
                                                    smr_winter_b3w0_50cm, smr_winter_b3w1_50cm))),
                              behavior=rep(rep(c('cold', 'cold-moist',
                                                 'warm', 'warm-moist',
                                                 'passive', 'passive-moist'),
                                               times=c(length(depth_winter_b1w0_50cm),length(depth_winter_b1w1_50cm),
                                                       length(depth_winter_b2w0_50cm),length(depth_winter_b2w1_50cm),
                                                       length(depth_winter_b3w0_50cm),length(depth_winter_b3w1_50cm))),
                                           times=2))


smr_summer_50cm <- data.frame(smr=c(smr_summer_b1w0_50cm, smr_summer_b1w1_50cm,
                                    smr_summer_b2w0_50cm, smr_summer_b2w1_50cm,
                                    smr_summer_b3w0_50cm, smr_summer_b3w1_50cm,
                                    smr_acc_summer_b1w0_50cm, smr_acc_summer_b1w1_50cm,
                                    smr_acc_summer_b2w0_50cm, smr_acc_summer_b2w1_50cm,
                                    smr_acc_summer_b3w0_50cm, smr_acc_summer_b3w1_50cm),
                              acc=rep(c('no acc', 'acc'),
                                      each=length(c(smr_summer_b1w0_50cm, smr_summer_b1w1_50cm,
                                                    smr_summer_b2w0_50cm, smr_summer_b2w1_50cm,
                                                    smr_summer_b3w0_50cm, smr_summer_b3w1_50cm))),
                              behavior=rep(rep(c('cold', 'cold-moist',
                                                 'warm', 'warm-moist',
                                                 'passive', 'passive-moist'),
                                               times=c(length(depth_summer_b1w0_50cm),length(depth_summer_b1w1_50cm),
                                                       length(depth_summer_b2w0_50cm),length(depth_summer_b2w1_50cm),
                                                       length(depth_summer_b3w0_50cm),length(depth_summer_b3w1_50cm))),
                                           times=2))


# Initiate a ggplot
library(ggplot2)
library(ggridges)
library(viridis)
library(tidyverse)
library(cowplot)



######
# Figure 1

library(reshape2)
library(ggpubr)


plot_depth_summer <- ggplot(summer, aes(x = behavior, y = depth, fill = behavior))
plot_depth_winter <- ggplot(winter, aes(x = behavior, y = depth, fill = behavior))
plot_depth_summer_50cm <- ggplot(summer_50cm, aes(x = behavior, y = depth, fill = behavior))
plot_depth_winter_50cm <- ggplot(winter_50cm, aes(x = behavior, y = depth, fill = behavior))


p_depth_summer <- plot_depth_summer + geom_violin(scale='width') +
  scale_fill_manual(values=c('cold' = '#0059FF', 
                             'cold-moist' = '#86B0FF',
                             'passive' = '#01BB04', 
                             'passive-moist' = '#8AFF86',
                             'warm' = '#FF8300', 
                             'warm-moist' = '#FEBB74')) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab('') + 
  ylab('')



p_depth_winter <- plot_depth_winter + geom_violin(scale='width') +
  scale_fill_manual(values=c('cold' = '#0059FF', 
                             'cold-moist' = '#86B0FF',
                             'passive' = '#01BB04', 
                             'passive-moist' = '#8AFF86',
                             'warm' = '#FF8300', 
                             'warm-moist' = '#FEBB74')) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab('') + 
  ylab('Depth')


p_depth_summer_50cm <- plot_depth_summer_50cm + geom_violin(scale='width') +
  scale_fill_manual(values=c('cold' = '#0059FF',
                             'cold-moist' = '#86B0FF',
                             'passive' = '#01BB04',
                             'passive-moist' = '#8AFF86',
                             'warm' = '#FF8300',
                             'warm-moist' = '#FEBB74')) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('') +
  ylab('')



p_depth_winter_50cm <- plot_depth_winter_50cm + geom_violin(scale='width') +
  scale_fill_manual(values=c('cold' = '#0059FF',
                             'cold-moist' = '#86B0FF',
                             'passive' = '#01BB04',
                             'passive-moist' = '#8AFF86',
                             'warm' = '#FF8300',
                             'warm-moist' = '#FEBB74')) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('') +
  ylab('')



soil <- data.frame(micro$soil)[,-3]
colnames(soil)[-c(1,2)] <- c("2 cm","3 cm","5 cm","8 cm","12 cm","20 cm","30 cm","50 cm","100 cm")

soilpot <- data.frame(micro$soilpot)[,-3]
colnames(soilpot)[-c(1,2)] <- c("2 cm","3 cm","5 cm","8 cm","12 cm","20 cm","30 cm","50 cm","100 cm")

mean_soil_temps <- apply(soil[,-c(1,2)], 2, function(x) tapply(x, soil$DOY, mean))
mean_soil_temps_m <- melt(mean_soil_temps)

min_soil_temps <- apply(soil[,-c(1,2)], 2, function(x) tapply(x, soil$DOY, min))
min_soil_temps_m <- melt(min_soil_temps)

max_soil_temps <- apply(soil[,-c(1,2)], 2, function(x) tapply(x, soil$DOY, max))
max_soil_temps_m <- melt(max_soil_temps)

mean_soilpot <- apply(soilpot[,-c(1,2)], 2, function(x) tapply(x, soilpot$DOY, mean))
mean_soilpot_m <- melt(mean_soilpot)

p_soiltemp <- ggplot(mean_soil_temps_m, aes(x = Var1, y = fct_rev(Var2))) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(
    low = 'deepskyblue3', mid = 'darkseagreen2', high = 'brown1',
    midpoint = 0, guide = 'colourbar', aesthetics = 'fill',
    name = 'Temperature (in ºC)'
  ) + 
  xlab('Day of the year') + 
  ylab('Depth') +
  theme_minimal() +
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 7),
        legend.position="bottom", legend.key.height=unit(.1,"cm"))

p_minsoiltemp <- ggplot(min_soil_temps_m, aes(x = Var1, y = fct_rev(Var2))) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(
    low = 'deepskyblue3', mid = 'darkseagreen2', high = 'brown1',
    midpoint = 0, guide = 'colourbar', aesthetics = 'fill',
    name = 'Temperature (in ºC)'
  ) + 
  xlab('Day of the year') + 
  ylab('') +
  theme_minimal() +
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 7),
        legend.position="bottom", legend.key.height=unit(.1,"cm"))

p_maxsoiltemp <- ggplot(max_soil_temps_m, aes(x = Var1, y = fct_rev(Var2))) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(
    low = 'deepskyblue3', mid = 'darkseagreen2', high = 'brown1',
    midpoint = 0, guide = 'colourbar', aesthetics = 'fill',
    name = 'Temperature (in ºC)'
  ) + 
  xlab('Day of the year') + 
  ylab('') +
  theme_minimal() +
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 7),
        legend.position="bottom", legend.key.height=unit(.1,"cm"))

p_soilpot <- ggplot(mean_soilpot_m, aes(x = Var1, y = fct_rev(Var2))) + 
  geom_tile(aes(fill = value)) +
  scale_fill_viridis(option = "E", direction = -1, name = expression(paste('Potential (J ', kg^-1, ')'))) + 
  xlab('Day of the year') + 
  ylab('') +
  theme_minimal() +
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 7),
        legend.position="bottom", legend.key.height=unit(.1,"cm"))



plot_depths <- ggarrange(p_soiltemp,
                         p_minsoiltemp,
                         p_maxsoiltemp,
                         p_soilpot,
                         ggarrange(p_depth_winter, p_depth_summer, ncol = 2, labels = c("e", "f")),
                         nrow = 5, 
                         labels = c("a","b","c","d")
) 


# ggexport(plot_depths, filename = "./results/Fig1_depths.pdf", height = 10, width=7)


########
# Figure 2

# body temperatures

plot_tb_summer <- ggplot(summer, aes(y = fct_reorder(behavior, tb, .fun = sum), 
                                     x = `tb`, 
                                     fill = stat(x)))

plot_tb_winter <- ggplot(winter, aes(y = fct_reorder(behavior, tb, .fun = sum), 
                                     x = `tb`, 
                                     fill = stat(x)))

plot_tb_summer_50cm <- ggplot(summer_50cm, aes(y = fct_reorder(behavior, tb, .fun = sum), 
                                               x = `tb`, 
                                               fill = stat(x)))

plot_tb_winter_50cm <- ggplot(winter_50cm, aes(y = fct_reorder(behavior, tb, .fun = sum), 
                                               x = `tb`, 
                                               fill = stat(x)))

p_tb_summer <- plot_tb_summer + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_fill_viridis(option = "B") +
  theme_ridges() +
  theme(legend.position = "none") +
  ylab('') + 
  xlab('')


p_tb_winter <- plot_tb_winter + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_fill_viridis(option = "B") +
  theme_ridges() +
  theme(legend.position = "none") +
  ylab('') + 
  xlab('')


p_tb_summer_50cm <- plot_tb_summer_50cm + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_fill_viridis(option = "B") +
  theme_ridges() +
  theme(legend.position = "none") +
  ylab('') + 
  xlab('Body temperature (in ºC)')


p_tb_winter_50cm <- plot_tb_winter_50cm + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_fill_viridis(option = "B") +
  theme_ridges() +
  theme(legend.position = "none") +
  ylab('') + 
  xlab('Body temperature (in ºC)')




plot_all <- plot_grid(p_tb_winter, p_tb_summer, 
                      p_tb_winter_50cm, p_tb_summer_50cm,
                      labels=c("a", "b", "c", "d"),
                      label_size = 24, ncol = 2, nrow = 2, align='h')

# save_plot("./results/Fig2_smr_tb.pdf", plot_all, ncol = 2, nrow = 2, base_height = 4.5, base_aspect_ratio = 1)



# Figure 3

# metabolic rates

plot_smr_winter <- ggplot(smr_winter, aes(y = fct_reorder(behavior, smr, .fun = sum), 
                                          x = `smr`, 
                                          fill = acc,
                                          color=acc))

plot_smr_summer <- ggplot(smr_summer, aes(y = fct_reorder(behavior, smr, .fun = sum), 
                                          x = `smr`, 
                                          fill = acc,
                                          color=acc))

plot_smr_winter_50cm <- ggplot(smr_winter_50cm, aes(y = fct_reorder(behavior, smr, .fun = sum), 
                                          x = `smr`, 
                                          fill = acc,
                                          color=acc))

plot_smr_summer_50cm <- ggplot(smr_summer_50cm, aes(y = fct_reorder(behavior, smr, .fun = sum), 
                                          x = `smr`, 
                                          fill = acc,
                                          color=acc))

p_smr_winter <- plot_smr_winter + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_color_manual(values = adjustcolor(c("seagreen3", "red"), alpha.f = 0.7)) +
  scale_fill_manual(values = adjustcolor(c("seagreen3", "red"), alpha.f = 0.7)) +
  theme_ridges() +
  theme(legend.position = "none") +
  ylab('') + 
  xlab('')


p_smr_summer <- plot_smr_summer + geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.001) +
  scale_color_manual(values = adjustcolor(c("seagreen3", "red"), alpha.f = 0.7)) +
  scale_fill_manual(values = adjustcolor(c("seagreen3", "red"), alpha.f = 0.7)) +
  theme_ridges() +
  theme(legend.position = "none") +
  ylab('') + 
  xlab('')


p_smr_winter_50cm <- plot_smr_winter_50cm + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_color_manual(values = adjustcolor(c("seagreen3", "red"), alpha.f = 0.7)) +
  scale_fill_manual(values = adjustcolor(c("seagreen3", "red"), alpha.f = 0.7)) +
  theme_ridges() +
  theme(legend.position = "none") +
  ylab('') + 
  xlab(expression(paste('SMR (in mL ', O[2], ' ', h^-1, ')')))


p_smr_summer_50cm <- plot_smr_summer_50cm + geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.001) +
  scale_color_manual(values = adjustcolor(c("seagreen3", "red"), alpha.f = 0.7)) +
  scale_fill_manual(values = adjustcolor(c("seagreen3", "red"), alpha.f = 0.7)) +
  theme_ridges() +
  theme(legend.position = "none") +
  ylab('') + 
  xlab(expression(paste('SMR (in mL ', O[2], ' ', h^-1, ')')))




plot_all_smr <- plot_grid(p_smr_winter, p_smr_summer, 
                      p_smr_winter_50cm, p_smr_summer_50cm,
                      labels=c("a", "b", "c", "d"),
                      label_size = 24, ncol = 2, nrow = 2, align='h')


# save_plot("./results/Fig2_smr_tb.pdf", plot_all, ncol = 2, nrow = 2, base_height = 4.5, base_aspect_ratio = 1)




####################################################################################
#           CLIMATE CHANGE
#


# load('environs_cc.RData')
# load('environs_cc_50cm.RData')
load('environs_cc_shade.RData')
load('environs_cc_shade_50cm.RData')


####################################
# extracting results

# temperatures underground for winter and summer
for(i in 1:length(environs_cc)){
  environ <- environs_cc[[i]]
  environ_bl <- environs[[substr(names(environs_cc), 1, 4)[[i]]]]
  
  # TB
  # winter; 335 = 1 Dec; 59 = 28 February
  dTC <- environ$TC[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0] - 
    environ_bl$TC[environ_bl$DOY >= 335 | environ_bl$DOY <= 59 & environ_bl$ACT == 0]
  
  assign(paste('Tb_winter_', names(environs_cc)[i], sep=''), dTC)
  
  # summer; 152 = 1 June; 243 = 31 August
  dTC <- environ$TC[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0] - 
    environ_bl$TC[environ_bl$DOY >= 152 & environ_bl$DOY <= 243 & environ_bl$ACT == 0]
  
  assign(paste('Tb_summer_', names(environs_cc)[i], sep=''), dTC)
  
  
  # Shelter depth
  # winter
  DEP <- environ$DEP[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0]
  
  assign(paste('depth_winter_', names(environs_cc)[i], sep=''), DEP)
  
  # summer
  DEP <- environ$DEP[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0]
  
  assign(paste('depth_summer_', names(environs_cc)[i], sep=''), DEP)
  
  
  # SMR (always compare non-acclimated to acclimated, SMRfut - SMR and SMRacc_fut - SMR)
  environ_bl <- environs[["b1w0"]]
  
  # winter
  dSMR <- environ$SMR[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 335 | environ_bl$DOY <= 59 & environ_bl$ACT == 0]
  
  assign(paste('smr_winter_', names(environs_cc)[i], sep=''), dSMR)
  
  # summer
  dSMR <- environ$SMR[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 152 & environ_bl$DOY <= 243 & environ_bl$ACT == 0]
  
  assign(paste('smr_summer_', names(environs_cc)[i], sep=''), dSMR)
  
  
  # SMR with acclimation
  # winter
  dSMR <- environ$SMR_acc[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 335 | environ_bl$DOY <= 59 & environ_bl$ACT == 0]
  
  assign(paste('smr_acc_winter_', names(environs_cc)[i], sep=''), dSMR)
  
  # summer
  dSMR <- environ$SMR_acc[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 152 & environ_bl$DOY <= 243 & environ_bl$ACT == 0]
  
  assign(paste('smr_acc_summer_', names(environs_cc)[i], sep=''), dSMR)
  
  
  
  
  
  # then if they can only retreat to shelters up to 50cm
  
  environ <- environs_cc_50cm[[i]]
  environ_bl <- environs_50cm[[paste(substr(names(environs_cc_50cm), 1, 4)[[i]], '_50cm', sep='')]]
  
  # TB
  # winter; 335 = 1 Dec; 59 = 28 February
  dTC <- environ$TC[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0] - 
    environ_bl$TC[environ_bl$DOY >= 335 | environ_bl$DOY <= 59 & environ_bl$ACT == 0]
  
  assign(paste('Tb_winter_', names(environs_cc_50cm)[i], sep=''), dTC)
  
  # summer; 152 = 1 June; 243 = 31 August
  dTC <- environ$TC[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0] - 
    environ_bl$TC[environ_bl$DOY >= 152 & environ_bl$DOY <= 243 & environ_bl$ACT == 0]
  
  assign(paste('Tb_summer_', names(environs_cc_50cm)[i], sep=''), dTC)
  
  
  # Shelter depth
  # winter
  DEP <- environ$DEP[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0]
  
  assign(paste('depth_winter_', names(environs_cc_50cm)[i], sep=''), DEP)
  
  # summer
  DEP <- environ$DEP[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0]
  
  assign(paste('depth_summer_', names(environs_cc_50cm)[i], sep=''), DEP)
  
  
  # SMR (always compare non-acclimated to acclimated, SMRfut - SMR and SMRacc_fut - SMR)
  environ_bl <- environs_50cm[["b1w0_50cm"]]
  
  # winter
  dSMR <- environ$SMR[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 335 | environ_bl$DOY <= 59 & environ_bl$ACT == 0]
  
  assign(paste('smr_winter_', names(environs_cc_50cm)[i], sep=''), dSMR)
  
  # summer
  dSMR <- environ$SMR[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 152 & environ_bl$DOY <= 243 & environ_bl$ACT == 0]
  
  assign(paste('smr_summer_', names(environs_cc_50cm)[i], sep=''), dSMR)
  
  
  # SMR with acclimation
  # winter
  dSMR <- environ$SMR_acc[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 335 | environ_bl$DOY <= 59 & environ_bl$ACT == 0]
  
  assign(paste('smr_acc_winter_', names(environs_cc_50cm)[i], sep=''), dSMR)
  
  # summer
  dSMR <- environ$SMR_acc[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 152 & environ_bl$DOY <= 243 & environ_bl$ACT == 0]
  
  assign(paste('smr_acc_summer_', names(environs_cc_50cm)[i], sep=''), dSMR)
  
  
}



smr_winter_cc <- data.frame(smr=c(smr_winter_b1w0_rcp45_CCSM4, `smr_winter_b1w0_rcp45_GFDL-CM3`, `smr_winter_b1w0_rcp45_HadGEM2-CC`,
                                  smr_winter_b1w0_rcp85_CCSM4, `smr_winter_b1w0_rcp85_GFDL-CM3`, `smr_winter_b1w0_rcp85_HadGEM2-CC`,
                                  smr_winter_b1w1_rcp45_CCSM4, `smr_winter_b1w1_rcp45_GFDL-CM3`, `smr_winter_b1w1_rcp45_HadGEM2-CC`,
                                  smr_winter_b1w1_rcp85_CCSM4, `smr_winter_b1w1_rcp85_GFDL-CM3`, `smr_winter_b1w1_rcp85_HadGEM2-CC`,
                                  smr_winter_b2w0_rcp45_CCSM4, `smr_winter_b2w0_rcp45_GFDL-CM3`, `smr_winter_b2w0_rcp45_HadGEM2-CC`,
                                  smr_winter_b2w0_rcp85_CCSM4, `smr_winter_b2w0_rcp85_GFDL-CM3`, `smr_winter_b2w0_rcp85_HadGEM2-CC`,
                                  smr_winter_b2w1_rcp45_CCSM4, `smr_winter_b2w1_rcp45_GFDL-CM3`, `smr_winter_b2w1_rcp45_HadGEM2-CC`,
                                  smr_winter_b2w1_rcp85_CCSM4, `smr_winter_b2w1_rcp85_GFDL-CM3`, `smr_winter_b2w1_rcp85_HadGEM2-CC`,
                                  smr_winter_b3w0_rcp45_CCSM4, `smr_winter_b3w0_rcp45_GFDL-CM3`, `smr_winter_b3w0_rcp45_HadGEM2-CC`,
                                  smr_winter_b3w0_rcp85_CCSM4, `smr_winter_b3w0_rcp85_GFDL-CM3`, `smr_winter_b3w0_rcp85_HadGEM2-CC`,
                                  smr_winter_b3w1_rcp45_CCSM4, `smr_winter_b3w1_rcp45_GFDL-CM3`, `smr_winter_b3w1_rcp45_HadGEM2-CC`,
                                  smr_winter_b3w1_rcp85_CCSM4, `smr_winter_b3w1_rcp85_GFDL-CM3`, `smr_winter_b3w1_rcp85_HadGEM2-CC`,
                                  smr_acc_winter_b1w0_rcp45_CCSM4, `smr_acc_winter_b1w0_rcp45_GFDL-CM3`, `smr_acc_winter_b1w0_rcp45_HadGEM2-CC`,
                                  smr_acc_winter_b1w0_rcp85_CCSM4, `smr_acc_winter_b1w0_rcp85_GFDL-CM3`, `smr_acc_winter_b1w0_rcp85_HadGEM2-CC`,
                                  smr_acc_winter_b1w1_rcp45_CCSM4, `smr_acc_winter_b1w1_rcp45_GFDL-CM3`, `smr_acc_winter_b1w1_rcp45_HadGEM2-CC`,
                                  smr_acc_winter_b1w1_rcp85_CCSM4, `smr_acc_winter_b1w1_rcp85_GFDL-CM3`, `smr_acc_winter_b1w1_rcp85_HadGEM2-CC`,
                                  smr_acc_winter_b2w0_rcp45_CCSM4, `smr_acc_winter_b2w0_rcp45_GFDL-CM3`, `smr_acc_winter_b2w0_rcp45_HadGEM2-CC`,
                                  smr_acc_winter_b2w0_rcp85_CCSM4, `smr_acc_winter_b2w0_rcp85_GFDL-CM3`, `smr_acc_winter_b2w0_rcp85_HadGEM2-CC`,
                                  smr_acc_winter_b2w1_rcp45_CCSM4, `smr_acc_winter_b2w1_rcp45_GFDL-CM3`, `smr_acc_winter_b2w1_rcp45_HadGEM2-CC`,
                                  smr_acc_winter_b2w1_rcp85_CCSM4, `smr_acc_winter_b2w1_rcp85_GFDL-CM3`, `smr_acc_winter_b2w1_rcp85_HadGEM2-CC`,
                                  smr_acc_winter_b3w0_rcp45_CCSM4, `smr_acc_winter_b3w0_rcp45_GFDL-CM3`, `smr_acc_winter_b3w0_rcp45_HadGEM2-CC`,
                                  smr_acc_winter_b3w0_rcp85_CCSM4, `smr_acc_winter_b3w0_rcp85_GFDL-CM3`, `smr_acc_winter_b3w0_rcp85_HadGEM2-CC`,
                                  smr_acc_winter_b3w1_rcp45_CCSM4, `smr_acc_winter_b3w1_rcp45_GFDL-CM3`, `smr_acc_winter_b3w1_rcp45_HadGEM2-CC`,
                                  smr_acc_winter_b3w1_rcp85_CCSM4, `smr_acc_winter_b3w1_rcp85_GFDL-CM3`, `smr_acc_winter_b3w1_rcp85_HadGEM2-CC`),
                         acc=rep(c('no acc', 'acc'), each=697752),
                         behavior=rep(rep(c('cold', 'cold-moist',
                                            'warm', 'warm-moist',
                                            'passive', 'passive-moist'), each=116292), times=2),
                         sce = rep(rep(c('rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                     'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC'), 
                                   each=19382), times=12))




smr_summer_cc <- data.frame(smr=c(smr_summer_b1w0_rcp45_CCSM4, `smr_summer_b1w0_rcp45_GFDL-CM3`, `smr_summer_b1w0_rcp45_HadGEM2-CC`,
                                  smr_summer_b1w0_rcp85_CCSM4, `smr_summer_b1w0_rcp85_GFDL-CM3`, `smr_summer_b1w0_rcp85_HadGEM2-CC`,
                                  smr_summer_b1w1_rcp45_CCSM4, `smr_summer_b1w1_rcp45_GFDL-CM3`, `smr_summer_b1w1_rcp45_HadGEM2-CC`,
                                  smr_summer_b1w1_rcp85_CCSM4, `smr_summer_b1w1_rcp85_GFDL-CM3`, `smr_summer_b1w1_rcp85_HadGEM2-CC`,
                                  smr_summer_b2w0_rcp45_CCSM4, `smr_summer_b2w0_rcp45_GFDL-CM3`, `smr_summer_b2w0_rcp45_HadGEM2-CC`,
                                  smr_summer_b2w0_rcp85_CCSM4, `smr_summer_b2w0_rcp85_GFDL-CM3`, `smr_summer_b2w0_rcp85_HadGEM2-CC`,
                                  smr_summer_b2w1_rcp45_CCSM4, `smr_summer_b2w1_rcp45_GFDL-CM3`, `smr_summer_b2w1_rcp45_HadGEM2-CC`,
                                  smr_summer_b2w1_rcp85_CCSM4, `smr_summer_b2w1_rcp85_GFDL-CM3`, `smr_summer_b2w1_rcp85_HadGEM2-CC`,
                                  smr_summer_b3w0_rcp45_CCSM4, `smr_summer_b3w0_rcp45_GFDL-CM3`, `smr_summer_b3w0_rcp45_HadGEM2-CC`,
                                  smr_summer_b3w0_rcp85_CCSM4, `smr_summer_b3w0_rcp85_GFDL-CM3`, `smr_summer_b3w0_rcp85_HadGEM2-CC`,
                                  smr_summer_b3w1_rcp45_CCSM4, `smr_summer_b3w1_rcp45_GFDL-CM3`, `smr_summer_b3w1_rcp45_HadGEM2-CC`,
                                  smr_summer_b3w1_rcp85_CCSM4, `smr_summer_b3w1_rcp85_GFDL-CM3`, `smr_summer_b3w1_rcp85_HadGEM2-CC`,
                                  smr_acc_summer_b1w0_rcp45_CCSM4, `smr_acc_summer_b1w0_rcp45_GFDL-CM3`, `smr_acc_summer_b1w0_rcp45_HadGEM2-CC`,
                                  smr_acc_summer_b1w0_rcp85_CCSM4, `smr_acc_summer_b1w0_rcp85_GFDL-CM3`, `smr_acc_summer_b1w0_rcp85_HadGEM2-CC`,
                                  smr_acc_summer_b1w1_rcp45_CCSM4, `smr_acc_summer_b1w1_rcp45_GFDL-CM3`, `smr_acc_summer_b1w1_rcp45_HadGEM2-CC`,
                                  smr_acc_summer_b1w1_rcp85_CCSM4, `smr_acc_summer_b1w1_rcp85_GFDL-CM3`, `smr_acc_summer_b1w1_rcp85_HadGEM2-CC`,
                                  smr_acc_summer_b2w0_rcp45_CCSM4, `smr_acc_summer_b2w0_rcp45_GFDL-CM3`, `smr_acc_summer_b2w0_rcp45_HadGEM2-CC`,
                                  smr_acc_summer_b2w0_rcp85_CCSM4, `smr_acc_summer_b2w0_rcp85_GFDL-CM3`, `smr_acc_summer_b2w0_rcp85_HadGEM2-CC`,
                                  smr_acc_summer_b2w1_rcp45_CCSM4, `smr_acc_summer_b2w1_rcp45_GFDL-CM3`, `smr_acc_summer_b2w1_rcp45_HadGEM2-CC`,
                                  smr_acc_summer_b2w1_rcp85_CCSM4, `smr_acc_summer_b2w1_rcp85_GFDL-CM3`, `smr_acc_summer_b2w1_rcp85_HadGEM2-CC`,
                                  smr_acc_summer_b3w0_rcp45_CCSM4, `smr_acc_summer_b3w0_rcp45_GFDL-CM3`, `smr_acc_summer_b3w0_rcp45_HadGEM2-CC`,
                                  smr_acc_summer_b3w0_rcp85_CCSM4, `smr_acc_summer_b3w0_rcp85_GFDL-CM3`, `smr_acc_summer_b3w0_rcp85_HadGEM2-CC`,
                                  smr_acc_summer_b3w1_rcp45_CCSM4, `smr_acc_summer_b3w1_rcp45_GFDL-CM3`, `smr_acc_summer_b3w1_rcp45_HadGEM2-CC`,
                                  smr_acc_summer_b3w1_rcp85_CCSM4, `smr_acc_summer_b3w1_rcp85_GFDL-CM3`, `smr_acc_summer_b3w1_rcp85_HadGEM2-CC`),
                            acc=rep(c('no acc', 'acc'), times=c(601436, 601436)),
                            behavior=rep(rep(c('cold', 'cold-moist',
                                           'warm', 'warm-moist',
                                           'passive', 'passive-moist'), 
                                         times=c(length(c(smr_summer_b1w0_rcp45_CCSM4, `smr_summer_b1w0_rcp45_GFDL-CM3`, `smr_summer_b1w0_rcp45_HadGEM2-CC`,
                                                          smr_summer_b1w0_rcp85_CCSM4, `smr_summer_b1w0_rcp85_GFDL-CM3`, `smr_summer_b1w0_rcp85_HadGEM2-CC`)),
                                                 length(c(smr_summer_b1w1_rcp45_CCSM4, `smr_summer_b1w1_rcp45_GFDL-CM3`, `smr_summer_b1w1_rcp45_HadGEM2-CC`,
                                                          smr_summer_b1w1_rcp85_CCSM4, `smr_summer_b1w1_rcp85_GFDL-CM3`, `smr_summer_b1w1_rcp85_HadGEM2-CC`)),
                                                 length(c(smr_summer_b2w0_rcp45_CCSM4, `smr_summer_b2w0_rcp45_GFDL-CM3`, `smr_summer_b2w0_rcp45_HadGEM2-CC`,
                                                          smr_summer_b2w0_rcp85_CCSM4, `smr_summer_b2w0_rcp85_GFDL-CM3`, `smr_summer_b2w0_rcp85_HadGEM2-CC`)),
                                                 length(c(smr_summer_b2w1_rcp45_CCSM4, `smr_summer_b2w1_rcp45_GFDL-CM3`, `smr_summer_b2w1_rcp45_HadGEM2-CC`,
                                                          smr_summer_b2w1_rcp85_CCSM4, `smr_summer_b2w1_rcp85_GFDL-CM3`, `smr_summer_b2w1_rcp85_HadGEM2-CC`)),
                                                 length(c(smr_summer_b3w0_rcp45_CCSM4, `smr_summer_b3w0_rcp45_GFDL-CM3`, `smr_summer_b3w0_rcp45_HadGEM2-CC`,
                                                          smr_summer_b3w0_rcp85_CCSM4, `smr_summer_b3w0_rcp85_GFDL-CM3`, `smr_summer_b3w0_rcp85_HadGEM2-CC`)),
                                                 length(c(smr_summer_b3w1_rcp45_CCSM4, `smr_summer_b3w1_rcp45_GFDL-CM3`, `smr_summer_b3w1_rcp45_HadGEM2-CC`,
                                                          smr_summer_b3w1_rcp85_CCSM4, `smr_summer_b3w1_rcp85_GFDL-CM3`, `smr_summer_b3w1_rcp85_HadGEM2-CC`)))),times=2),
                            sce = rep(c('rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                        'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                        'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                        'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                        'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                        'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                        'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                        'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                        'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                        'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                        'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                        'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC'), 
                                      times=c(length(smr_summer_b1w0_rcp45_CCSM4), length(`smr_summer_b1w0_rcp45_GFDL-CM3`), length(`smr_summer_b1w0_rcp45_HadGEM2-CC`),
                                              length(smr_summer_b1w0_rcp85_CCSM4), length(`smr_summer_b1w0_rcp85_GFDL-CM3`), length(`smr_summer_b1w0_rcp85_HadGEM2-CC`),
                                              length(smr_summer_b1w1_rcp45_CCSM4), length(`smr_summer_b1w1_rcp45_GFDL-CM3`), length(`smr_summer_b1w1_rcp45_HadGEM2-CC`),
                                              length(smr_summer_b1w1_rcp85_CCSM4), length(`smr_summer_b1w1_rcp85_GFDL-CM3`), length(`smr_summer_b1w1_rcp85_HadGEM2-CC`),
                                              length(smr_summer_b2w0_rcp45_CCSM4), length(`smr_summer_b2w0_rcp45_GFDL-CM3`), length(`smr_summer_b2w0_rcp45_HadGEM2-CC`),
                                              length(smr_summer_b2w0_rcp85_CCSM4), length(`smr_summer_b2w0_rcp85_GFDL-CM3`), length(`smr_summer_b2w0_rcp85_HadGEM2-CC`),
                                              length(smr_summer_b2w1_rcp45_CCSM4), length(`smr_summer_b2w1_rcp45_GFDL-CM3`), length(`smr_summer_b2w1_rcp45_HadGEM2-CC`),
                                              length(smr_summer_b2w1_rcp85_CCSM4), length(`smr_summer_b2w1_rcp85_GFDL-CM3`), length(`smr_summer_b2w1_rcp85_HadGEM2-CC`),
                                              length(smr_summer_b3w0_rcp45_CCSM4), length(`smr_summer_b3w0_rcp45_GFDL-CM3`), length(`smr_summer_b3w0_rcp45_HadGEM2-CC`),
                                              length(smr_summer_b3w0_rcp85_CCSM4), length(`smr_summer_b3w0_rcp85_GFDL-CM3`), length(`smr_summer_b3w0_rcp85_HadGEM2-CC`),
                                              length(smr_summer_b3w1_rcp45_CCSM4), length(`smr_summer_b3w1_rcp45_GFDL-CM3`), length(`smr_summer_b3w1_rcp45_HadGEM2-CC`),
                                              length(smr_summer_b3w1_rcp85_CCSM4), length(`smr_summer_b3w1_rcp85_GFDL-CM3`), length(`smr_summer_b3w1_rcp85_HadGEM2-CC`))))


smr_winter_cc_50cm <- data.frame(smr=c(smr_winter_b1w0_rcp45_CCSM4_50cm, `smr_winter_b1w0_rcp45_GFDL-CM3_50cm`, `smr_winter_b1w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_winter_b1w0_rcp85_CCSM4_50cm, `smr_winter_b1w0_rcp85_GFDL-CM3_50cm`, `smr_winter_b1w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_winter_b1w1_rcp45_CCSM4_50cm, `smr_winter_b1w1_rcp45_GFDL-CM3_50cm`, `smr_winter_b1w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_winter_b1w1_rcp85_CCSM4_50cm, `smr_winter_b1w1_rcp85_GFDL-CM3_50cm`, `smr_winter_b1w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_winter_b2w0_rcp45_CCSM4_50cm, `smr_winter_b2w0_rcp45_GFDL-CM3_50cm`, `smr_winter_b2w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_winter_b2w0_rcp85_CCSM4_50cm, `smr_winter_b2w0_rcp85_GFDL-CM3_50cm`, `smr_winter_b2w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_winter_b2w1_rcp45_CCSM4_50cm, `smr_winter_b2w1_rcp45_GFDL-CM3_50cm`, `smr_winter_b2w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_winter_b2w1_rcp85_CCSM4_50cm, `smr_winter_b2w1_rcp85_GFDL-CM3_50cm`, `smr_winter_b2w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_winter_b3w0_rcp45_CCSM4_50cm, `smr_winter_b3w0_rcp45_GFDL-CM3_50cm`, `smr_winter_b3w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_winter_b3w0_rcp85_CCSM4_50cm, `smr_winter_b3w0_rcp85_GFDL-CM3_50cm`, `smr_winter_b3w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_winter_b3w1_rcp45_CCSM4_50cm, `smr_winter_b3w1_rcp45_GFDL-CM3_50cm`, `smr_winter_b3w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_winter_b3w1_rcp85_CCSM4_50cm, `smr_winter_b3w1_rcp85_GFDL-CM3_50cm`, `smr_winter_b3w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b1w0_rcp45_CCSM4_50cm, `smr_acc_winter_b1w0_rcp45_GFDL-CM3_50cm`, `smr_acc_winter_b1w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b1w0_rcp85_CCSM4_50cm, `smr_acc_winter_b1w0_rcp85_GFDL-CM3_50cm`, `smr_acc_winter_b1w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b1w1_rcp45_CCSM4_50cm, `smr_acc_winter_b1w1_rcp45_GFDL-CM3_50cm`, `smr_acc_winter_b1w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b1w1_rcp85_CCSM4_50cm, `smr_acc_winter_b1w1_rcp85_GFDL-CM3_50cm`, `smr_acc_winter_b1w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b2w0_rcp45_CCSM4_50cm, `smr_acc_winter_b2w0_rcp45_GFDL-CM3_50cm`, `smr_acc_winter_b2w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b2w0_rcp85_CCSM4_50cm, `smr_acc_winter_b2w0_rcp85_GFDL-CM3_50cm`, `smr_acc_winter_b2w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b2w1_rcp45_CCSM4_50cm, `smr_acc_winter_b2w1_rcp45_GFDL-CM3_50cm`, `smr_acc_winter_b2w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b2w1_rcp85_CCSM4_50cm, `smr_acc_winter_b2w1_rcp85_GFDL-CM3_50cm`, `smr_acc_winter_b2w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b3w0_rcp45_CCSM4_50cm, `smr_acc_winter_b3w0_rcp45_GFDL-CM3_50cm`, `smr_acc_winter_b3w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b3w0_rcp85_CCSM4_50cm, `smr_acc_winter_b3w0_rcp85_GFDL-CM3_50cm`, `smr_acc_winter_b3w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b3w1_rcp45_CCSM4_50cm, `smr_acc_winter_b3w1_rcp45_GFDL-CM3_50cm`, `smr_acc_winter_b3w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b3w1_rcp85_CCSM4_50cm, `smr_acc_winter_b3w1_rcp85_GFDL-CM3_50cm`, `smr_acc_winter_b3w1_rcp85_HadGEM2-CC_50cm`),
                                 acc=rep(c('no acc', 'acc'), each=697752),
                                 behavior=rep(rep(c('cold', 'cold-moist',
                                                    'warm', 'warm-moist',
                                                    'passive', 'passive-moist'), 
                                                  times=c(length(c(smr_winter_b1w0_rcp45_CCSM4_50cm, `smr_winter_b1w0_rcp45_GFDL-CM3_50cm`, `smr_winter_b1w0_rcp45_HadGEM2-CC_50cm`,
                                                                   smr_winter_b1w0_rcp85_CCSM4_50cm, `smr_winter_b1w0_rcp85_GFDL-CM3_50cm`, `smr_winter_b1w0_rcp85_HadGEM2-CC_50cm`)),
                                                          length(c(smr_winter_b1w1_rcp45_CCSM4_50cm, `smr_winter_b1w1_rcp45_GFDL-CM3_50cm`, `smr_winter_b1w1_rcp45_HadGEM2-CC_50cm`,
                                                                   smr_winter_b1w1_rcp85_CCSM4_50cm, `smr_winter_b1w1_rcp85_GFDL-CM3_50cm`, `smr_winter_b1w1_rcp85_HadGEM2-CC_50cm`)),
                                                          length(c(smr_winter_b2w0_rcp45_CCSM4_50cm, `smr_winter_b2w0_rcp45_GFDL-CM3_50cm`, `smr_winter_b2w0_rcp45_HadGEM2-CC_50cm`,
                                                                   smr_winter_b2w0_rcp85_CCSM4_50cm, `smr_winter_b2w0_rcp85_GFDL-CM3_50cm`, `smr_winter_b2w0_rcp85_HadGEM2-CC_50cm`)),
                                                          length(c(smr_winter_b2w1_rcp45_CCSM4_50cm, `smr_winter_b2w1_rcp45_GFDL-CM3_50cm`, `smr_winter_b2w1_rcp45_HadGEM2-CC_50cm`,
                                                                   smr_winter_b2w1_rcp85_CCSM4_50cm, `smr_winter_b2w1_rcp85_GFDL-CM3_50cm`, `smr_winter_b2w1_rcp85_HadGEM2-CC_50cm`)),
                                                          length(c(smr_winter_b3w0_rcp45_CCSM4_50cm, `smr_winter_b3w0_rcp45_GFDL-CM3_50cm`, `smr_winter_b3w0_rcp45_HadGEM2-CC_50cm`,
                                                                   smr_winter_b3w0_rcp85_CCSM4_50cm, `smr_winter_b3w0_rcp85_GFDL-CM3_50cm`, `smr_winter_b3w0_rcp85_HadGEM2-CC_50cm`)),
                                                          length(c(smr_winter_b3w1_rcp45_CCSM4_50cm, `smr_winter_b3w1_rcp45_GFDL-CM3_50cm`, `smr_winter_b3w1_rcp45_HadGEM2-CC_50cm`,
                                                                   smr_winter_b3w1_rcp85_CCSM4_50cm, `smr_winter_b3w1_rcp85_GFDL-CM3_50cm`, `smr_winter_b3w1_rcp85_HadGEM2-CC_50cm`)))),times=2),
                                 sce = rep(c('rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                             'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                             'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                             'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                             'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                             'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                             'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                             'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                             'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                             'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                             'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                             'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC'), 
                                           times=c(length(smr_winter_b1w0_rcp45_CCSM4_50cm), length(`smr_winter_b1w0_rcp45_GFDL-CM3_50cm`), length(`smr_winter_b1w0_rcp45_HadGEM2-CC_50cm`),
                                                   length(smr_winter_b1w0_rcp85_CCSM4_50cm), length(`smr_winter_b1w0_rcp85_GFDL-CM3_50cm`), length(`smr_winter_b1w0_rcp85_HadGEM2-CC_50cm`),
                                                   length(smr_winter_b1w1_rcp45_CCSM4_50cm), length(`smr_winter_b1w1_rcp45_GFDL-CM3_50cm`), length(`smr_winter_b1w1_rcp45_HadGEM2-CC_50cm`),
                                                   length(smr_winter_b1w1_rcp85_CCSM4_50cm), length(`smr_winter_b1w1_rcp85_GFDL-CM3_50cm`), length(`smr_winter_b1w1_rcp85_HadGEM2-CC_50cm`),
                                                   length(smr_winter_b2w0_rcp45_CCSM4_50cm), length(`smr_winter_b2w0_rcp45_GFDL-CM3_50cm`), length(`smr_winter_b2w0_rcp45_HadGEM2-CC_50cm`),
                                                   length(smr_winter_b2w0_rcp85_CCSM4_50cm), length(`smr_winter_b2w0_rcp85_GFDL-CM3_50cm`), length(`smr_winter_b2w0_rcp85_HadGEM2-CC_50cm`),
                                                   length(smr_winter_b2w1_rcp45_CCSM4_50cm), length(`smr_winter_b2w1_rcp45_GFDL-CM3_50cm`), length(`smr_winter_b2w1_rcp45_HadGEM2-CC_50cm`),
                                                   length(smr_winter_b2w1_rcp85_CCSM4_50cm), length(`smr_winter_b2w1_rcp85_GFDL-CM3_50cm`), length(`smr_winter_b2w1_rcp85_HadGEM2-CC_50cm`),
                                                   length(smr_winter_b3w0_rcp45_CCSM4_50cm), length(`smr_winter_b3w0_rcp45_GFDL-CM3_50cm`), length(`smr_winter_b3w0_rcp45_HadGEM2-CC_50cm`),
                                                   length(smr_winter_b3w0_rcp85_CCSM4_50cm), length(`smr_winter_b3w0_rcp85_GFDL-CM3_50cm`), length(`smr_winter_b3w0_rcp85_HadGEM2-CC_50cm`),
                                                   length(smr_winter_b3w1_rcp45_CCSM4_50cm), length(`smr_winter_b3w1_rcp45_GFDL-CM3_50cm`), length(`smr_winter_b3w1_rcp45_HadGEM2-CC_50cm`),
                                                   length(smr_winter_b3w1_rcp85_CCSM4_50cm), length(`smr_winter_b3w1_rcp85_GFDL-CM3_50cm`), length(`smr_winter_b3w1_rcp85_HadGEM2-CC_50cm`))))



smr_summer_cc_50cm <- data.frame(smr=c(smr_summer_b1w0_rcp45_CCSM4_50cm, `smr_summer_b1w0_rcp45_GFDL-CM3_50cm`, `smr_summer_b1w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_summer_b1w0_rcp85_CCSM4_50cm, `smr_summer_b1w0_rcp85_GFDL-CM3_50cm`, `smr_summer_b1w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_summer_b1w1_rcp45_CCSM4_50cm, `smr_summer_b1w1_rcp45_GFDL-CM3_50cm`, `smr_summer_b1w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_summer_b1w1_rcp85_CCSM4_50cm, `smr_summer_b1w1_rcp85_GFDL-CM3_50cm`, `smr_summer_b1w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_summer_b2w0_rcp45_CCSM4_50cm, `smr_summer_b2w0_rcp45_GFDL-CM3_50cm`, `smr_summer_b2w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_summer_b2w0_rcp85_CCSM4_50cm, `smr_summer_b2w0_rcp85_GFDL-CM3_50cm`, `smr_summer_b2w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_summer_b2w1_rcp45_CCSM4_50cm, `smr_summer_b2w1_rcp45_GFDL-CM3_50cm`, `smr_summer_b2w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_summer_b2w1_rcp85_CCSM4_50cm, `smr_summer_b2w1_rcp85_GFDL-CM3_50cm`, `smr_summer_b2w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_summer_b3w0_rcp45_CCSM4_50cm, `smr_summer_b3w0_rcp45_GFDL-CM3_50cm`, `smr_summer_b3w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_summer_b3w0_rcp85_CCSM4_50cm, `smr_summer_b3w0_rcp85_GFDL-CM3_50cm`, `smr_summer_b3w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_summer_b3w1_rcp45_CCSM4_50cm, `smr_summer_b3w1_rcp45_GFDL-CM3_50cm`, `smr_summer_b3w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_summer_b3w1_rcp85_CCSM4_50cm, `smr_summer_b3w1_rcp85_GFDL-CM3_50cm`, `smr_summer_b3w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b1w0_rcp45_CCSM4_50cm, `smr_acc_summer_b1w0_rcp45_GFDL-CM3_50cm`, `smr_acc_summer_b1w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b1w0_rcp85_CCSM4_50cm, `smr_acc_summer_b1w0_rcp85_GFDL-CM3_50cm`, `smr_acc_summer_b1w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b1w1_rcp45_CCSM4_50cm, `smr_acc_summer_b1w1_rcp45_GFDL-CM3_50cm`, `smr_acc_summer_b1w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b1w1_rcp85_CCSM4_50cm, `smr_acc_summer_b1w1_rcp85_GFDL-CM3_50cm`, `smr_acc_summer_b1w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b2w0_rcp45_CCSM4_50cm, `smr_acc_summer_b2w0_rcp45_GFDL-CM3_50cm`, `smr_acc_summer_b2w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b2w0_rcp85_CCSM4_50cm, `smr_acc_summer_b2w0_rcp85_GFDL-CM3_50cm`, `smr_acc_summer_b2w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b2w1_rcp45_CCSM4_50cm, `smr_acc_summer_b2w1_rcp45_GFDL-CM3_50cm`, `smr_acc_summer_b2w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b2w1_rcp85_CCSM4_50cm, `smr_acc_summer_b2w1_rcp85_GFDL-CM3_50cm`, `smr_acc_summer_b2w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b3w0_rcp45_CCSM4_50cm, `smr_acc_summer_b3w0_rcp45_GFDL-CM3_50cm`, `smr_acc_summer_b3w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b3w0_rcp85_CCSM4_50cm, `smr_acc_summer_b3w0_rcp85_GFDL-CM3_50cm`, `smr_acc_summer_b3w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b3w1_rcp45_CCSM4_50cm, `smr_acc_summer_b3w1_rcp45_GFDL-CM3_50cm`, `smr_acc_summer_b3w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b3w1_rcp85_CCSM4_50cm, `smr_acc_summer_b3w1_rcp85_GFDL-CM3_50cm`, `smr_acc_summer_b3w1_rcp85_HadGEM2-CC_50cm`),
                                 acc=rep(c('no acc', 'acc'), each=601436),
                                 behavior=rep(rep(c('cold', 'cold-moist',
                                                    'warm', 'warm-moist',
                                                    'passive', 'passive-moist'), 
                                                  times=c(length(c(smr_summer_b1w0_rcp45_CCSM4_50cm, `smr_summer_b1w0_rcp45_GFDL-CM3_50cm`, `smr_summer_b1w0_rcp45_HadGEM2-CC_50cm`,
                                                                   smr_summer_b1w0_rcp85_CCSM4_50cm, `smr_summer_b1w0_rcp85_GFDL-CM3_50cm`, `smr_summer_b1w0_rcp85_HadGEM2-CC_50cm`)),
                                                          length(c(smr_summer_b1w1_rcp45_CCSM4_50cm, `smr_summer_b1w1_rcp45_GFDL-CM3_50cm`, `smr_summer_b1w1_rcp45_HadGEM2-CC_50cm`,
                                                                   smr_summer_b1w1_rcp85_CCSM4_50cm, `smr_summer_b1w1_rcp85_GFDL-CM3_50cm`, `smr_summer_b1w1_rcp85_HadGEM2-CC_50cm`)),
                                                          length(c(smr_summer_b2w0_rcp45_CCSM4_50cm, `smr_summer_b2w0_rcp45_GFDL-CM3_50cm`, `smr_summer_b2w0_rcp45_HadGEM2-CC_50cm`,
                                                                   smr_summer_b2w0_rcp85_CCSM4_50cm, `smr_summer_b2w0_rcp85_GFDL-CM3_50cm`, `smr_summer_b2w0_rcp85_HadGEM2-CC_50cm`)),
                                                          length(c(smr_summer_b2w1_rcp45_CCSM4_50cm, `smr_summer_b2w1_rcp45_GFDL-CM3_50cm`, `smr_summer_b2w1_rcp45_HadGEM2-CC_50cm`,
                                                                   smr_summer_b2w1_rcp85_CCSM4_50cm, `smr_summer_b2w1_rcp85_GFDL-CM3_50cm`, `smr_summer_b2w1_rcp85_HadGEM2-CC_50cm`)),
                                                          length(c(smr_summer_b3w0_rcp45_CCSM4_50cm, `smr_summer_b3w0_rcp45_GFDL-CM3_50cm`, `smr_summer_b3w0_rcp45_HadGEM2-CC_50cm`,
                                                                   smr_summer_b3w0_rcp85_CCSM4_50cm, `smr_summer_b3w0_rcp85_GFDL-CM3_50cm`, `smr_summer_b3w0_rcp85_HadGEM2-CC_50cm`)),
                                                          length(c(smr_summer_b3w1_rcp45_CCSM4_50cm, `smr_summer_b3w1_rcp45_GFDL-CM3_50cm`, `smr_summer_b3w1_rcp45_HadGEM2-CC_50cm`,
                                                                   smr_summer_b3w1_rcp85_CCSM4_50cm, `smr_summer_b3w1_rcp85_GFDL-CM3_50cm`, `smr_summer_b3w1_rcp85_HadGEM2-CC_50cm`)))),times=2),
                                 sce = rep(c('rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                             'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                             'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                             'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                             'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                             'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                             'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                             'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                             'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                             'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                             'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                             'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC'), 
                                           times=c(length(smr_summer_b1w0_rcp45_CCSM4_50cm), length(`smr_summer_b1w0_rcp45_GFDL-CM3_50cm`), length(`smr_summer_b1w0_rcp45_HadGEM2-CC_50cm`),
                                                   length(smr_summer_b1w0_rcp85_CCSM4_50cm), length(`smr_summer_b1w0_rcp85_GFDL-CM3_50cm`), length(`smr_summer_b1w0_rcp85_HadGEM2-CC_50cm`),
                                                   length(smr_summer_b1w1_rcp45_CCSM4_50cm), length(`smr_summer_b1w1_rcp45_GFDL-CM3_50cm`), length(`smr_summer_b1w1_rcp45_HadGEM2-CC_50cm`),
                                                   length(smr_summer_b1w1_rcp85_CCSM4_50cm), length(`smr_summer_b1w1_rcp85_GFDL-CM3_50cm`), length(`smr_summer_b1w1_rcp85_HadGEM2-CC_50cm`),
                                                   length(smr_summer_b2w0_rcp45_CCSM4_50cm), length(`smr_summer_b2w0_rcp45_GFDL-CM3_50cm`), length(`smr_summer_b2w0_rcp45_HadGEM2-CC_50cm`),
                                                   length(smr_summer_b2w0_rcp85_CCSM4_50cm), length(`smr_summer_b2w0_rcp85_GFDL-CM3_50cm`), length(`smr_summer_b2w0_rcp85_HadGEM2-CC_50cm`),
                                                   length(smr_summer_b2w1_rcp45_CCSM4_50cm), length(`smr_summer_b2w1_rcp45_GFDL-CM3_50cm`), length(`smr_summer_b2w1_rcp45_HadGEM2-CC_50cm`),
                                                   length(smr_summer_b2w1_rcp85_CCSM4_50cm), length(`smr_summer_b2w1_rcp85_GFDL-CM3_50cm`), length(`smr_summer_b2w1_rcp85_HadGEM2-CC_50cm`),
                                                   length(smr_summer_b3w0_rcp45_CCSM4_50cm), length(`smr_summer_b3w0_rcp45_GFDL-CM3_50cm`), length(`smr_summer_b3w0_rcp45_HadGEM2-CC_50cm`),
                                                   length(smr_summer_b3w0_rcp85_CCSM4_50cm), length(`smr_summer_b3w0_rcp85_GFDL-CM3_50cm`), length(`smr_summer_b3w0_rcp85_HadGEM2-CC_50cm`),
                                                   length(smr_summer_b3w1_rcp45_CCSM4_50cm), length(`smr_summer_b3w1_rcp45_GFDL-CM3_50cm`), length(`smr_summer_b3w1_rcp45_HadGEM2-CC_50cm`),
                                                   length(smr_summer_b3w1_rcp85_CCSM4_50cm), length(`smr_summer_b3w1_rcp85_GFDL-CM3_50cm`), length(`smr_summer_b3w1_rcp85_HadGEM2-CC_50cm`))))



depth_winter_cc <- data.frame(depth=c(depth_winter_b1w0_rcp45_CCSM4, `depth_winter_b1w0_rcp45_GFDL-CM3`, `depth_winter_b1w0_rcp45_HadGEM2-CC`,
                                      depth_winter_b1w0_rcp85_CCSM4, `depth_winter_b1w0_rcp85_GFDL-CM3`, `depth_winter_b1w0_rcp85_HadGEM2-CC`,
                                      depth_winter_b1w1_rcp45_CCSM4, `depth_winter_b1w1_rcp45_GFDL-CM3`, `depth_winter_b1w1_rcp45_HadGEM2-CC`,
                                      depth_winter_b1w1_rcp85_CCSM4, `depth_winter_b1w1_rcp85_GFDL-CM3`, `depth_winter_b1w1_rcp85_HadGEM2-CC`,
                                      depth_winter_b2w0_rcp45_CCSM4, `depth_winter_b2w0_rcp45_GFDL-CM3`, `depth_winter_b2w0_rcp45_HadGEM2-CC`,
                                      depth_winter_b2w0_rcp85_CCSM4, `depth_winter_b2w0_rcp85_GFDL-CM3`, `depth_winter_b2w0_rcp85_HadGEM2-CC`,
                                      depth_winter_b2w1_rcp45_CCSM4, `depth_winter_b2w1_rcp45_GFDL-CM3`, `depth_winter_b2w1_rcp45_HadGEM2-CC`,
                                      depth_winter_b2w1_rcp85_CCSM4, `depth_winter_b2w1_rcp85_GFDL-CM3`, `depth_winter_b2w1_rcp85_HadGEM2-CC`,
                                      depth_winter_b3w0_rcp45_CCSM4, `depth_winter_b3w0_rcp45_GFDL-CM3`, `depth_winter_b3w0_rcp45_HadGEM2-CC`,
                                      depth_winter_b3w0_rcp85_CCSM4, `depth_winter_b3w0_rcp85_GFDL-CM3`, `depth_winter_b3w0_rcp85_HadGEM2-CC`,
                                      depth_winter_b3w1_rcp45_CCSM4, `depth_winter_b3w1_rcp45_GFDL-CM3`, `depth_winter_b3w1_rcp45_HadGEM2-CC`,
                                      depth_winter_b3w1_rcp85_CCSM4, `depth_winter_b3w1_rcp85_GFDL-CM3`, `depth_winter_b3w1_rcp85_HadGEM2-CC`),
                              behavior=rep(c('cold', 'cold-moist',
                                             'warm', 'warm-moist',
                                             'passive', 'passive-moist'), 
                                           times=c(length(c(depth_winter_b1w0_rcp45_CCSM4, `depth_winter_b1w0_rcp45_GFDL-CM3`, `depth_winter_b1w0_rcp45_HadGEM2-CC`,
                                                            depth_winter_b1w0_rcp85_CCSM4, `depth_winter_b1w0_rcp85_GFDL-CM3`, `depth_winter_b1w0_rcp85_HadGEM2-CC`)),
                                                   length(c(depth_winter_b1w1_rcp45_CCSM4, `depth_winter_b1w1_rcp45_GFDL-CM3`, `depth_winter_b1w1_rcp45_HadGEM2-CC`,
                                                            depth_winter_b1w1_rcp85_CCSM4, `depth_winter_b1w1_rcp85_GFDL-CM3`, `depth_winter_b1w1_rcp85_HadGEM2-CC`)),
                                                   length(c(depth_winter_b2w0_rcp45_CCSM4, `depth_winter_b2w0_rcp45_GFDL-CM3`, `depth_winter_b2w0_rcp45_HadGEM2-CC`,
                                                            depth_winter_b2w0_rcp85_CCSM4, `depth_winter_b2w0_rcp85_GFDL-CM3`, `depth_winter_b2w0_rcp85_HadGEM2-CC`)),
                                                   length(c(depth_winter_b2w1_rcp45_CCSM4, `depth_winter_b2w1_rcp45_GFDL-CM3`, `depth_winter_b2w1_rcp45_HadGEM2-CC`,
                                                            depth_winter_b2w1_rcp85_CCSM4, `depth_winter_b2w1_rcp85_GFDL-CM3`, `depth_winter_b2w1_rcp85_HadGEM2-CC`)),
                                                   length(c(depth_winter_b3w0_rcp45_CCSM4, `depth_winter_b3w0_rcp45_GFDL-CM3`, `depth_winter_b3w0_rcp45_HadGEM2-CC`,
                                                            depth_winter_b3w0_rcp85_CCSM4, `depth_winter_b3w0_rcp85_GFDL-CM3`, `depth_winter_b3w0_rcp85_HadGEM2-CC`)),
                                                   length(c(depth_winter_b3w1_rcp45_CCSM4, `depth_winter_b3w1_rcp45_GFDL-CM3`, `depth_winter_b3w1_rcp45_HadGEM2-CC`,
                                                            depth_winter_b3w1_rcp85_CCSM4, `depth_winter_b3w1_rcp85_GFDL-CM3`, `depth_winter_b3w1_rcp85_HadGEM2-CC`)))),
                              sce = rep(c('rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                          'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                          'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                          'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                          'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                          'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                          'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                          'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                          'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                          'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                          'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                          'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC'), 
                                        times=c(length(depth_winter_b1w0_rcp45_CCSM4), length(`depth_winter_b1w0_rcp45_GFDL-CM3`), length(`depth_winter_b1w0_rcp45_HadGEM2-CC`),
                                                length(depth_winter_b1w0_rcp85_CCSM4), length(`depth_winter_b1w0_rcp85_GFDL-CM3`), length(`depth_winter_b1w0_rcp85_HadGEM2-CC`),
                                                length(depth_winter_b1w1_rcp45_CCSM4), length(`depth_winter_b1w1_rcp45_GFDL-CM3`), length(`depth_winter_b1w1_rcp45_HadGEM2-CC`),
                                                length(depth_winter_b1w1_rcp85_CCSM4), length(`depth_winter_b1w1_rcp85_GFDL-CM3`), length(`depth_winter_b1w1_rcp85_HadGEM2-CC`),
                                                length(depth_winter_b2w0_rcp45_CCSM4), length(`depth_winter_b2w0_rcp45_GFDL-CM3`), length(`depth_winter_b2w0_rcp45_HadGEM2-CC`),
                                                length(depth_winter_b2w0_rcp85_CCSM4), length(`depth_winter_b2w0_rcp85_GFDL-CM3`), length(`depth_winter_b2w0_rcp85_HadGEM2-CC`),
                                                length(depth_winter_b2w1_rcp45_CCSM4), length(`depth_winter_b2w1_rcp45_GFDL-CM3`), length(`depth_winter_b2w1_rcp45_HadGEM2-CC`),
                                                length(depth_winter_b2w1_rcp85_CCSM4), length(`depth_winter_b2w1_rcp85_GFDL-CM3`), length(`depth_winter_b2w1_rcp85_HadGEM2-CC`),
                                                length(depth_winter_b3w0_rcp45_CCSM4), length(`depth_winter_b3w0_rcp45_GFDL-CM3`), length(`depth_winter_b3w0_rcp45_HadGEM2-CC`),
                                                length(depth_winter_b3w0_rcp85_CCSM4), length(`depth_winter_b3w0_rcp85_GFDL-CM3`), length(`depth_winter_b3w0_rcp85_HadGEM2-CC`),
                                                length(depth_winter_b3w1_rcp45_CCSM4), length(`depth_winter_b3w1_rcp45_GFDL-CM3`), length(`depth_winter_b3w1_rcp45_HadGEM2-CC`),
                                                length(depth_winter_b3w1_rcp85_CCSM4), length(`depth_winter_b3w1_rcp85_GFDL-CM3`), length(`depth_winter_b3w1_rcp85_HadGEM2-CC`))))



depth_summer_cc <- data.frame(depth=c(depth_summer_b1w0_rcp45_CCSM4, `depth_summer_b1w0_rcp45_GFDL-CM3`, `depth_summer_b1w0_rcp45_HadGEM2-CC`,
                                      depth_summer_b1w0_rcp85_CCSM4, `depth_summer_b1w0_rcp85_GFDL-CM3`, `depth_summer_b1w0_rcp85_HadGEM2-CC`,
                                      depth_summer_b1w1_rcp45_CCSM4, `depth_summer_b1w1_rcp45_GFDL-CM3`, `depth_summer_b1w1_rcp45_HadGEM2-CC`,
                                      depth_summer_b1w1_rcp85_CCSM4, `depth_summer_b1w1_rcp85_GFDL-CM3`, `depth_summer_b1w1_rcp85_HadGEM2-CC`,
                                      depth_summer_b2w0_rcp45_CCSM4, `depth_summer_b2w0_rcp45_GFDL-CM3`, `depth_summer_b2w0_rcp45_HadGEM2-CC`,
                                      depth_summer_b2w0_rcp85_CCSM4, `depth_summer_b2w0_rcp85_GFDL-CM3`, `depth_summer_b2w0_rcp85_HadGEM2-CC`,
                                      depth_summer_b2w1_rcp45_CCSM4, `depth_summer_b2w1_rcp45_GFDL-CM3`, `depth_summer_b2w1_rcp45_HadGEM2-CC`,
                                      depth_summer_b2w1_rcp85_CCSM4, `depth_summer_b2w1_rcp85_GFDL-CM3`, `depth_summer_b2w1_rcp85_HadGEM2-CC`,
                                      depth_summer_b3w0_rcp45_CCSM4, `depth_summer_b3w0_rcp45_GFDL-CM3`, `depth_summer_b3w0_rcp45_HadGEM2-CC`,
                                      depth_summer_b3w0_rcp85_CCSM4, `depth_summer_b3w0_rcp85_GFDL-CM3`, `depth_summer_b3w0_rcp85_HadGEM2-CC`,
                                      depth_summer_b3w1_rcp45_CCSM4, `depth_summer_b3w1_rcp45_GFDL-CM3`, `depth_summer_b3w1_rcp45_HadGEM2-CC`,
                                      depth_summer_b3w1_rcp85_CCSM4, `depth_summer_b3w1_rcp85_GFDL-CM3`, `depth_summer_b3w1_rcp85_HadGEM2-CC`),
                              behavior=rep(c('cold', 'cold-moist',
                                             'warm', 'warm-moist',
                                             'passive', 'passive-moist'), 
                                           times=c(length(c(depth_summer_b1w0_rcp45_CCSM4, `depth_summer_b1w0_rcp45_GFDL-CM3`, `depth_summer_b1w0_rcp45_HadGEM2-CC`,
                                                            depth_summer_b1w0_rcp85_CCSM4, `depth_summer_b1w0_rcp85_GFDL-CM3`, `depth_summer_b1w0_rcp85_HadGEM2-CC`)),
                                                   length(c(depth_summer_b1w1_rcp45_CCSM4, `depth_summer_b1w1_rcp45_GFDL-CM3`, `depth_summer_b1w1_rcp45_HadGEM2-CC`,
                                                            depth_summer_b1w1_rcp85_CCSM4, `depth_summer_b1w1_rcp85_GFDL-CM3`, `depth_summer_b1w1_rcp85_HadGEM2-CC`)),
                                                   length(c(depth_summer_b2w0_rcp45_CCSM4, `depth_summer_b2w0_rcp45_GFDL-CM3`, `depth_summer_b2w0_rcp45_HadGEM2-CC`,
                                                            depth_summer_b2w0_rcp85_CCSM4, `depth_summer_b2w0_rcp85_GFDL-CM3`, `depth_summer_b2w0_rcp85_HadGEM2-CC`)),
                                                   length(c(depth_summer_b2w1_rcp45_CCSM4, `depth_summer_b2w1_rcp45_GFDL-CM3`, `depth_summer_b2w1_rcp45_HadGEM2-CC`,
                                                            depth_summer_b2w1_rcp85_CCSM4, `depth_summer_b2w1_rcp85_GFDL-CM3`, `depth_summer_b2w1_rcp85_HadGEM2-CC`)),
                                                   length(c(depth_summer_b3w0_rcp45_CCSM4, `depth_summer_b3w0_rcp45_GFDL-CM3`, `depth_summer_b3w0_rcp45_HadGEM2-CC`,
                                                            depth_summer_b3w0_rcp85_CCSM4, `depth_summer_b3w0_rcp85_GFDL-CM3`, `depth_summer_b3w0_rcp85_HadGEM2-CC`)),
                                                   length(c(depth_summer_b3w1_rcp45_CCSM4, `depth_summer_b3w1_rcp45_GFDL-CM3`, `depth_summer_b3w1_rcp45_HadGEM2-CC`,
                                                            depth_summer_b3w1_rcp85_CCSM4, `depth_summer_b3w1_rcp85_GFDL-CM3`, `depth_summer_b3w1_rcp85_HadGEM2-CC`)))),
                              sce = rep(c('rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                          'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                          'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                          'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                          'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                          'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                          'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                          'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                          'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                          'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                          'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                          'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC'), 
                                        times=c(length(depth_summer_b1w0_rcp45_CCSM4), length(`depth_summer_b1w0_rcp45_GFDL-CM3`), length(`depth_summer_b1w0_rcp45_HadGEM2-CC`),
                                                length(depth_summer_b1w0_rcp85_CCSM4), length(`depth_summer_b1w0_rcp85_GFDL-CM3`), length(`depth_summer_b1w0_rcp85_HadGEM2-CC`),
                                                length(depth_summer_b1w1_rcp45_CCSM4), length(`depth_summer_b1w1_rcp45_GFDL-CM3`), length(`depth_summer_b1w1_rcp45_HadGEM2-CC`),
                                                length(depth_summer_b1w1_rcp85_CCSM4), length(`depth_summer_b1w1_rcp85_GFDL-CM3`), length(`depth_summer_b1w1_rcp85_HadGEM2-CC`),
                                                length(depth_summer_b2w0_rcp45_CCSM4), length(`depth_summer_b2w0_rcp45_GFDL-CM3`), length(`depth_summer_b2w0_rcp45_HadGEM2-CC`),
                                                length(depth_summer_b2w0_rcp85_CCSM4), length(`depth_summer_b2w0_rcp85_GFDL-CM3`), length(`depth_summer_b2w0_rcp85_HadGEM2-CC`),
                                                length(depth_summer_b2w1_rcp45_CCSM4), length(`depth_summer_b2w1_rcp45_GFDL-CM3`), length(`depth_summer_b2w1_rcp45_HadGEM2-CC`),
                                                length(depth_summer_b2w1_rcp85_CCSM4), length(`depth_summer_b2w1_rcp85_GFDL-CM3`), length(`depth_summer_b2w1_rcp85_HadGEM2-CC`),
                                                length(depth_summer_b3w0_rcp45_CCSM4), length(`depth_summer_b3w0_rcp45_GFDL-CM3`), length(`depth_summer_b3w0_rcp45_HadGEM2-CC`),
                                                length(depth_summer_b3w0_rcp85_CCSM4), length(`depth_summer_b3w0_rcp85_GFDL-CM3`), length(`depth_summer_b3w0_rcp85_HadGEM2-CC`),
                                                length(depth_summer_b3w1_rcp45_CCSM4), length(`depth_summer_b3w1_rcp45_GFDL-CM3`), length(`depth_summer_b3w1_rcp45_HadGEM2-CC`),
                                                length(depth_summer_b3w1_rcp85_CCSM4), length(`depth_summer_b3w1_rcp85_GFDL-CM3`), length(`depth_summer_b3w1_rcp85_HadGEM2-CC`))))



depth_winter_cc_50cm <- data.frame(depth=c(depth_winter_b1w0_rcp45_CCSM4_50cm, `depth_winter_b1w0_rcp45_GFDL-CM3_50cm`, `depth_winter_b1w0_rcp45_HadGEM2-CC_50cm`,
                                           depth_winter_b1w0_rcp85_CCSM4_50cm, `depth_winter_b1w0_rcp85_GFDL-CM3_50cm`, `depth_winter_b1w0_rcp85_HadGEM2-CC_50cm`,
                                           depth_winter_b1w1_rcp45_CCSM4_50cm, `depth_winter_b1w1_rcp45_GFDL-CM3_50cm`, `depth_winter_b1w1_rcp45_HadGEM2-CC_50cm`,
                                           depth_winter_b1w1_rcp85_CCSM4_50cm, `depth_winter_b1w1_rcp85_GFDL-CM3_50cm`, `depth_winter_b1w1_rcp85_HadGEM2-CC_50cm`,
                                           depth_winter_b2w0_rcp45_CCSM4_50cm, `depth_winter_b2w0_rcp45_GFDL-CM3_50cm`, `depth_winter_b2w0_rcp45_HadGEM2-CC_50cm`,
                                           depth_winter_b2w0_rcp85_CCSM4_50cm, `depth_winter_b2w0_rcp85_GFDL-CM3_50cm`, `depth_winter_b2w0_rcp85_HadGEM2-CC_50cm`,
                                           depth_winter_b2w1_rcp45_CCSM4_50cm, `depth_winter_b2w1_rcp45_GFDL-CM3_50cm`, `depth_winter_b2w1_rcp45_HadGEM2-CC_50cm`,
                                           depth_winter_b2w1_rcp85_CCSM4_50cm, `depth_winter_b2w1_rcp85_GFDL-CM3_50cm`, `depth_winter_b2w1_rcp85_HadGEM2-CC_50cm`,
                                           depth_winter_b3w0_rcp45_CCSM4_50cm, `depth_winter_b3w0_rcp45_GFDL-CM3_50cm`, `depth_winter_b3w0_rcp45_HadGEM2-CC_50cm`,
                                           depth_winter_b3w0_rcp85_CCSM4_50cm, `depth_winter_b3w0_rcp85_GFDL-CM3_50cm`, `depth_winter_b3w0_rcp85_HadGEM2-CC_50cm`,
                                           depth_winter_b3w1_rcp45_CCSM4_50cm, `depth_winter_b3w1_rcp45_GFDL-CM3_50cm`, `depth_winter_b3w1_rcp45_HadGEM2-CC_50cm`,
                                           depth_winter_b3w1_rcp85_CCSM4_50cm, `depth_winter_b3w1_rcp85_GFDL-CM3_50cm`, `depth_winter_b3w1_rcp85_HadGEM2-CC_50cm`),
                                   behavior=rep(c('cold', 'cold-moist',
                                                  'warm', 'warm-moist',
                                                  'passive', 'passive-moist'), 
                                                times=c(length(c(depth_winter_b1w0_rcp45_CCSM4_50cm, `depth_winter_b1w0_rcp45_GFDL-CM3_50cm`, `depth_winter_b1w0_rcp45_HadGEM2-CC_50cm`,
                                                                 depth_winter_b1w0_rcp85_CCSM4_50cm, `depth_winter_b1w0_rcp85_GFDL-CM3_50cm`, `depth_winter_b1w0_rcp85_HadGEM2-CC_50cm`)),
                                                        length(c(depth_winter_b1w1_rcp45_CCSM4_50cm, `depth_winter_b1w1_rcp45_GFDL-CM3_50cm`, `depth_winter_b1w1_rcp45_HadGEM2-CC_50cm`,
                                                                 depth_winter_b1w1_rcp85_CCSM4_50cm, `depth_winter_b1w1_rcp85_GFDL-CM3_50cm`, `depth_winter_b1w1_rcp85_HadGEM2-CC_50cm`)),
                                                        length(c(depth_winter_b2w0_rcp45_CCSM4_50cm, `depth_winter_b2w0_rcp45_GFDL-CM3_50cm`, `depth_winter_b2w0_rcp45_HadGEM2-CC_50cm`,
                                                                 depth_winter_b2w0_rcp85_CCSM4_50cm, `depth_winter_b2w0_rcp85_GFDL-CM3_50cm`, `depth_winter_b2w0_rcp85_HadGEM2-CC_50cm`)),
                                                        length(c(depth_winter_b2w1_rcp45_CCSM4_50cm, `depth_winter_b2w1_rcp45_GFDL-CM3_50cm`, `depth_winter_b2w1_rcp45_HadGEM2-CC_50cm`,
                                                                 depth_winter_b2w1_rcp85_CCSM4_50cm, `depth_winter_b2w1_rcp85_GFDL-CM3_50cm`, `depth_winter_b2w1_rcp85_HadGEM2-CC_50cm`)),
                                                        length(c(depth_winter_b3w0_rcp45_CCSM4_50cm, `depth_winter_b3w0_rcp45_GFDL-CM3_50cm`, `depth_winter_b3w0_rcp45_HadGEM2-CC_50cm`,
                                                                 depth_winter_b3w0_rcp85_CCSM4_50cm, `depth_winter_b3w0_rcp85_GFDL-CM3_50cm`, `depth_winter_b3w0_rcp85_HadGEM2-CC_50cm`)),
                                                        length(c(depth_winter_b3w1_rcp45_CCSM4_50cm, `depth_winter_b3w1_rcp45_GFDL-CM3_50cm`, `depth_winter_b3w1_rcp45_HadGEM2-CC_50cm`,
                                                                 depth_winter_b3w1_rcp85_CCSM4_50cm, `depth_winter_b3w1_rcp85_GFDL-CM3_50cm`, `depth_winter_b3w1_rcp85_HadGEM2-CC_50cm`)))),
                                   sce = rep(c('rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                               'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                               'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                               'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                               'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                               'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                               'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                               'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                               'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                               'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC',
                                               'rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                               'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC'), 
                                             times=c(length(depth_winter_b1w0_rcp45_CCSM4_50cm), length(`depth_winter_b1w0_rcp45_GFDL-CM3_50cm`), length(`depth_winter_b1w0_rcp45_HadGEM2-CC_50cm`),
                                                     length(depth_winter_b1w0_rcp85_CCSM4_50cm), length(`depth_winter_b1w0_rcp85_GFDL-CM3_50cm`), length(`depth_winter_b1w0_rcp85_HadGEM2-CC_50cm`),
                                                     length(depth_winter_b1w1_rcp45_CCSM4_50cm), length(`depth_winter_b1w1_rcp45_GFDL-CM3_50cm`), length(`depth_winter_b1w1_rcp45_HadGEM2-CC_50cm`),
                                                     length(depth_winter_b1w1_rcp85_CCSM4_50cm), length(`depth_winter_b1w1_rcp85_GFDL-CM3_50cm`), length(`depth_winter_b1w1_rcp85_HadGEM2-CC_50cm`),
                                                     length(depth_winter_b2w0_rcp45_CCSM4_50cm), length(`depth_winter_b2w0_rcp45_GFDL-CM3_50cm`), length(`depth_winter_b2w0_rcp45_HadGEM2-CC_50cm`),
                                                     length(depth_winter_b2w0_rcp85_CCSM4_50cm), length(`depth_winter_b2w0_rcp85_GFDL-CM3_50cm`), length(`depth_winter_b2w0_rcp85_HadGEM2-CC_50cm`),
                                                     length(depth_winter_b2w1_rcp45_CCSM4_50cm), length(`depth_winter_b2w1_rcp45_GFDL-CM3_50cm`), length(`depth_winter_b2w1_rcp45_HadGEM2-CC_50cm`),
                                                     length(depth_winter_b2w1_rcp85_CCSM4_50cm), length(`depth_winter_b2w1_rcp85_GFDL-CM3_50cm`), length(`depth_winter_b2w1_rcp85_HadGEM2-CC_50cm`),
                                                     length(depth_winter_b3w0_rcp45_CCSM4_50cm), length(`depth_winter_b3w0_rcp45_GFDL-CM3_50cm`), length(`depth_winter_b3w0_rcp45_HadGEM2-CC_50cm`),
                                                     length(depth_winter_b3w0_rcp85_CCSM4_50cm), length(`depth_winter_b3w0_rcp85_GFDL-CM3_50cm`), length(`depth_winter_b3w0_rcp85_HadGEM2-CC_50cm`),
                                                     length(depth_winter_b3w1_rcp45_CCSM4_50cm), length(`depth_winter_b3w1_rcp45_GFDL-CM3_50cm`), length(`depth_winter_b3w1_rcp45_HadGEM2-CC_50cm`),
                                                     length(depth_winter_b3w1_rcp85_CCSM4_50cm), length(`depth_winter_b3w1_rcp85_GFDL-CM3_50cm`), length(`depth_winter_b3w1_rcp85_HadGEM2-CC_50cm`))))



depth_summer_cc_50cm <- data.frame(depth=c(depth_summer_b1w0_rcp45_CCSM4_50cm, `depth_summer_b1w0_rcp45_GFDL-CM3_50cm`, `depth_summer_b1w0_rcp45_HadGEM2-CC_50cm`,
                                           depth_summer_b1w0_rcp85_CCSM4_50cm, `depth_summer_b1w0_rcp85_GFDL-CM3_50cm`, `depth_summer_b1w0_rcp85_HadGEM2-CC_50cm`,
                                           depth_summer_b1w1_rcp45_CCSM4_50cm, `depth_summer_b1w1_rcp45_GFDL-CM3_50cm`, `depth_summer_b1w1_rcp45_HadGEM2-CC_50cm`,
                                           depth_summer_b1w1_rcp85_CCSM4_50cm, `depth_summer_b1w1_rcp85_GFDL-CM3_50cm`, `depth_summer_b1w1_rcp85_HadGEM2-CC_50cm`,
                                           depth_summer_b2w0_rcp45_CCSM4_50cm, `depth_summer_b2w0_rcp45_GFDL-CM3_50cm`, `depth_summer_b2w0_rcp45_HadGEM2-CC_50cm`,
                                           depth_summer_b2w0_rcp85_CCSM4_50cm, `depth_summer_b2w0_rcp85_GFDL-CM3_50cm`, `depth_summer_b2w0_rcp85_HadGEM2-CC_50cm`,
                                           depth_summer_b2w1_rcp45_CCSM4_50cm, `depth_summer_b2w1_rcp45_GFDL-CM3_50cm`, `depth_summer_b2w1_rcp45_HadGEM2-CC_50cm`,
                                           depth_summer_b2w1_rcp85_CCSM4_50cm, `depth_summer_b2w1_rcp85_GFDL-CM3_50cm`, `depth_summer_b2w1_rcp85_HadGEM2-CC_50cm`,
                                           depth_summer_b3w0_rcp45_CCSM4_50cm, `depth_summer_b3w0_rcp45_GFDL-CM3_50cm`, `depth_summer_b3w0_rcp45_HadGEM2-CC_50cm`,
                                           depth_summer_b3w0_rcp85_CCSM4_50cm, `depth_summer_b3w0_rcp85_GFDL-CM3_50cm`, `depth_summer_b3w0_rcp85_HadGEM2-CC_50cm`,
                                           depth_summer_b3w1_rcp45_CCSM4_50cm, `depth_summer_b3w1_rcp45_GFDL-CM3_50cm`, `depth_summer_b3w1_rcp45_HadGEM2-CC_50cm`,
                                           depth_summer_b3w1_rcp85_CCSM4_50cm, `depth_summer_b3w1_rcp85_GFDL-CM3_50cm`, `depth_summer_b3w1_rcp85_HadGEM2-CC_50cm`),
                                   behavior=rep(c('cold', 'cold-moist',
                                                  'warm', 'warm-moist',
                                                  'passive', 'passive-moist'), 
                                                times=c(length(c(depth_summer_b1w0_rcp45_CCSM4_50cm, `depth_summer_b1w0_rcp45_GFDL-CM3_50cm`, `depth_summer_b1w0_rcp45_HadGEM2-CC_50cm`,
                                                                 depth_summer_b1w0_rcp85_CCSM4_50cm, `depth_summer_b1w0_rcp85_GFDL-CM3_50cm`, `depth_summer_b1w0_rcp85_HadGEM2-CC_50cm`)),
                                                        length(c(depth_summer_b1w1_rcp45_CCSM4_50cm, `depth_summer_b1w1_rcp45_GFDL-CM3_50cm`, `depth_summer_b1w1_rcp45_HadGEM2-CC_50cm`,
                                                                 depth_summer_b1w1_rcp85_CCSM4_50cm, `depth_summer_b1w1_rcp85_GFDL-CM3_50cm`, `depth_summer_b1w1_rcp85_HadGEM2-CC_50cm`)),
                                                        length(c(depth_summer_b2w0_rcp45_CCSM4_50cm, `depth_summer_b2w0_rcp45_GFDL-CM3_50cm`, `depth_summer_b2w0_rcp45_HadGEM2-CC_50cm`,
                                                                 depth_summer_b2w0_rcp85_CCSM4_50cm, `depth_summer_b2w0_rcp85_GFDL-CM3_50cm`, `depth_summer_b2w0_rcp85_HadGEM2-CC_50cm`)),
                                                        length(c(depth_summer_b2w1_rcp45_CCSM4_50cm, `depth_summer_b2w1_rcp45_GFDL-CM3_50cm`, `depth_summer_b2w1_rcp45_HadGEM2-CC_50cm`,
                                                                 depth_summer_b2w1_rcp85_CCSM4_50cm, `depth_summer_b2w1_rcp85_GFDL-CM3_50cm`, `depth_summer_b2w1_rcp85_HadGEM2-CC_50cm`)),
                                                        length(c(depth_summer_b3w0_rcp45_CCSM4_50cm, `depth_summer_b3w0_rcp45_GFDL-CM3_50cm`, `depth_summer_b3w0_rcp45_HadGEM2-CC_50cm`,
                                                                 depth_summer_b3w0_rcp85_CCSM4_50cm, `depth_summer_b3w0_rcp85_GFDL-CM3_50cm`, `depth_summer_b3w0_rcp85_HadGEM2-CC_50cm`)),
                                                        length(c(depth_summer_b3w1_rcp45_CCSM4_50cm, `depth_summer_b3w1_rcp45_GFDL-CM3_50cm`, `depth_summer_b3w1_rcp45_HadGEM2-CC_50cm`,
                                                                 depth_summer_b3w1_rcp85_CCSM4_50cm, `depth_summer_b3w1_rcp85_GFDL-CM3_50cm`, `depth_summer_b3w1_rcp85_HadGEM2-CC_50cm`)))),
                                   sce = rep(c('rcp45_CCSM4_50cm', 'rcp45_GFDL-CM3_50cm', 'rcp45_HadGEM2-CC_50cm',
                                               'rcp85_CCSM4_50cm', 'rcp85_GFDL-CM3_50cm', 'rcp85_HadGEM2-CC_50cm',
                                               'rcp45_CCSM4_50cm', 'rcp45_GFDL-CM3_50cm', 'rcp45_HadGEM2-CC_50cm',
                                               'rcp85_CCSM4_50cm', 'rcp85_GFDL-CM3_50cm', 'rcp85_HadGEM2-CC_50cm',
                                               'rcp45_CCSM4_50cm', 'rcp45_GFDL-CM3_50cm', 'rcp45_HadGEM2-CC_50cm',
                                               'rcp85_CCSM4_50cm', 'rcp85_GFDL-CM3_50cm', 'rcp85_HadGEM2-CC_50cm',
                                               'rcp45_CCSM4_50cm', 'rcp45_GFDL-CM3_50cm', 'rcp45_HadGEM2-CC_50cm',
                                               'rcp85_CCSM4_50cm', 'rcp85_GFDL-CM3_50cm', 'rcp85_HadGEM2-CC_50cm',
                                               'rcp45_CCSM4_50cm', 'rcp45_GFDL-CM3_50cm', 'rcp45_HadGEM2-CC_50cm',
                                               'rcp85_CCSM4_50cm', 'rcp85_GFDL-CM3_50cm', 'rcp85_HadGEM2-CC_50cm',
                                               'rcp45_CCSM4_50cm', 'rcp45_GFDL-CM3_50cm', 'rcp45_HadGEM2-CC_50cm',
                                               'rcp85_CCSM4_50cm', 'rcp85_GFDL-CM3_50cm', 'rcp85_HadGEM2-CC_50cm'), 
                                             times=c(length(depth_summer_b1w0_rcp45_CCSM4_50cm), length(`depth_summer_b1w0_rcp45_GFDL-CM3_50cm`), length(`depth_summer_b1w0_rcp45_HadGEM2-CC_50cm`),
                                                     length(depth_summer_b1w0_rcp85_CCSM4_50cm), length(`depth_summer_b1w0_rcp85_GFDL-CM3_50cm`), length(`depth_summer_b1w0_rcp85_HadGEM2-CC_50cm`),
                                                     length(depth_summer_b1w1_rcp45_CCSM4_50cm), length(`depth_summer_b1w1_rcp45_GFDL-CM3_50cm`), length(`depth_summer_b1w1_rcp45_HadGEM2-CC_50cm`),
                                                     length(depth_summer_b1w1_rcp85_CCSM4_50cm), length(`depth_summer_b1w1_rcp85_GFDL-CM3_50cm`), length(`depth_summer_b1w1_rcp85_HadGEM2-CC_50cm`),
                                                     length(depth_summer_b2w0_rcp45_CCSM4_50cm), length(`depth_summer_b2w0_rcp45_GFDL-CM3_50cm`), length(`depth_summer_b2w0_rcp45_HadGEM2-CC_50cm`),
                                                     length(depth_summer_b2w0_rcp85_CCSM4_50cm), length(`depth_summer_b2w0_rcp85_GFDL-CM3_50cm`), length(`depth_summer_b2w0_rcp85_HadGEM2-CC_50cm`),
                                                     length(depth_summer_b2w1_rcp45_CCSM4_50cm), length(`depth_summer_b2w1_rcp45_GFDL-CM3_50cm`), length(`depth_summer_b2w1_rcp45_HadGEM2-CC_50cm`),
                                                     length(depth_summer_b2w1_rcp85_CCSM4_50cm), length(`depth_summer_b2w1_rcp85_GFDL-CM3_50cm`), length(`depth_summer_b2w1_rcp85_HadGEM2-CC_50cm`),
                                                     length(depth_summer_b3w0_rcp45_CCSM4_50cm), length(`depth_summer_b3w0_rcp45_GFDL-CM3_50cm`), length(`depth_summer_b3w0_rcp45_HadGEM2-CC_50cm`),
                                                     length(depth_summer_b3w0_rcp85_CCSM4_50cm), length(`depth_summer_b3w0_rcp85_GFDL-CM3_50cm`), length(`depth_summer_b3w0_rcp85_HadGEM2-CC_50cm`),
                                                     length(depth_summer_b3w1_rcp45_CCSM4_50cm), length(`depth_summer_b3w1_rcp45_GFDL-CM3_50cm`), length(`depth_summer_b3w1_rcp45_HadGEM2-CC_50cm`),
                                                     length(depth_summer_b3w1_rcp85_CCSM4_50cm), length(`depth_summer_b3w1_rcp85_GFDL-CM3_50cm`), length(`depth_summer_b3w1_rcp85_HadGEM2-CC_50cm`))))


## SMR differences

# plot_smr_winter_cc <- ggplot(smr_winter_cc, aes(y = fct_reorder(interaction(acc, behavior), smr, .fun = sum),
#                                           x = `smr`, 
#                                           fill = sce,
#                                           color=sce))
# 
# plot_smr_summer_cc <- ggplot(smr_summer_cc, aes(y = fct_reorder(interaction(acc, behavior), smr, .fun = sum), 
#                                                 x = `smr`, 
#                                                 fill = sce,
#                                                 color=sce))
# 
# plot_smr_winter_cc_50cm <- ggplot(smr_winter_cc_50cm, aes(y = fct_reorder(interaction(acc, behavior), smr, .fun = sum), 
#                                                 x = `smr`, 
#                                                 fill = sce,
#                                                 color=sce))
# 
# plot_smr_summer_cc_50cm <- ggplot(smr_summer_cc_50cm, aes(y = fct_reorder(interaction(acc, behavior), smr, .fun = sum), 
#                                                 x = `smr`, 
#                                                 fill = sce,
#                                                 color=sce))

plot_smr_winter_cc <- ggplot(smr_winter_cc, aes(y = interaction(acc, behavior),
                                          x = `smr`,
                                          fill = sce,
                                          color=sce))

plot_smr_summer_cc <- ggplot(smr_summer_cc, aes(y = interaction(acc, behavior),
                                                x = `smr`,
                                                fill = sce,
                                                color=sce))

plot_smr_winter_cc_50cm <- ggplot(smr_winter_cc_50cm, aes(y = interaction(acc, behavior),
                                                x = `smr`,
                                                fill = sce,
                                                color=sce))

plot_smr_summer_cc_50cm <- ggplot(smr_summer_cc_50cm, aes(y = interaction(acc, behavior),
                                                x = `smr`,
                                                fill = sce,
                                                color=sce))

p_smr_winter_cc <- plot_smr_winter_cc + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_color_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  scale_fill_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  theme_ridges() +
  theme(legend.position = "none", axis.text.y = element_text(size=10)) +
  ylab('') + 
  xlab('') +
  geom_vline(xintercept=0)



p_smr_summer_cc <- plot_smr_summer_cc + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_color_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  scale_fill_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  theme_ridges() +
  theme(legend.position = "none", axis.text.y = element_text(size=10)) +
  ylab('') + 
  xlab('') +
  geom_vline(xintercept=0)


p_smr_winter_cc_50cm <- plot_smr_winter_cc_50cm + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_color_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  scale_fill_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  theme_ridges() +
  theme(legend.position = "none", axis.text.y = element_text(size=10)) +
  ylab('') + 
  xlab('') + 
  xlab(expression(paste(Delta, 'SMR (in mL ', O[2], ' ', h^-1, ')'))) +
  geom_vline(xintercept=0)



p_smr_summer_cc_50cm <- plot_smr_summer_cc_50cm + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_color_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  scale_fill_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  theme_ridges() +
  theme(legend.position = "none", axis.text.y = element_text(size=10)) +
  ylab('') + 
  xlab('') + 
  xlab(expression(paste(Delta, 'SMR (in mL ', O[2], ' ', h^-1, ')'))) +
  geom_vline(xintercept=0)



plot_all_smr_cc <- plot_grid(p_smr_winter_cc, p_smr_summer_cc, 
                          p_smr_winter_cc_50cm, p_smr_summer_cc_50cm,
                          labels=c("a", "b", "c", "d"),
                          label_size = 24, ncol = 2, nrow = 2, align='h')


# save_plot("./results/Fig2_smr_tb.pdf", plot_all, ncol = 2, nrow = 2, base_height = 4.5, base_aspect_ratio = 1)




### DEPTH


plot_dep_winter_cc <- ggplot(depth_winter_cc, aes(x = behavior, y = depth, fill = behavior,
                                                  group = interaction(behavior, sce)))

plot_dep_summer_cc <- ggplot(depth_summer_cc, aes(x = behavior, y = depth, fill = behavior,
                                                  group = interaction(behavior, sce)))

plot_dep_winter_cc_50cm <- ggplot(depth_winter_cc_50cm, aes(x = behavior, y = depth, fill = behavior,
                                                  group = interaction(behavior, sce)))

plot_dep_summer_cc_50cm <- ggplot(depth_summer_cc_50cm, aes(x = behavior, y = depth, fill = behavior,
                                                  group = interaction(behavior, sce)))


p_dep_winter_cc <- plot_dep_winter_cc + geom_violin(scale='width', position='identity', alpha = 0.4) +
  scale_fill_manual(values=c('cold' = '#0059FF', 
                             'cold-moist' = '#86B0FF',
                             'passive' = '#01BB04', 
                             'passive-moist' = '#8AFF86',
                             'warm' = '#FF8300', 
                             'warm-moist' = '#FEBB74')) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab('') + 
  ylab('Depth')


p_dep_summer_cc <- plot_dep_summer_cc + geom_violin(scale='width', position='identity', alpha = 0.4) +
  scale_fill_manual(values=c('cold' = '#0059FF', 
                             'cold-moist' = '#86B0FF',
                             'passive' = '#01BB04', 
                             'passive-moist' = '#8AFF86',
                             'warm' = '#FF8300', 
                             'warm-moist' = '#FEBB74')) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab('') + 
  ylab('')


p_dep_winter_cc_50cm <- plot_dep_winter_cc_50cm + geom_violin(scale='width', position='identity', alpha = 0.4) +
  scale_fill_manual(values=c('cold' = '#0059FF', 
                             'cold-moist' = '#86B0FF',
                             'passive' = '#01BB04', 
                             'passive-moist' = '#8AFF86',
                             'warm' = '#FF8300', 
                             'warm-moist' = '#FEBB74')) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab('') + 
  ylab('')


p_dep_summer_cc_50cm <- plot_dep_summer_cc_50cm + geom_violin(scale='width', position='identity', alpha = 0.4) +
  scale_fill_manual(values=c('cold' = '#0059FF', 
                             'cold-moist' = '#86B0FF',
                             'passive' = '#01BB04', 
                             'passive-moist' = '#8AFF86',
                             'warm' = '#FF8300', 
                             'warm-moist' = '#FEBB74')) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab('') + 
  ylab('')



plot_all_depth_cc <- plot_grid(p_dep_winter_cc, p_dep_summer_cc, 
                             p_dep_winter_cc_50cm, p_dep_summer_cc_50cm,
                             labels=c("a", "b", "c", "d"),
                             label_size = 24, ncol = 2, nrow = 2, align='h')


# save_plot("./results/Fig2_smr_tb.pdf", plot_all, ncol = 2, nrow = 2, base_height = 4.5, base_aspect_ratio = 1)








### Figure out better ways of grouping the plots


# Figure 1: soil and depths ()


plot_depths <- ggarrange(p_soiltemp, p_minsoiltemp, p_maxsoiltemp, p_soilpot,
                         p_depth_winter, p_depth_summer, p_depth_winter_50cm, p_depth_summer_50cm, 
                         p_dep_winter_cc, p_dep_summer_cc, p_dep_winter_cc_50cm, p_dep_summer_cc_50cm,
                         ncol = 4, nrow = 3, align='v',
                         labels=c('a', 'b', 'c', 'd', "e", "f", "g", "h", "i", "j", "k", "l"))


# ggexport(plot_depths, filename = "./results/FigS1_depths_shade.pdf", height = 7, width=12)



# Figure 2 SMR at the present

plot_all_smr <- plot_grid(p_smr_winter, p_smr_summer, 
                          p_smr_winter_50cm, p_smr_summer_50cm,
                          labels=c("a", "b", "c", "d"),
                          label_size = 24, ncol = 2, nrow = 2, align='h')

# save_plot("./results/FigSX_smr_present_shade.pdf", plot_all_smr, ncol = 2, nrow = 2, base_height = 4.5, base_aspect_ratio = 1)


# Figure3 SMR in 2070 vs b1w0


plot_all_smr_cc <- plot_grid(p_smr_winter_cc, p_smr_summer_cc, 
                             p_smr_winter_cc_50cm, p_smr_summer_cc_50cm,
                             labels=c("a", "b", "c", "d"),
                             label_size = 24, ncol = 2, nrow = 2, align='h')


# save_plot("./results/FigS2_dSMR_vsColdNonAdapted_shade.pdf", plot_all_smr_cc, ncol = 2, nrow = 2, base_height = 4.5, base_aspect_ratio = 1)


# Figure 4 SMR compared to non-acclimated but same strategy

for(i in 1:length(environs_cc)){
  environ <- environs_cc[[i]]
  environ_bl <- environs[[substr(names(environs_cc), 1, 4)[[i]]]]
  
  # SMR (always compare non-acclimated to acclimated, SMRfut - SMR and SMRacc_fut - SMR)
  
  # winter
  dSMR <- environ$SMR[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 335 | environ_bl$DOY <= 59 & environ_bl$ACT == 0]
  
  assign(paste('smr_winter_', names(environs_cc)[i], sep=''), dSMR)
  
  # summer
  dSMR <- environ$SMR[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 152 & environ_bl$DOY <= 243 & environ_bl$ACT == 0]
  
  assign(paste('smr_summer_', names(environs_cc)[i], sep=''), dSMR)
  
  
  # SMR with acclimation
  # winter
  dSMR <- environ$SMR_acc[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 335 | environ_bl$DOY <= 59 & environ_bl$ACT == 0]
  
  assign(paste('smr_acc_winter_', names(environs_cc)[i], sep=''), dSMR)
  
  # summer
  dSMR <- environ$SMR_acc[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 152 & environ_bl$DOY <= 243 & environ_bl$ACT == 0]
  
  assign(paste('smr_acc_summer_', names(environs_cc)[i], sep=''), dSMR)
  
  
  
  
  
  # then if they can only retreat to shelters up to 50cm
  
  environ <- environs_cc_50cm[[i]]
  environ_bl <- environs_50cm[[paste(substr(names(environs_cc_50cm), 1, 4)[[i]], '_50cm', sep='')]]
  
  # SMR (always compare non-acclimated to acclimated, SMRfut - SMR and SMRacc_fut - SMR)
  environ_bl <- environs[["b1w0"]]
  
  # winter
  dSMR <- environ$SMR[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 335 | environ_bl$DOY <= 59 & environ_bl$ACT == 0]
  
  assign(paste('smr_winter_', names(environs_cc_50cm)[i], sep=''), dSMR)
  
  # summer
  dSMR <- environ$SMR[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 152 & environ_bl$DOY <= 243 & environ_bl$ACT == 0]
  
  assign(paste('smr_summer_', names(environs_cc_50cm)[i], sep=''), dSMR)
  
  
  # SMR with acclimation
  # winter
  dSMR <- environ$SMR_acc[environ$DOY >= 335 | environ$DOY <= 59 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 335 | environ_bl$DOY <= 59 & environ_bl$ACT == 0]
  
  assign(paste('smr_acc_winter_', names(environs_cc_50cm)[i], sep=''), dSMR)
  
  # summer
  dSMR <- environ$SMR_acc[environ$DOY >= 152 & environ$DOY <= 243 & environ$ACT == 0] - 
    environ_bl$SMR[environ_bl$DOY >= 152 & environ_bl$DOY <= 243 & environ_bl$ACT == 0]
  
  assign(paste('smr_acc_summer_', names(environs_cc_50cm)[i], sep=''), dSMR)
  
  
}




smr_winter_cc <- data.frame(smr=c(smr_winter_b1w0_rcp45_CCSM4, `smr_winter_b1w0_rcp45_GFDL-CM3`, `smr_winter_b1w0_rcp45_HadGEM2-CC`,
                                  smr_winter_b1w0_rcp85_CCSM4, `smr_winter_b1w0_rcp85_GFDL-CM3`, `smr_winter_b1w0_rcp85_HadGEM2-CC`,
                                  smr_winter_b1w1_rcp45_CCSM4, `smr_winter_b1w1_rcp45_GFDL-CM3`, `smr_winter_b1w1_rcp45_HadGEM2-CC`,
                                  smr_winter_b1w1_rcp85_CCSM4, `smr_winter_b1w1_rcp85_GFDL-CM3`, `smr_winter_b1w1_rcp85_HadGEM2-CC`,
                                  smr_winter_b2w0_rcp45_CCSM4, `smr_winter_b2w0_rcp45_GFDL-CM3`, `smr_winter_b2w0_rcp45_HadGEM2-CC`,
                                  smr_winter_b2w0_rcp85_CCSM4, `smr_winter_b2w0_rcp85_GFDL-CM3`, `smr_winter_b2w0_rcp85_HadGEM2-CC`,
                                  smr_winter_b2w1_rcp45_CCSM4, `smr_winter_b2w1_rcp45_GFDL-CM3`, `smr_winter_b2w1_rcp45_HadGEM2-CC`,
                                  smr_winter_b2w1_rcp85_CCSM4, `smr_winter_b2w1_rcp85_GFDL-CM3`, `smr_winter_b2w1_rcp85_HadGEM2-CC`,
                                  smr_winter_b3w0_rcp45_CCSM4, `smr_winter_b3w0_rcp45_GFDL-CM3`, `smr_winter_b3w0_rcp45_HadGEM2-CC`,
                                  smr_winter_b3w0_rcp85_CCSM4, `smr_winter_b3w0_rcp85_GFDL-CM3`, `smr_winter_b3w0_rcp85_HadGEM2-CC`,
                                  smr_winter_b3w1_rcp45_CCSM4, `smr_winter_b3w1_rcp45_GFDL-CM3`, `smr_winter_b3w1_rcp45_HadGEM2-CC`,
                                  smr_winter_b3w1_rcp85_CCSM4, `smr_winter_b3w1_rcp85_GFDL-CM3`, `smr_winter_b3w1_rcp85_HadGEM2-CC`,
                                  smr_acc_winter_b1w0_rcp45_CCSM4, `smr_acc_winter_b1w0_rcp45_GFDL-CM3`, `smr_acc_winter_b1w0_rcp45_HadGEM2-CC`,
                                  smr_acc_winter_b1w0_rcp85_CCSM4, `smr_acc_winter_b1w0_rcp85_GFDL-CM3`, `smr_acc_winter_b1w0_rcp85_HadGEM2-CC`,
                                  smr_acc_winter_b1w1_rcp45_CCSM4, `smr_acc_winter_b1w1_rcp45_GFDL-CM3`, `smr_acc_winter_b1w1_rcp45_HadGEM2-CC`,
                                  smr_acc_winter_b1w1_rcp85_CCSM4, `smr_acc_winter_b1w1_rcp85_GFDL-CM3`, `smr_acc_winter_b1w1_rcp85_HadGEM2-CC`,
                                  smr_acc_winter_b2w0_rcp45_CCSM4, `smr_acc_winter_b2w0_rcp45_GFDL-CM3`, `smr_acc_winter_b2w0_rcp45_HadGEM2-CC`,
                                  smr_acc_winter_b2w0_rcp85_CCSM4, `smr_acc_winter_b2w0_rcp85_GFDL-CM3`, `smr_acc_winter_b2w0_rcp85_HadGEM2-CC`,
                                  smr_acc_winter_b2w1_rcp45_CCSM4, `smr_acc_winter_b2w1_rcp45_GFDL-CM3`, `smr_acc_winter_b2w1_rcp45_HadGEM2-CC`,
                                  smr_acc_winter_b2w1_rcp85_CCSM4, `smr_acc_winter_b2w1_rcp85_GFDL-CM3`, `smr_acc_winter_b2w1_rcp85_HadGEM2-CC`,
                                  smr_acc_winter_b3w0_rcp45_CCSM4, `smr_acc_winter_b3w0_rcp45_GFDL-CM3`, `smr_acc_winter_b3w0_rcp45_HadGEM2-CC`,
                                  smr_acc_winter_b3w0_rcp85_CCSM4, `smr_acc_winter_b3w0_rcp85_GFDL-CM3`, `smr_acc_winter_b3w0_rcp85_HadGEM2-CC`,
                                  smr_acc_winter_b3w1_rcp45_CCSM4, `smr_acc_winter_b3w1_rcp45_GFDL-CM3`, `smr_acc_winter_b3w1_rcp45_HadGEM2-CC`,
                                  smr_acc_winter_b3w1_rcp85_CCSM4, `smr_acc_winter_b3w1_rcp85_GFDL-CM3`, `smr_acc_winter_b3w1_rcp85_HadGEM2-CC`),
                            acc=rep(c('no acc', 'acc'), each=697824),
                            behavior=rep(rep(c('cold', 'cold-moist',
                                               'warm', 'warm-moist',
                                               'passive', 'passive-moist'), each=116304), times=2),
                            sce = rep(rep(c('rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                            'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC'), 
                                          each=19384), times=12))




smr_summer_cc <- data.frame(smr=c(smr_summer_b1w0_rcp45_CCSM4, `smr_summer_b1w0_rcp45_GFDL-CM3`, `smr_summer_b1w0_rcp45_HadGEM2-CC`,
                                  smr_summer_b1w0_rcp85_CCSM4, `smr_summer_b1w0_rcp85_GFDL-CM3`, `smr_summer_b1w0_rcp85_HadGEM2-CC`,
                                  smr_summer_b1w1_rcp45_CCSM4, `smr_summer_b1w1_rcp45_GFDL-CM3`, `smr_summer_b1w1_rcp45_HadGEM2-CC`,
                                  smr_summer_b1w1_rcp85_CCSM4, `smr_summer_b1w1_rcp85_GFDL-CM3`, `smr_summer_b1w1_rcp85_HadGEM2-CC`,
                                  smr_summer_b2w0_rcp45_CCSM4, `smr_summer_b2w0_rcp45_GFDL-CM3`, `smr_summer_b2w0_rcp45_HadGEM2-CC`,
                                  smr_summer_b2w0_rcp85_CCSM4, `smr_summer_b2w0_rcp85_GFDL-CM3`, `smr_summer_b2w0_rcp85_HadGEM2-CC`,
                                  smr_summer_b2w1_rcp45_CCSM4, `smr_summer_b2w1_rcp45_GFDL-CM3`, `smr_summer_b2w1_rcp45_HadGEM2-CC`,
                                  smr_summer_b2w1_rcp85_CCSM4, `smr_summer_b2w1_rcp85_GFDL-CM3`, `smr_summer_b2w1_rcp85_HadGEM2-CC`,
                                  smr_summer_b3w0_rcp45_CCSM4, `smr_summer_b3w0_rcp45_GFDL-CM3`, `smr_summer_b3w0_rcp45_HadGEM2-CC`,
                                  smr_summer_b3w0_rcp85_CCSM4, `smr_summer_b3w0_rcp85_GFDL-CM3`, `smr_summer_b3w0_rcp85_HadGEM2-CC`,
                                  smr_summer_b3w1_rcp45_CCSM4, `smr_summer_b3w1_rcp45_GFDL-CM3`, `smr_summer_b3w1_rcp45_HadGEM2-CC`,
                                  smr_summer_b3w1_rcp85_CCSM4, `smr_summer_b3w1_rcp85_GFDL-CM3`, `smr_summer_b3w1_rcp85_HadGEM2-CC`,
                                  smr_acc_summer_b1w0_rcp45_CCSM4, `smr_acc_summer_b1w0_rcp45_GFDL-CM3`, `smr_acc_summer_b1w0_rcp45_HadGEM2-CC`,
                                  smr_acc_summer_b1w0_rcp85_CCSM4, `smr_acc_summer_b1w0_rcp85_GFDL-CM3`, `smr_acc_summer_b1w0_rcp85_HadGEM2-CC`,
                                  smr_acc_summer_b1w1_rcp45_CCSM4, `smr_acc_summer_b1w1_rcp45_GFDL-CM3`, `smr_acc_summer_b1w1_rcp45_HadGEM2-CC`,
                                  smr_acc_summer_b1w1_rcp85_CCSM4, `smr_acc_summer_b1w1_rcp85_GFDL-CM3`, `smr_acc_summer_b1w1_rcp85_HadGEM2-CC`,
                                  smr_acc_summer_b2w0_rcp45_CCSM4, `smr_acc_summer_b2w0_rcp45_GFDL-CM3`, `smr_acc_summer_b2w0_rcp45_HadGEM2-CC`,
                                  smr_acc_summer_b2w0_rcp85_CCSM4, `smr_acc_summer_b2w0_rcp85_GFDL-CM3`, `smr_acc_summer_b2w0_rcp85_HadGEM2-CC`,
                                  smr_acc_summer_b2w1_rcp45_CCSM4, `smr_acc_summer_b2w1_rcp45_GFDL-CM3`, `smr_acc_summer_b2w1_rcp45_HadGEM2-CC`,
                                  smr_acc_summer_b2w1_rcp85_CCSM4, `smr_acc_summer_b2w1_rcp85_GFDL-CM3`, `smr_acc_summer_b2w1_rcp85_HadGEM2-CC`,
                                  smr_acc_summer_b3w0_rcp45_CCSM4, `smr_acc_summer_b3w0_rcp45_GFDL-CM3`, `smr_acc_summer_b3w0_rcp45_HadGEM2-CC`,
                                  smr_acc_summer_b3w0_rcp85_CCSM4, `smr_acc_summer_b3w0_rcp85_GFDL-CM3`, `smr_acc_summer_b3w0_rcp85_HadGEM2-CC`,
                                  smr_acc_summer_b3w1_rcp45_CCSM4, `smr_acc_summer_b3w1_rcp45_GFDL-CM3`, `smr_acc_summer_b3w1_rcp45_HadGEM2-CC`,
                                  smr_acc_summer_b3w1_rcp85_CCSM4, `smr_acc_summer_b3w1_rcp85_GFDL-CM3`, `smr_acc_summer_b3w1_rcp85_HadGEM2-CC`),
                            acc=rep(c('no acc', 'acc'), each=602514),
                            behavior=rep(rep(c('cold', 'cold-moist',
                                               'warm', 'warm-moist',
                                               'passive', 'passive-moist'), each=100419), times=2),
                            sce = rep(rep(c('rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                            'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC'), 
                                          each=167365), times=12))


smr_winter_cc_50cm <- data.frame(smr=c(smr_winter_b1w0_rcp45_CCSM4_50cm, `smr_winter_b1w0_rcp45_GFDL-CM3_50cm`, `smr_winter_b1w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_winter_b1w0_rcp85_CCSM4_50cm, `smr_winter_b1w0_rcp85_GFDL-CM3_50cm`, `smr_winter_b1w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_winter_b1w1_rcp45_CCSM4_50cm, `smr_winter_b1w1_rcp45_GFDL-CM3_50cm`, `smr_winter_b1w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_winter_b1w1_rcp85_CCSM4_50cm, `smr_winter_b1w1_rcp85_GFDL-CM3_50cm`, `smr_winter_b1w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_winter_b2w0_rcp45_CCSM4_50cm, `smr_winter_b2w0_rcp45_GFDL-CM3_50cm`, `smr_winter_b2w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_winter_b2w0_rcp85_CCSM4_50cm, `smr_winter_b2w0_rcp85_GFDL-CM3_50cm`, `smr_winter_b2w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_winter_b2w1_rcp45_CCSM4_50cm, `smr_winter_b2w1_rcp45_GFDL-CM3_50cm`, `smr_winter_b2w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_winter_b2w1_rcp85_CCSM4_50cm, `smr_winter_b2w1_rcp85_GFDL-CM3_50cm`, `smr_winter_b2w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_winter_b3w0_rcp45_CCSM4_50cm, `smr_winter_b3w0_rcp45_GFDL-CM3_50cm`, `smr_winter_b3w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_winter_b3w0_rcp85_CCSM4_50cm, `smr_winter_b3w0_rcp85_GFDL-CM3_50cm`, `smr_winter_b3w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_winter_b3w1_rcp45_CCSM4_50cm, `smr_winter_b3w1_rcp45_GFDL-CM3_50cm`, `smr_winter_b3w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_winter_b3w1_rcp85_CCSM4_50cm, `smr_winter_b3w1_rcp85_GFDL-CM3_50cm`, `smr_winter_b3w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b1w0_rcp45_CCSM4_50cm, `smr_acc_winter_b1w0_rcp45_GFDL-CM3_50cm`, `smr_acc_winter_b1w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b1w0_rcp85_CCSM4_50cm, `smr_acc_winter_b1w0_rcp85_GFDL-CM3_50cm`, `smr_acc_winter_b1w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b1w1_rcp45_CCSM4_50cm, `smr_acc_winter_b1w1_rcp45_GFDL-CM3_50cm`, `smr_acc_winter_b1w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b1w1_rcp85_CCSM4_50cm, `smr_acc_winter_b1w1_rcp85_GFDL-CM3_50cm`, `smr_acc_winter_b1w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b2w0_rcp45_CCSM4_50cm, `smr_acc_winter_b2w0_rcp45_GFDL-CM3_50cm`, `smr_acc_winter_b2w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b2w0_rcp85_CCSM4_50cm, `smr_acc_winter_b2w0_rcp85_GFDL-CM3_50cm`, `smr_acc_winter_b2w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b2w1_rcp45_CCSM4_50cm, `smr_acc_winter_b2w1_rcp45_GFDL-CM3_50cm`, `smr_acc_winter_b2w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b2w1_rcp85_CCSM4_50cm, `smr_acc_winter_b2w1_rcp85_GFDL-CM3_50cm`, `smr_acc_winter_b2w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b3w0_rcp45_CCSM4_50cm, `smr_acc_winter_b3w0_rcp45_GFDL-CM3_50cm`, `smr_acc_winter_b3w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b3w0_rcp85_CCSM4_50cm, `smr_acc_winter_b3w0_rcp85_GFDL-CM3_50cm`, `smr_acc_winter_b3w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b3w1_rcp45_CCSM4_50cm, `smr_acc_winter_b3w1_rcp45_GFDL-CM3_50cm`, `smr_acc_winter_b3w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_winter_b3w1_rcp85_CCSM4_50cm, `smr_acc_winter_b3w1_rcp85_GFDL-CM3_50cm`, `smr_acc_winter_b3w1_rcp85_HadGEM2-CC_50cm`),
                                 acc=rep(c('no acc', 'acc'), each=697968),
                                 behavior=rep(rep(c('cold', 'cold-moist',
                                                    'warm', 'warm-moist',
                                                    'passive', 'passive-moist'), each=116328), times=2),
                                 sce = rep(rep(c('rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                                 'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC'), 
                                               each=19388), times=12))



smr_summer_cc_50cm <- data.frame(smr=c(smr_summer_b1w0_rcp45_CCSM4_50cm, `smr_summer_b1w0_rcp45_GFDL-CM3_50cm`, `smr_summer_b1w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_summer_b1w0_rcp85_CCSM4_50cm, `smr_summer_b1w0_rcp85_GFDL-CM3_50cm`, `smr_summer_b1w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_summer_b1w1_rcp45_CCSM4_50cm, `smr_summer_b1w1_rcp45_GFDL-CM3_50cm`, `smr_summer_b1w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_summer_b1w1_rcp85_CCSM4_50cm, `smr_summer_b1w1_rcp85_GFDL-CM3_50cm`, `smr_summer_b1w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_summer_b2w0_rcp45_CCSM4_50cm, `smr_summer_b2w0_rcp45_GFDL-CM3_50cm`, `smr_summer_b2w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_summer_b2w0_rcp85_CCSM4_50cm, `smr_summer_b2w0_rcp85_GFDL-CM3_50cm`, `smr_summer_b2w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_summer_b2w1_rcp45_CCSM4_50cm, `smr_summer_b2w1_rcp45_GFDL-CM3_50cm`, `smr_summer_b2w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_summer_b2w1_rcp85_CCSM4_50cm, `smr_summer_b2w1_rcp85_GFDL-CM3_50cm`, `smr_summer_b2w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_summer_b3w0_rcp45_CCSM4_50cm, `smr_summer_b3w0_rcp45_GFDL-CM3_50cm`, `smr_summer_b3w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_summer_b3w0_rcp85_CCSM4_50cm, `smr_summer_b3w0_rcp85_GFDL-CM3_50cm`, `smr_summer_b3w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_summer_b3w1_rcp45_CCSM4_50cm, `smr_summer_b3w1_rcp45_GFDL-CM3_50cm`, `smr_summer_b3w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_summer_b3w1_rcp85_CCSM4_50cm, `smr_summer_b3w1_rcp85_GFDL-CM3_50cm`, `smr_summer_b3w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b1w0_rcp45_CCSM4_50cm, `smr_acc_summer_b1w0_rcp45_GFDL-CM3_50cm`, `smr_acc_summer_b1w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b1w0_rcp85_CCSM4_50cm, `smr_acc_summer_b1w0_rcp85_GFDL-CM3_50cm`, `smr_acc_summer_b1w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b1w1_rcp45_CCSM4_50cm, `smr_acc_summer_b1w1_rcp45_GFDL-CM3_50cm`, `smr_acc_summer_b1w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b1w1_rcp85_CCSM4_50cm, `smr_acc_summer_b1w1_rcp85_GFDL-CM3_50cm`, `smr_acc_summer_b1w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b2w0_rcp45_CCSM4_50cm, `smr_acc_summer_b2w0_rcp45_GFDL-CM3_50cm`, `smr_acc_summer_b2w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b2w0_rcp85_CCSM4_50cm, `smr_acc_summer_b2w0_rcp85_GFDL-CM3_50cm`, `smr_acc_summer_b2w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b2w1_rcp45_CCSM4_50cm, `smr_acc_summer_b2w1_rcp45_GFDL-CM3_50cm`, `smr_acc_summer_b2w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b2w1_rcp85_CCSM4_50cm, `smr_acc_summer_b2w1_rcp85_GFDL-CM3_50cm`, `smr_acc_summer_b2w1_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b3w0_rcp45_CCSM4_50cm, `smr_acc_summer_b3w0_rcp45_GFDL-CM3_50cm`, `smr_acc_summer_b3w0_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b3w0_rcp85_CCSM4_50cm, `smr_acc_summer_b3w0_rcp85_GFDL-CM3_50cm`, `smr_acc_summer_b3w0_rcp85_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b3w1_rcp45_CCSM4_50cm, `smr_acc_summer_b3w1_rcp45_GFDL-CM3_50cm`, `smr_acc_summer_b3w1_rcp45_HadGEM2-CC_50cm`,
                                       smr_acc_summer_b3w1_rcp85_CCSM4_50cm, `smr_acc_summer_b3w1_rcp85_GFDL-CM3_50cm`, `smr_acc_summer_b3w1_rcp85_HadGEM2-CC_50cm`),
                                 acc=rep(c('no acc', 'acc'), each=602514),
                                 behavior=rep(rep(c('cold', 'cold-moist',
                                                    'warm', 'warm-moist',
                                                    'passive', 'passive-moist'), each=100419), times=2),
                                 sce = rep(rep(c('rcp45_CCSM4', 'rcp45_GFDL-CM3', 'rcp45_HadGEM2-CC',
                                                 'rcp85_CCSM4', 'rcp85_GFDL-CM3', 'rcp85_HadGEM2-CC'), 
                                               each=167365), times=12))




plot_smr_winter_cc <- ggplot(smr_winter_cc, aes(y = interaction(acc, behavior),
                                                x = `smr`,
                                                fill = sce,
                                                color=sce))

plot_smr_summer_cc <- ggplot(smr_summer_cc, aes(y = interaction(acc, behavior),
                                                x = `smr`,
                                                fill = sce,
                                                color=sce))

plot_smr_winter_cc_50cm <- ggplot(smr_winter_cc_50cm, aes(y = interaction(acc, behavior),
                                                          x = `smr`,
                                                          fill = sce,
                                                          color=sce))

plot_smr_summer_cc_50cm <- ggplot(smr_summer_cc_50cm, aes(y = interaction(acc, behavior),
                                                          x = `smr`,
                                                          fill = sce,
                                                          color=sce))

p_smr_winter_cc <- plot_smr_winter_cc + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_color_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  scale_fill_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  theme_ridges() +
  theme(legend.position = "none") +
  ylab('') + 
  xlab('') +
  geom_vline(xintercept=0)



p_smr_summer_cc <- plot_smr_summer_cc + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_color_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  scale_fill_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  theme_ridges() +
  theme(legend.position = "none") +
  ylab('') + 
  xlab('') +
  geom_vline(xintercept=0)


p_smr_winter_cc_50cm <- plot_smr_winter_cc_50cm + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_color_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  scale_fill_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  theme_ridges() +
  theme(legend.position = "none") +
  ylab('') + 
  xlab('') +
  geom_vline(xintercept=0)



p_smr_summer_cc_50cm <- plot_smr_summer_cc_50cm + geom_density_ridges_gradient(scale = 1, rel_min_height = 0.001) +
  scale_color_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  scale_fill_viridis(discrete = TRUE, alpha=.5, option = "plasma") + 
  theme_ridges() +
  theme(legend.position = "none") +
  ylab('') + 
  xlab('') +
  geom_vline(xintercept=0)



plot_all_smr_cc_2 <- plot_grid(p_smr_winter_cc, p_smr_summer_cc, 
                             p_smr_winter_cc_50cm, p_smr_summer_cc_50cm,
                             labels=c("a", "b", "c", "d"),
                             label_size = 24, ncol = 2, nrow = 2, align='h')


save_plot("./results/Fig4_dSMR_vsNonAdapted_sameStrategy.pdf", plot_all_smr_cc_2, ncol = 2, nrow = 2, base_height = 4.5, base_aspect_ratio = 1)


