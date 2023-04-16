
setwd('./code')
rm(list=ls())

library(ggplot2)
library(ggridges)
library(viridis)
library(forcats)
source('./aux_functions.R')

load('../results/micro_Jihlava.Rda')

ecto_df_current_sun <- load_ectos(scenario = "cur", movement = "200cm", 
                                  shading = "sun", micro = micro)

# str(ecto_df_current_sun)
# levels(ecto_df_current_sun$model)

# with(ecto_df_current_sun[ecto_df_current_sun$model == "ecto_b1w0_50cm_sun",],
#      plot(dates, TC, type='l'))


ecto_df_current_sun_summer <- subset_env(ecto_df_current_sun, season = "summer")
ecto_df_current_sun_winter <- subset_env(ecto_df_current_sun, season = "winter")


plot_test <- ggplot(ecto_df_current_sun_winter, aes(x = model, y = SMR, fill = model))
plot_test + geom_violin(scale='width') +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_blank()) + 
  xlab('') + 
  ylab('SMR') +
  geom_point(alpha = .01, position = position_jitter(seed = 1, width = .2))


plot_smr <- ggplot(ecto_df_current_sun_winter, aes(y = fct_reorder(model, SMR, .fun = sum),
                          x = SMR,
                          fill = model))
                          # fill = stat(x)))

plot_smr + geom_density_ridges_gradient(rel_min_height = 0.001,
  jittered_points = TRUE, quantile_lines = TRUE, scale = 1, alpha = 0.7,
  vline_size = 1, vline_color = "red",
  point_size = 0.01, point_alpha = 0.01,
  position = position_raincloud(adjust_vlines = TRUE)) +
  #scale_fill_viridis(option = "B") +
  theme_ridges() +
  theme(legend.position = "none") +
  ylab('') + 
  xlab('') +
  xlim(0.02,0.15)





# example Te traces
# pdf("./results/FigSX_Te_traces_unconstrained.pdf", height=20, width=10)

# par(mfrow=c(6,1), mar=c(3,4,0.5,0.5))

for(i in levels(ecto_df_current_sun$model)){
  
  environ <- ecto_df_current_sun[ecto_df_current_sun$model == i,]
  metout <- data.frame(micro$metout)
  
  # append dates
  days <- rep(seq(1, length(unique(environ$DAY))), 24)
  days <- days[order(days)]
  dates <- days+metout$TIME/60/24-1 # dates for hourly output
  
  
  with(environ, plot(TC ~ dates, ylab = "", xlab="", col = 'black', ylim = c(-70, 38), type = "l", yaxt = 'n'))
  with(environ, points(ACT * 2 - 10 ~ dates, type = "l", pch = 16, col = "orange"))
  with(environ, points(DEP/4 - 15 ~ dates, type = "l", col = "brown"))
  abline(4, 0, lty = 2, col = 'blue') # T_F_min
  abline(20, 0, lty = 2, col = 'red') # T_F_max
  abline(-2, 0, col = 'blue') # CT_min
  abline(36, 0, col = 'red') # CT_max
  ytick<-seq(0, 40, by=5)
  axis(side=2, at=ytick, labels = TRUE, las = 2, cex.axis = .7)
  mtext(text = c('Active', 'Inactive'), side = 2, line = 1, at = c(-6, -10), cex = .7, las = 2)
  ytick<-seq(-6, -10, by=-4)
  axis(side=2, at=ytick, labels = FALSE)
  mtext(text = rev(seq(0, 200, 40)), side = 2, line = 1, at = seq(-65, -15, length.out=6), las = 2, cex = .7)
  ytick<-seq(-65, -15, length.out=6)
  axis(side=2, at=ytick, labels = FALSE)
  abline(h = -15, lty = 2, col = 'grey')
  mtext(text = c('body temperature (°C)', 'depth (cm)'), side = 2, line = 2.5, at = c(22, -40), cex = .7)
  text(15, c(20 + 1, 4 + 1), c('VTmax', 'VTmin'), col = c('red', 'blue'), cex = 0.75)
  text(15, c(36 + 1, -2 + 1), c('CTmax', 'CTmin'), col = c('red', 'blue'), cex = 0.75)
  
}


# dev.off()


