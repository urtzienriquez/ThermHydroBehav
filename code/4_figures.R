
setwd('./code')

# # check ecto results
# ecto_files <- list.files(path='../results', pattern='ecto*', full.names=T)
# 
# pdf('../results/ecto_check.pdf')
# par(mfrow=c(2,1))
# for(ec in ecto_files){
#   load(ec)
#   ecto_name <- strsplit(strsplit(ec, '/')[[1]][3], '.R')[[1]][1]
#   environ <- data.frame(ecto$environ)
#   plot(environ$TC, type='l', main=ecto_name)
#   plot(environ$DEP, type='l')
# }
# dev.off()

#ls()[grep('sun', ls())]

load('../results/micro_Jihlava.Rda')

ecto_current_sun_files <- list.files(path='../results', pattern='*200cm_sun.Rda', full.names=T)
ecto_current_shd_files <- list.files(path='../results', pattern='*200cm_shade.Rda', full.names=T)

a <- 1
for(ec in ecto_current_sun_files){
  load(ec)
  ecto$environ$dates <- micro$dates
  ecto$environ$model <- strsplit(strsplit(ec, '/')[[1]][3], '.R')[[1]][1]
  if(a==1){envs_current_sun <- ecto$environ}
  else{envs_current_sun <- rbind(envs_current_sun, ecto$environ)}
  a = a+1
}
str(envs_current_sun)
unique(envs_current_sun$model)

with(envs_current_sun[envs_current_sun$model == "ecto_b1w0_200cm_sun",],
     plot(dates, TC, type='l'))


# summer: 21 June – 23 September
# winter: 22 December – 21 March

env_subset <- envs_current_sun[as.numeric(format(envs_current_sun$dates, "%m%d")) >= 621 &
                                 as.numeric(format(envs_current_sun$dates, "%m%d")) <= 923,]
env_subset2 <- env_subset[env_subset$ACT == 0,]
unique(format(env_subset$dates, "%m-%d"))
str(env_subset2)

env_subset2$model <- as.factor(env_subset2$model)

library(ggplot2)
plot_test <- ggplot(env_subset2, aes(x = model, y = SMR, fill = model))
plot_test + geom_violin(scale='width') +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_blank()) + 
  xlab('') + 
  ylab('SMR') +
  geom_point(alpha = .01, position = position_jitter(seed = 1, width = .2))
