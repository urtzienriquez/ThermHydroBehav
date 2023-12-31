
rm(list=ls())
setwd('./code')
library(NicheMapR)

##########################
# Physiology

## preferred temperatures
tpref <- read.table('../data/Gvozdik_Kristin_TP.txt', 
                    header=T, sep='\t')
str(tpref)

m_tpref <- mean(tpref$MeanTp[tpref$Treatment == 'Food'])
lbt <- mean(tpref$LBTp[tpref$Treatment == 'Food'])
ubt <- mean(tpref$UBTp[tpref$Treatment == 'Food'])


## metabolic rates
mrs <- read.table('../data/Gvozdik_Kristin_AS.txt', 
                  header=T, sep='\t')
str(mrs)
colnames(mrs) <- c('temp', 'sex', 'mass', 'smr', 'mmr', 'scope', 'fscope')


### predict smr from body temperature
smr_model <- nls(smr ~ exp(a + b * temp), data = mrs, start = list(a = 0, b = 0))
new <- data.frame(temp=seq(10,25))
new$predicted <- predict(smr_model, newdata=new)

plot(smr ~ temp, data=mrs, pch=19, cex=.5,
     xlab='Temperature (ºC)', 
     ylab=expression(paste('SMR (in mL ', O[2], ' ', h^-1, ')')))
lines(new$temp, new$predicted)
lines(new$temp, new$predicted * 0.7, col='gray', lty=2) # acclimated


##########################
# Ectotherm model

load('../results/micro_Jihlava.Rda') # load microclimate

# set strategies
# movement restriction (50cm) is inside the loop
behav <- c(1,2,3) # 1 = cold shelters; 2 = warm shelters; 3 = passive
bwater <- c(0,1) # following moist shelters (0 = no, 1 = yes)
shade <- c(FALSE, TRUE)

for(s in shade){
  if(s){
    maxshades <- micro$maxshade # shaded conditions
    minshades <- micro$maxshade # shaded conditions
  } 
  else{
    maxshades <- micro$minshade + 1 # unshaded conditions
    minshades <- micro$minshade # unshaded conditions
  }
  
  # first thermoregulating "freely"
  for(i in 1:length(behav)){
    for(j in 1:length(bwater)){
      if(s) {
        ecto_name <- paste0('ecto_b',behav[i],'w',bwater[j],'_200cm_shade_current')
        shdburrow = 2
      }
      else {
        ecto_name <- paste0('ecto_b',behav[i],'w',bwater[j],'_200cm_sun_current')
        shdburrow = 0
      }
      ecto <- ectotherm(Ww_g =3.5, shape = 3, burrow=1,
                        T_F_min = 4, T_F_max = ubt, T_pref = m_tpref,
                        T_B_min = 4, T_RB_min = 4,
                        CT_max = 36, CT_min = -2,
                        diurn = 0, nocturn = 1, crepus = 1, shade_seek = 0, maxdepth = 10,
                        pct_wet = 90, burrowtmp = behav[i], burrowwtr = bwater[j],
                        maxshade=maxshades, minshade=minshades, shdburrow = shdburrow)
      # shdburrow = 2 to simulate conditions in the shade
      save(ecto, file=paste0('../results/', ecto_name, '.Rda')) 
    }
  }
  
  # then thermoregulating to a maximum of 50 cm
  for(i in 1:length(behav)){
    for(j in 1:length(bwater)){
      if(s) {
        ecto_name <- paste0('ecto_b',behav[i],'w',bwater[j],'_50cm_shade_current')
        shdburrow = 2
      }
      else {
        ecto_name <- paste0('ecto_b',behav[i],'w',bwater[j],'_50cm_sun_current')
        shdburrow = 0
      }
      ecto <- ectotherm(Ww_g =3.5, shape = 3, burrow=1,
                        T_F_min = 4, T_F_max = ubt, T_pref = m_tpref,
                        T_B_min = 4, T_RB_min = 4,
                        CT_max = 36, CT_min = -2,
                        diurn = 0, nocturn = 1, crepus = 1, shade_seek = 0, maxdepth = 8,
                        pct_wet = 90, burrowtmp = behav[i], burrowwtr = bwater[j],
                        maxshade=maxshades, minshade=minshades, shdburrow = shdburrow)
      save(ecto, file=paste0('../results/', ecto_name, '.RData')) 
    }
  }
}


##########################
# Climate change scenarios

micro_files <- list.files(path='../results', pattern='*5.RData', full.names=T)

sce <- c("cc45", "cc85", "gf45", "gf85", "hg45", "hg85") # scenarios
for(sc in sce){
  load(paste0("../results/micro_", sc, ".RData")) 
  
  # run ectotherm models as above, with all behavioral strategies, shading levels, and restrictions
  for(s in shade){
    if(s){
      maxshades <- micro$maxshade # shaded conditions
      minshades <- micro$maxshade # shaded conditions
    } 
    else{
      maxshades <- micro$minshade + 1 # unshaded conditions
      minshades <- micro$minshade # unshaded conditions
    }
    
    # first thermoregulating "freely"
    for(i in 1:length(behav)){
      for(j in 1:length(bwater)){
        if(s) {
          ecto_name <- paste0('ecto_b',behav[i],'w',bwater[j],'_200cm_shade_',sc)
          shdburrow = 2
        }
        else {
          ecto_name <- paste0('ecto_b',behav[i],'w',bwater[j],'_200cm_sun_',sc)
          shdburrow = 0
        }
        ecto <- ectotherm(Ww_g =3.5, shape = 3, burrow=1,
                          T_F_min = 4, T_F_max = ubt, T_pref = m_tpref,
                          T_B_min = 4, T_RB_min = 4,
                          CT_max = 36, CT_min = -2,
                          diurn = 0, nocturn = 1, crepus = 1, shade_seek = 0, maxdepth = 10,
                          pct_wet = 90, burrowtmp = behav[i], burrowwtr = bwater[j],
                          maxshade=maxshades, minshade=minshades, shdburrow = shdburrow)
        # shdburrow = 2 to simulate conditions in the shade
        save(ecto, file=paste0('../results/', ecto_name, '.RData')) 
      }
    }
    
    # then thermoregulating to a maximum of 50 cm
    for(i in 1:length(behav)){
      for(j in 1:length(bwater)){
        if(s) {
          ecto_name <- paste0('ecto_b',behav[i],'w',bwater[j],'_50cm_shade_',sc)
          shdburrow = 2
        }
        else {
          ecto_name <- paste0('ecto_b',behav[i],'w',bwater[j],'_50cm_sun_',sc)
          shdburrow = 0
        }
        ecto <- ectotherm(Ww_g =3.5, shape = 3, burrow=1,
                          T_F_min = 4, T_F_max = ubt, T_pref = m_tpref,
                          T_B_min = 4, T_RB_min = 4,
                          CT_max = 36, CT_min = -2,
                          diurn = 0, nocturn = 1, crepus = 1, shade_seek = 0, maxdepth = 8,
                          pct_wet = 90, burrowtmp = behav[i], burrowwtr = bwater[j],
                          maxshade=maxshades, minshade=minshades, shdburrow = shdburrow)
        save(ecto, file=paste0('../results/', ecto_name, '.RData')) 
      }
    }
  }
}


##########################
# Compute metabolic rates from estimated Tbs
# and with seasonal acclimation (70%)

source('./aux_functions.R') # to load the smr.calc function that applies the model in
# line 26 to predict smr from tbs
ecto_files <- list.files(path='../results', pattern='ecto', full.names=T)
# pdf("../results/ecto_explore.pdf")
for(ec in ecto_files){
  load(ec)
  environ <- data.frame(ecto$environ)
  # plot(environ$TC, type='l')
  if("SMR" %in% colnames(environ)){next}
  else{
    ecto <- smr.calc(ecto)
    save(ecto, file=ec)
  }
}
# dev.off()

