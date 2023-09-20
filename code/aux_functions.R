
#########
# Auxiliary functions


# Calculate RMSEs
rmse.calc <- function(pred, obs, depth=1, micro){
  env_test <- pred
  env_test$datetime <- micro$dates
  env_test <- merge(env_test, obs, by="datetime")
  round(sqrt(mean((env_test[,ncol(env_test)] - env_test[,3+depth])^2)), 3)
}

# Calculate metabolic rates
smr.calc <- function(ecto){
  environ <- data.frame(ecto$environ)
  tcs <- data.frame(temp=environ$TC)
  environ$SMR <- predict(smr_model, newdata=tcs)
  environ$SMR_acc <- predict(smr_model, newdata=tcs) * 0.7
  ecto$environ <- environ
  return(ecto)
}

# function to load ectotherm objects
load_ectos <- function(path = path,
                       scenario = c("current", "cc45", "cc85", "gf45", "gf85", "hg45", "hg85"), 
                       movement = c("200cm", "50cm"), 
                       shading = c("sun","shade"), 
                       micro = micro){
  patt <- paste0("*", movement, "_", shading, "_", scenario)
  ecto_files <- list.files(path=path, pattern=patt, full.names=T)
  a <- 1
  for(ec in ecto_files){
    load(ec)
    ecto$environ$dates <- micro$dates
    ecto$environ$model <- strsplit(strsplit(ec, '/')[[1]][3], '.R')[[1]][1]
    if(a==1){ectos_df <- ecto$environ}
    else{ectos_df <- rbind(ectos_df, ecto$environ)}
    a = a+1
  }
  ectos_df$model <- as.factor(ectos_df$model)
  return(ectos_df)
}


# function to subset seasonally & when animals are predicted to be inactive
subset_env <- function(ectos_df, season = c("summer", "winter")){
  if(season == "summer"){
    # summer: 21 June – 23 September
    dstart <- 621; dfinish <- 923
    env_subset <- ectos_df[as.numeric(format(ectos_df$dates, "%m%d")) >= dstart &
                             as.numeric(format(ectos_df$dates, "%m%d")) <= dfinish,]
  } else {
    # winter: 22 December – 21 March
    dstart <- 1222; dfinish <- 321
    env_subset <- ectos_df[as.numeric(format(ectos_df$dates, "%m%d")) >= dstart |
                             as.numeric(format(ectos_df$dates, "%m%d")) <= dfinish,]
  }
  env_subset <- env_subset[env_subset$ACT == 0,]
  env_subset$model <- as.factor(env_subset$model)
  return(env_subset)
}


# function to format data to make statistical comparisons
# data = ec_cur_sun_200cm
format_env <- function(data){
  df_data <- data.frame(doy = rep(data$DOY, times=2), 
                        time = rep(data$TIME, times=2), 
                        smr = c(data$SMR, data$SMR_acc),
                        acc = as.factor(rep(c('no', 'yes'), each=nrow(data))), 
                        model = rep(data$model, times=2), 
                        therm = NA, 
                        hydro = NA)
  df_data$therm[grep("b1",df_data$model)] <- 'cold'
  df_data$therm[grep("b2",df_data$model)] <- 'warm'
  df_data$therm[grep("b3",df_data$model)] <- 'passive'
  df_data$hydro[grep("w0",df_data$model)] <- 'no-moist'
  df_data$hydro[grep("w1",df_data$model)] <- 'moist'
  df_data$therm <- as.factor(df_data$therm)
  df_data$hydro <- as.factor(df_data$hydro)
  return(df_data)
}

#########
# plotting functions

# plot cumulative O2 consumption (~cumulative smr)
plot_smr <- function(data, period=c('all', 'subset'), title='', 
                     xlab = "", ylab = expression(paste('cumulative ', O[2], ' consumed (ml)'))){
  csdf <- data %>%
    dplyr::group_by(model) %>%
    dplyr::mutate(cs = cumsum(SMR)) %>%
    dplyr::mutate(cs_acc = cumsum(SMR_acc))
  
  if(period == 'subset'){
    csdf$hour <- NA
    for(i in unique(csdf$model)){
      csdf$hour[csdf$model == i] <- 1:length(csdf$hour[csdf$model == i])
    }
    csdf %>% 
      ggplot() +
      geom_line(aes(hour, cs, color = model), linetype = "solid", linewidth = 0.7) +
      geom_line(aes(hour, cs_acc, color = model), linetype = "dashed", linewidth = 0.7) + 
      scale_colour_manual(values=c('#0059FF', '#86B0FF', '#FF8300', '#FEBB74', '#01BB04', '#8AFF86')) +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            legend.position = 'none') +
      ggtitle(title) +
      labs(x = xlab, y = ylab)
  } else {
    csdf %>% 
      ggplot() +
      geom_line(aes(dates, cs, color = model), linetype = "solid", linewidth = 0.7) +
      geom_line(aes(dates, cs_acc, color = model), linetype = "dashed", linewidth = 0.7) + 
      scale_colour_manual(values=c('#0059FF', '#86B0FF', '#FF8300', '#FEBB74', '#01BB04', '#8AFF86'),
                          labels=c('cold', 'cold-moist', 'warm', 'warm-moist', 'passive', 'passive-moist')) +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            legend.position = c(0.2, 0.70)) +
      ggtitle(title) +
      labs(x = xlab, y = ylab)
  }
}


# plot cumulative O2 consumption increase with CC scenario
plot_delta_smr <- function(data_baseline, data_cc,
                           period=c('all', 'summer', 'winter'), title='', 
                           xlab = "", ylab = 'Energy use change (kJ)',
                           legend="no"){
  data_cc$deltaSMR <- (data_cc$SMR - data_baseline$SMR) * 20.1 / 1000 # 20.1 Joules/ o2 ml
  data_cc$deltaSMR_acc <- (data_cc$SMR_acc - data_baseline$SMR_acc) * 20.1 / 1000 # 20.1 Joules/ o2 ml
  if(period == 'summer'){
    data_cc <- subset_env(data_cc, season = "summer")
  } else if(period == 'winter'){
    data_cc <- subset_env(data_cc, season = "winter")
  }
  csdf <- data_cc %>%
    dplyr::group_by(model) %>%
    dplyr::mutate(cs = cumsum(deltaSMR)) %>%
    dplyr::mutate(cs_acc = cumsum(deltaSMR_acc))
  
  if(period != 'all'){
    csdf$hour <- NA
    for(i in unique(csdf$model)){
      csdf$hour[csdf$model == i] <- 1:length(csdf$hour[csdf$model == i])
    }
    if(legend == "no"){
      csdf %>% 
        ggplot() +
        geom_line(aes(hour, cs, color = model), linetype = "solid", linewidth = 0.7) +
        geom_line(aes(hour, cs_acc, color = model), linetype = "dashed", linewidth = 0.7) + 
        geom_hline(yintercept=0, linetype='dashed') +
        scale_colour_manual(values=c('#0059FF', '#86B0FF', '#FF8300', '#FEBB74', '#01BB04', '#8AFF86')) +
        theme_bw() +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
              legend.position = 'none') +
        ggtitle(title) +
        labs(x = xlab, y = ylab)
    } else if(legend == "yes"){
      csdf %>% 
        ggplot() +
        geom_line(aes(hour, cs, color = model), linetype = "solid", linewidth = 0.7) +
        geom_line(aes(hour, cs_acc, color = model), linetype = "dashed", linewidth = 0.7) + 
        geom_hline(yintercept=0, linetype='dashed') +
        scale_colour_manual(name = "Behavior",
                            values=c('#0059FF', '#86B0FF', '#FF8300', '#FEBB74', '#01BB04', '#8AFF86'),
                            labels=c('cold', 'cold-moist', 'warm', 'warm-moist', 'passive', 'passive-moist')) +
        theme_bw() +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
              legend.position = c(0.2, 0.70)) +
        ggtitle(title) +
        labs(x = xlab, y = ylab)
    }
  } else {
    csdf %>% 
      ggplot() +
      geom_line(aes(dates, cs, color = model), linetype = "solid", linewidth = 0.7) +
      geom_line(aes(dates, cs_acc, color = model), linetype = "dashed", linewidth = 0.7) + 
      geom_hline(yintercept=0, linetype='dashed') +
      scale_colour_manual(name = "Behavior",
                          values=c('#0059FF', '#86B0FF', '#FF8300', '#FEBB74', '#01BB04', '#8AFF86'),
                          labels=c('cold', 'cold-moist', 'warm', 'warm-moist', 'passive', 'passive-moist')) +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            legend.position = c(0.2, 0.70)) +
      ggtitle(title) +
      labs(x = xlab, y = ylab)
  }
}


# plot selected depths
plot_depth <- function(data_list, title = '', subtitle = '', ylab='Depth (cm)', labels=c('yes', 'no')){ # data_list has to be a list length >= 1
  p <- ggplot()
  for(i in 1:length(data_list)){
    if(length(data_list) == 1){
      p <- p + geom_violin(data = data_list[[i]], aes(x = model, y = DEP, fill = model),
                           scale='width', position='identity')
    } else {
      p <- p + geom_violin(data = data_list[[i]], aes(x = model, y = DEP, fill = model),
                           scale='width', position='identity', alpha = 0.4)
    }
  }
  if(labels == 'no'){
    p +
      scale_fill_manual(values=c('#0059FF', '#86B0FF', '#FF8300', '#FEBB74', '#01BB04', '#8AFF86'),
                        labels=c('cold', 'cold-moist', 'warm', 'warm-moist', 'passive', 'passive-moist')) +
      scale_x_discrete(labels = c('cold', 'cold-moist', 'warm', 'warm-moist', 'passive', 'passive-moist')) +
      theme_classic() +
      theme(legend.position = "none", axis.text.x = element_blank()) + 
      ggtitle(title, subtitle = subtitle) +
      labs(x = '', y = ylab)
  } else if(labels == 'yes'){
    p +
      scale_fill_manual(values=c('#0059FF', '#86B0FF', '#FF8300', '#FEBB74', '#01BB04', '#8AFF86'),
                        labels=c('cold', 'cold-moist', 'warm', 'warm-moist', 'passive', 'passive-moist')) +
      scale_x_discrete(labels = c('cold', 'cold-moist', 'warm', 'warm-moist', 'passive', 'passive-moist')) +
      theme_classic() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle(title, subtitle = subtitle) +
      labs(x = '', y = ylab)
  }
}
