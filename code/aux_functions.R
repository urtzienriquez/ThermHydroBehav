
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

# TO DO

#     scenario = "cur" # c("cur", "C", "G", "H")
#     movement = "200cm" # c("200cm", "50cm")
#     shading = "sun" # c("sun","shade")

# and make it rearrange the data to have cols for models, constraints, 
# acc/no acc (for that might need to rbind)
# each ecto goes twice: once with smr and the other with acc_smr

load_ectos <- function(scenario = c("cur", "C", "G", "H"), movement = c("200cm", "50cm"), 
                       shading = c("sun","shade"), micro = micro){
  if(scenario == "cur"){
    patt <- paste0("*",movement,"_",shading,".Rda")
  }
  ecto_files <- list.files(path='../results', pattern=patt, full.names=T)
  
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


# some plotting function


