
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