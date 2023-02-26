
setwd('./code')

# load ectotherm objects
ecto_files <- list.files(path='../results', pattern='ecto', full.names=T)
failed_models <- c()
for(ec in ecto_files){
  tryCatch(load(ec), error = function(e) {return(NULL)})
  ecto_name <- strsplit(strsplit(ec, '/')[[1]][3], '.R')[[1]][1]
  if(is.null(ec)){
    failed_models <- c(failed_models, ecto_name)
    next
  }
  else{
    assign(ecto_name, ec)
  }
}