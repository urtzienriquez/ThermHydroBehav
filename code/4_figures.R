
setwd('./code')

# load ectotherm objects
ecto_files <- list.files(path='../results', pattern='*sun.Rda', full.names=T)
ecto_shade_files <- list.files(path='../results', pattern='*shade.Rda', full.names=T)
ecto_files <- c(ecto_files, ecto_shade_files)
for(ec in ecto_files){
  load(ec)
  ecto_name <- strsplit(strsplit(ec, '/')[[1]][3], '.R')[[1]][1]
  assign(ecto_name, ecto)
}

ls()[grep('sun', ls())]
