readdatafiles<- function(path, files, namesuffix , extension){
  walk(files, ~ assign(
    paste0(.x, namesuffix), 
    fread(paste0("data/", .x, ".csv")), 
    envir = .GlobalEnv
  ))
}

