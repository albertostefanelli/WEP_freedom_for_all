### run all the script in the `scripts` folder
library(tidyverse)
library(here)
all_items <- list.files(here("scripts"), full.names = TRUE)
script_files <- all_items[!file.info(all_items)$isdir & basename(all_items) != "batch_run.R"]

for (s in script_files){
  start_time <- now()
  message("Starting script: ", basename(s), " at ", start_time)
  
  tryCatch({
    source(s)
  }, error = function(e) {
    message("Error occurred: ", conditionMessage(e))
  }, finally = {
    # Clean the environment after each script is executed or if it fails
    rm(list = setdiff(ls(), c("script_files", "all_items") ), envir = .GlobalEnv)
  })
  rm(list = setdiff(ls(), c("script_files", "all_items") ), envir = .GlobalEnv)
}

