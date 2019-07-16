library(dplyr)

(function() {
  install_and_load_pkgs <- function(x) x
  get_filepath <- function(filename, dir = NA) {
    if (dir == "public") {
      dirpath <- "X:/External and Public Data/"
    } else if (dir == "hotline") {
      dirpath <- "P:/Center for Victim Studies/INFONET/Protected Files/Research projects involving InfoNet data/SA and DV Calls/"
    } else if (dir == "deidentified") {
      dirpath <- "P:/Center for Victim Studies/INFONET/Protected Files/Research projects involving InfoNet data/Deidentified+Exploratory/"      
    }
    
    paste0(dirpath, filename)
  }
  
  load_csv <- function(filename, dir = NA, col_types = NA) {
    filepath <- get_filepath(filename, dir = dir)
    read_csv(filepath, col_types = col_types)
  }
  
  test_lm <- function(model) {
    list(summary(model), anova_stats(model))
  }

  # export
  list(
    install_and_load_pkgs = install_and_load_pkgs,
    load_csv = load_csv,
    test_lm = test_lm
  )
})()