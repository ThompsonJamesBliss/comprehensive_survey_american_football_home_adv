library(rvest)
library(stringi)
library(tidygeocoder)
library(stringr)
library(tidyverse)

params <- list(
  seasons = seq(2004, 2023),
  states = append("dc", tolower(state.abb)),
  minseason = 1,
  mingames = 7,
  instateonly = T,
  seasons_to_remove = 2020,
  state_season_combos_to_remove = c("OR_2006", "OR_2005", "OR_2004",
                                    "MD_2006", "MD_2005", "MD_2004",
                                    "MD_2007"),
  rescrape_data = FALSE,
  repull_long_lat = FALSE,
  meters_to_miles = 0.00062137119
)

source("code/utils.R")

if(params$rescrape_data){

  source("code/data/high_school_helper/scrape_max_preps.R")
  
  rm(list = setdiff(ls(), c(
    "params",
    lsf.str()
  )))

}

if(params$repull_long_lat){

  source("code/data/high_school_helper/get_long_lat_addresses.R")
  
  rm(list = setdiff(ls(), c(
    "params",
    lsf.str()
  )))


}

source("code/data/high_school_helper/clean_max_preps.R")
