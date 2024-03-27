library(tidyverse)

### Set Options for Model Name(s) we want to compare
model_names <- c('model_1', 'model_2', 'model_3')
parameter_set_name <- 'parameter_set_8'

get_diagnostics <- function(league, model_names, parameter_set_name) {
  ### Read in Objects
  diagnostics <- map_dfr(model_names, ~read_rds(paste0("stan_results/", .x, "/", parameter_set_name, "/diagnostics/", league, ".csv")))
  
  return(diagnostics)
}

leagues <- gsub(".csv", "", list.files(paste0("stan_results/", model_names[1], "/", parameter_set_name, "/"), pattern = ".rds"))

df_diagnostics <- map_dfr(leagues, ~get_diagnostics(league = .x, model_names, parameter_set_name))
write_csv(df_diagnostics, 'analysis_code/analysis/model_diagnostics.csv')



