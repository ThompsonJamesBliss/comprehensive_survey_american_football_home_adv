library(tidyverse)

### Set Options for Model Name(s) we want to compare
model_names <- c('model_1', 'model_2', 'model_3')

get_diagnostics <- function(league, model_names) {
  ### Read in Objects
  diagnostics <- map_dfr(model_names, ~read_csv(paste0("stan_results/", .x, "/diagnostics/", league, ".csv")))
  
  return(diagnostics)
}

leagues <- gsub(".rds", "", list.files(paste0("stan_results/", model_names[1], "/loo_objects"), pattern = ".rds"))

df_diagnostics <- map_dfr(leagues, ~get_diagnostics(league = .x, model_names))
write_csv(df_diagnostics, 'results/model_diagnostics.csv')

### Make Tex Tables
df_1 <- 
  df_diagnostics %>% 
  filter(model_name == 'model_1') %>% 
  group_by(league) %>% 
  mutate('max_rhat' = max(max_rhat),
         'min_rhat' = min(min_rhat)) %>% 
  ungroup() %>% 
  select(-rhat_pct, -model_name) %>% 
  pivot_wider(names_from = 'parameter', 
              values_from = 'mean_ess') %>% 
  select(league, min_rhat, max_rhat, everything())

df_2 <- 
  df_diagnostics %>% 
  filter(model_name == 'model_2') %>% 
  group_by(league) %>% 
  mutate('max_rhat' = max(max_rhat),
         'min_rhat' = min(min_rhat)) %>% 
  ungroup() %>% 
  select(-rhat_pct, -model_name) %>% 
  pivot_wider(names_from = 'parameter', 
              values_from = 'mean_ess') %>% 
  select(league, min_rhat, max_rhat, everything())

df_3 <- 
  df_diagnostics %>% 
  filter(model_name == 'model_3') %>% 
  group_by(league) %>% 
  mutate('max_rhat' = max(max_rhat),
         'min_rhat' = min(min_rhat)) %>% 
  ungroup() %>% 
  select(-rhat_pct, -model_name) %>% 
  pivot_wider(names_from = 'parameter', 
              values_from = 'mean_ess') %>% 
  select(league, min_rhat, max_rhat, everything())



df_1 %>% 
  xtable::xtable(align = rep('c', ncol(.) + 1), 
                 digits = c(0, 0, 2, 2, rep(0, ncol(.) - 3))) %>% 
  print(include.rownames = F) 

df_2 %>% 
  xtable::xtable(align = rep('c', ncol(.) + 1), 
                 digits = c(0, 0, 2, 2, rep(0, ncol(.) - 3))) %>% 
  print(include.rownames = F) 

df_3 %>% 
  xtable::xtable(align = rep('c', ncol(.) + 1), 
                 digits = c(0, 0, 2, 2, rep(0, ncol(.) - 3))) %>% 
  print(include.rownames = F) 


