library(tidyverse)
library(loo)
library(gt)
library(rstan)
options(mc.cores = parallel::detectCores() - 1)

source("code/utils.R")

params <- list(
  min_season = 2004,
  models = c("model_1", "model_2", "model_3"),
  lower_bound = 0.025,
  upper_bound = 0.975
)

df_HS <- data.frame(file_name = list.files("data/final/", pattern = "hs_games", full.names = T)) |>
  mutate(data = map(file_name, read_csv)) |>
  unnest() |>
  distinct() |>
  select(-c("date"))

df_NFL <- read_csv("data/final/NFL_games.csv") |>
  select(-c("date"))

df_NCAA <- read_csv("data/final/NCAA_games.csv") |>
  select(-c("date"))

df_model <- df_HS |>
  filter(in_state) |>
  rename(league = state_home) |>
  bind_rows(
    df_NFL |>
      mutate(league = "NFL")
  ) |>
  bind_rows(
    df_NCAA |>
      rename(league = division)
  ) |>
  filter(satisfies_cutoff) |>
  mutate(
    home_point_diff = home_score - away_score,
    home_game = as.integer(location == "Home")
  ) |>
  select(
    league, season,
    home_team, away_team, home_game,
    home_point_diff
  ) |>
  filter(!is.na(home_point_diff))



### Do the LOO Comparison for All Models
leagues <- gsub(".rds", "", list.files(paste0("stan_results/", unique(df_model$league), "/"),pattern = ".rds"))

df_loo <- 
  map_dfr(leagues, ~get_loo_comparison(league = .x, model_names, parameter_set_name))

### Visualize Results
df_loo <-
  df_loo %>%
  arrange(league, model_name) %>%
  select(league, model_name, contains('diff')) %>%
  mutate('n_se' = abs(elpd_diff/pmax(1e-6, se_diff))) %>%
  pivot_wider(id_cols = 'league',
              values_from = c(contains('diff'), 'n_se'),
              names_from = 'model_name') %>%
  mutate('league_group' = case_when(league == 'NFL' ~ 'NFL',
                                    league %in% c('FBS', 'FCS', 'Div II', 'Div III') ~ 'NCAA',
                                    T ~ 'High School')) %>%
  mutate('ha_2023' = NA,
         'ha_2023_lower' = NA,
         'ha_2023_upper' = NA)



df_min_seasons <- df_model |>
  group_by(league) |>
  summarise(
    min_season = min(season)
  )



### Add in 2023 HA estimate
### Note here Model 3 in our code is linear and model 2 is TV even though 
### we flip that convention in our paper
for(i in 1:nrow(df_loo)) {
  cat('Computing 2023 HA Estimate for', df_loo$league[i], '\n')
  ### Model 1 (Constant) Best
  if(df_loo$elpd_diff_model_1[i] == 0) {
    file <- paste0('stan_results/model_1/', df_loo$league[i], ".rds")
    stan_object <- read_rds(file)
    posterior <- extract(stan_object)
    
    ### Constant HA
    alpha_2023 <- posterior$alpha
  
  } else if(df_loo$elpd_diff_model_3[i] == 0) {  ### Model 2 (Linear Trend) Best
    file <- paste0('stan_results/model_2/', df_loo$league[i], ".rds")
    stan_object <- read_rds(file)
    posterior <- extract(stan_object)
    
    ### When we fit the model, the min-season (t0) is season 1 so the current 
    ### season shoudl be t_current - t0 + 1
    t0 <- max(df_min_seasons |> filter(league == df_loo$league[i]) |> pull(min_season), 2004)
    alpha_2023 <- posterior$alpha_intercept + posterior$alpha_trend * (2023 - t0 + 1)
    
  } else if(df_loo$elpd_diff_model_2[i] == 0) { ### Model 3 (Time Varying) Best 
    file <- paste0('stan_results/model_3/',  df_loo$league[i], ".rds")
    stan_object <- read_rds(file)
    posterior <- extract(stan_object)
    
    ### Most recent HA in TV model
    alpha_2023 <- posterior$alpha[,ncol(posterior$alpha)]
    
  }
  
  ### Posterior Mean and Quantiles
  df_loo$ha_2023[i] <- mean(alpha_2023)
  df_loo$ha_2023_lower[i] <- quantile(alpha_2023, params$lower_bound)
  df_loo$ha_2023_upper[i] <- quantile(alpha_2023, params$upper_bound)
  
}

write_csv(df_loo, 'analysis_code/analysis/loo_results.csv')