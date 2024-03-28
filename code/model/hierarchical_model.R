library(tidyverse)
library(rstan)
library(loo)

source("code/utils.R")

params <- list(
  min_season = 2004,
  models = 'model_2_hierarchical',
  seed = 73097,
  chains = 4,
  iter = 5000,
  warmup = 1000,
  adapt_delta = 0.95
)

model_name <- params$models

### Set Parallelization
options(mc.cores = params$chains)


### Load Data
df_HS <- data.frame(file_name = list.files("data/final/", pattern = "hs_games", full.names = T)) |>
  mutate(data = map(file_name, read_csv)) |>
  unnest() |>
  distinct() |>
  select(-c("date"))


df_model <- df_HS |>
  filter(in_state) |>
  rename(league = state_home) |>
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


### Create Directory for LOO Objects if doesn't exist
if (!dir.exists(paste0("stan_results/", model_name, "/loo_objects"))) {
  dir.create(paste0("stan_results/", model_name, "/loo_objects"))
}

df_model_temp <- 
  df_model %>% 
  filter(season >= params$min_season) %>% 
  arrange(league) %>% 
  mutate('league_id' = as.integer(factor(league)),
         'game_id' = row_number()) %>% 
  pivot_longer(cols = contains('team'),
               names_to = 'side', 
               values_to = 'team') %>% 
  mutate('team' = as.integer(factor(paste0(season, "_", team)))) %>% 
  pivot_wider(names_from = 'side', 
              values_from = 'team') %>% 
  group_by(league) %>% 
  mutate('season' = season - min(season) + 1) %>%  ### rescale each season to min w/in league %>% 
  ungroup() 

num_clubs_league <- 
  df_model_temp %>% 
  group_by(league_id) %>% 
  summarise('n_teams' = n_distinct(c(home_team, away_team))) %>% 
  pull(n_teams)


### List of Stan Params
stan_data <- list(
  'num_clubs' = sum(num_clubs_league),
  'num_clubs_league' = num_clubs_league,
  'num_games' = nrow(df_model_temp),
  'num_seasons' = max(df_model_temp$season) - min(df_model_temp$season) + 1,
  'num_leagues' = max(df_model_temp$league_id),
  'home_team_code' = df_model_temp$home_team,
  'away_team_code' = df_model_temp$away_team,
  'season' = df_model_temp$season,
  'league' = df_model_temp$league_id,
  'h_point_diff' = df_model_temp$home_point_diff,
  'h_adv' = df_model_temp$home_game
)


### Fit Model
model <- stan(
  file = paste0("code/model/stan/", model_name, ".stan"),
  data = stan_data,
  seed = params$seed,
  chains = params$chains,
  iter = params$iter,
  warmup = params$warmup,
  control = list(adapt_delta = params$adapt_delta),
  pars = c("mu"),
  include = F
)

write_rds(model, paste0("stan_results/", model_name, "/", l, ".rds"))


### Make Sure that categorical variables for this model are 1-indexed so we
### can compute the log-likelihood from the posterior means (i.e. if season 4 is the
### first season for given model, make sure we recode that to be season 1 so we know
### first posterior mean corresponds to that season)
stan_data$season <- 1 + stan_data$season - min(stan_data$season)
stan_data$home_team_code <- 1 + stan_data$home_team_code - min(c(stan_data$away_team_code, stan_data$home_team_code))
stan_data$away_team_code <- 1 + stan_data$away_team_code - min(c(stan_data$away_team_code, stan_data$home_team_code))
stan_data$school_matchup_type <- 1 + stan_data$school_matchup_type - min(stan_data$num_school_matchup_type)

### Expand log-likelihood so we can compute LOO
### This re-derives pointwise log likelihood so that we don't have to
### store it in the model object itself, but that way we can get SE for our
### LOO in order to compare models via hypothesis testing
### Computing LOO takes a few seconds
log_lik <- expand_log_lik(model, model_name, stan_data)
loo_object <- loo(log_lik)
write_rds(loo_object, paste0("stan_results/", model_name,  "/loo_objects/", l, ".rds"))

### Compute DIC
DIC <-
  compute_dic(
    model = model,
    likelihood = model_name,
    stan_data = stan_data
  )

### Save Results
df_DIC_temp <-
  tibble(
    "model_name" = model_name,
    "league" = l,
    "DIC" = DIC
  )



write.table(df_DIC_temp,
            sep = ",",
            row.names = FALSE,
            paste0("stan_results/", model_name, "/model_DIC.csv"),
            append = TRUE,
            col.names = !file.exists(paste0("stan_results/", model_name, "/model_DIC.csv"))
)


### Compute Model Diagnostics
df_diagnostics <- 
  model_diagnostics(model) %>% 
  mutate('model_name' = model_name,
         'parameter_set_name' = parameter_set_name,
         'league' = l) 

write_csv(df_diagnostics, paste0("stan_results/", model_name, "/", parameter_set_name, "/diagnostics/", l, ".csv"))
