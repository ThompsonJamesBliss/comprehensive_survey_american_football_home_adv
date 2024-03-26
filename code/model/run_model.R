library(tidyverse)
library(rstan)
library(loo)

source("code/utils.R")

params <- list(
  min_season = 2004,
  models = c("model_1", "model_2", "model_3"),
  seed = 73097,
  chains = 3,
  iter = 1000,
  warmup = 500,
  adapt_delta = 0.95
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


for (model_name in params$models) {
  ### Create Directory for LOO Objects if doesn't exist
  if (!dir.exists(paste0("stan_results/", model_name, "/loo_objects"))) {
    dir.create(paste0("stan_results/", model_name, "/loo_objects"))
  }

  for (l in unique(df_model$league)) {
    df_model_temp <- df_model |>
      filter(league == l, season >= params$min_season) |>
      mutate(game_id = row_number()) |>
      gather(key = "side", value = "team", home_team, away_team) |>
      mutate(team = as.integer(factor(paste0(season, "_", team)))) |>
      spread(key = side, value = team) |>
      mutate(season = season - min(season) + 1) |>
      select(-c("game_id"))

    ### List of Stan Params
    stan_data <- list(
      num_clubs = length(unique(c(
        paste0(df_model_temp$season, "_", df_model_temp$home_team),
        paste0(df_model_temp$season, "_", df_model_temp$away_team)
      ))),
      num_games = nrow(df_model_temp),
      num_seasons = max(df_model_temp$season) - min(df_model_temp$season) + 1,
      num_school_matchup_types = max(df_model_temp$school_matchup_type),
      home_team_code = df_model_temp$home_team,
      away_team_code = df_model_temp$away_team,
      season = df_model_temp$season,
      distance = df_model_temp$distance,
      h_point_diff = df_model_temp$home_point_diff,
      h_adv = df_model_temp$home_game,
      school_matchup_type = df_model_temp$school_matchup_type
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


    rm("model")
    rm("loo_object")
    rm("df_DIC_temp")
  }
}
