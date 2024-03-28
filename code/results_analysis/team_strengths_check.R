library(broom)
library(rgeos)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(viridis)
library(ggridges)
library(tidyverse)
library(rstan)

params <- list(
  min_season = 2004,
  max_season = 2023,
  models = c("model_1", "model_2", "model_3"),
  division = c("FBS", "FCS", "NCAA D2", "NCAA D3")
)

df_NCAA <- read_csv("data/final/NCAA_games.csv") |>
  select(-c("date"))

df_results_team_strength <- data.frame()

for(l in unique(df_NCAA$division)){
  
  for(n in params$models){
    
    df_map <- df_NCAA |>
      filter(satisfies_cutoff) |>
      filter(division == l, season >= params$min_season) |>
      mutate(game_id = row_number()) |>
      gather(key = "side", value = "team", home_team, away_team) |>
      mutate(team_name = paste0(season, "_", team),
             team = as.integer(factor(paste0(season, "_", team)))) |>
      select(team_name, team) |>
      distinct()
    
    model <- readRDS(paste0("stan_results/", n, "/",  l, ".rds"))
    
    model_results <- extract(model)
    
    df_results_team_strength_temp <- data.frame()
    
    for(t in seq(model_results$theta[1,])){
      
      df_results_team_strength_temp <- data.frame(team = t,
                                                  theta = median(model_results$theta[,t]),
                                                  model = n) |>
        bind_rows(df_results_team_strength_temp)
      
    }
    
    rm("model")
    
    df_results_team_strength <- bind_rows(df_results_team_strength,
                                          df_results_team_strength_temp |>
                                            inner_join(df_map) |>
                                            mutate(division = l))
    
    
  }
  
  
}

df_rankings <- data.frame()

for(f in list.files("data/raw/ncaa", pattern = ".csv")){
  
  df_rankings <- read_csv(paste0("data/raw/ncaa/", f)) |>
    mutate(file = f) |>
    select(file, team = Team, rank_massey = Rat) |>
    bind_rows(df_rankings)
  
  
}


df_loo_results <- read_csv("results/loo_results.csv") |>
  filter(league %in% unique(df_NCAA$division)) |>
  select(league, elpd_diff_model_1, elpd_diff_model_2,
         elpd_diff_model_3) |>
  pivot_longer(cols = -c("league"), names_to = "model", values_to = "elpd") |>
  mutate(
    model = gsub("elpd_diff_", "", model)
  ) |>
  group_by(league) |>
  filter(elpd == max(elpd)) |>
  ungroup() |>
  select(-c("elpd"))


df_ranks_compare <- df_results_team_strength |>
  select(-c("team")) |>
  separate(team_name,
           sep = "_",
           into = c("season", "team"),
           convert = TRUE,
           extra = "merge") |>
  mutate(team = gsub("_", " ", team)) |>
  inner_join(df_loo_results,
             by = c("model",
                    "division"= "league")) |>
  group_by(season, division) |>
  mutate(rank_benz_bliss_lopez = rank(-theta,
                                        ties.method = "min")) |>
  ungroup() |>
  inner_join(
    
    df_rankings |>
      mutate(file = stringi::stri_reverse(file)) |>
      separate(file, sep = "_", into = c("season", "division"), extra = "merge") |>
      mutate(season = as.integer(gsub(".csv", "", stringi::stri_reverse(season))),
             division = gsub("_", " ", stringi::stri_reverse(division)))
    
  )  |>
  summarise(
    rank_correlation = cor(rank_massey, rank_benz_bliss_lopez, method = "spearman")
  )
  

write.csv(df_ranks_compare, "results/rank_correlation.csv")