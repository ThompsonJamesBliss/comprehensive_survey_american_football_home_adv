library(tidyverse)
library(loo)
library(gt)
library(rstan)
library(ggridges)

options(mc.cores = parallel::detectCores() - 1)

source("code/utils.R")

params <- list(
  min_season = 2004,
  max_season = 2023,
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
  mutate(level = "High School") |>
  filter(in_state) |>
  rename(league = state_home) |>
  bind_rows(
    df_NFL |>
      mutate(league = "NFL",
             level = "NFL")
  ) |>
  bind_rows(
    df_NCAA |>
      rename(league = division) |>
      mutate(level = "NCAA")
  ) |>
  filter(satisfies_cutoff) |>
  mutate(
    home_point_diff = home_score - away_score,
    home_game = as.integer(location == "Home")
  ) |>
  select(
    league, season, level,
    home_team, away_team, home_game,
    home_point_diff
  ) |>
  filter(!is.na(home_point_diff))



### Do the LOO Comparison for All Models
leagues <- unique(df_model$league)

df_loo <- map_dfr(leagues,
                  ~get_loo_comparison(league = .x, model_names = params$models))

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

df_plot <- data.frame()

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
    alpha_current <- posterior$alpha
    
    df_plot <- bind_rows(df_plot,
                         data.frame(
                           league = df_loo$league[i],
                           model = "Model 1",
                           alpha = alpha_current
                         ))
    
  } else if(df_loo$elpd_diff_model_2[i] == 0) {  ### Model 2 (Linear Trend) Best
    file <- paste0('stan_results/model_2/', df_loo$league[i], ".rds")
    stan_object <- read_rds(file)
    posterior <- extract(stan_object)
    
    ### When we fit the model, the min-season (t0) is season 1 so the current 
    ### season shoudl be t_current - t0 + 1
    t0 <- max(df_min_seasons |> filter(league == df_loo$league[i]) |> pull(min_season), params$min_season)
    alpha_current <- posterior$alpha_intercept + posterior$alpha_trend * (params$max_season - t0 + 1)
    
    df_plot <- bind_rows(df_plot,
                         data.frame(
                           league = df_loo$league[i],
                           model = "Model 2",
                           alpha = alpha_current
                         ))
    
    
  } else if(df_loo$elpd_diff_model_3[i] == 0) { ### Model 3 (Time Varying) Best 
    file <- paste0('stan_results/model_3/',  df_loo$league[i], ".rds")
    stan_object <- read_rds(file)
    posterior <- extract(stan_object)
    
    ### Most recent HA in TV model
    alpha_current <- posterior$alpha[,ncol(posterior$alpha)]
    
    df_plot <- bind_rows(df_plot,
                         data.frame(
                           league = df_loo$league[i],
                           model = "Model 3",
                           alpha = alpha_current
                         ))
    
  }
  
  
  
  ### Posterior Mean and Quantiles
  df_loo$ha_2023[i] <- mean(alpha_current)
  df_loo$ha_2023_lower[i] <- quantile(alpha_current, params$lower_bound)
  df_loo$ha_2023_upper[i] <- quantile(alpha_current, params$upper_bound)
  
}

write_csv(df_loo, 'results/loo_results.csv')





ggplot(df_plot |>
         group_by(league) |>
         mutate(alpha_current = median(alpha)) |>
         ungroup() |>
         mutate(league_f = fct_reorder(league, alpha_current, .desc = T)) |>
         inner_join(df_model |>
                      select(league, level) |>
                      distinct(),
                    by = "league"),
       aes(x = alpha, y = league_f)) + 
  geom_vline(lty = 2, xintercept = 0) +
  geom_density_ridges(aes(fill = level), 
                      alpha = 0.5, 
                      quantiles = 0.5, 
                      quantile_lines = T, 
                      rel_min_height = 0.01) + 
  scale_x_continuous() +
  labs(x = 'Home Advantage (Points)',
       y = 'League',
       fill = '',
       title = 'Posterior Distributions for 2023 Home Advantage') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 18),
        legend.position = "bottom")

ggsave(paste0('results/ridge_plots_2023.jpg'),
       height = 16,
       width = 8)


### Latex Table
df_loo <- read_csv('results/loo_results.csv')

df_loo %>% 
  mutate('ha_23' = paste0(sprintf('%0.2f', ha_2023), ' (', sprintf('%0.2f', ha_2023_lower), ', ', sprintf('%0.2f', ha_2023_upper), ')')) %>% 
  arrange(desc(league_group)) %>% 
  select(league, ends_with('_1'), ends_with('_2'), ends_with('_3'), ha_23) %>% 
  xtable::xtable(align = 'cccccccccccc') %>% 
  print(include.rownames = F) %>% 
  gsub('0.00', '\\\\textit{0.00}', .) %>% 
  cat()




