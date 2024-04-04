library(tidyverse)
library(rstan)

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


df_min_seasons <- bind_rows(df_HS |>
                              filter(in_state) |>
                              rename(league = state_home),
                            
                            df_NCAA |>
                              rename(league = division),
                            
                            
                            df_NFL |>
                              mutate(league = "NFL")) |>
  filter(satisfies_cutoff) |>
  group_by(league) |>
  summarise(
    min_season = min(season)
  )



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

df_small <- 
  df_model %>% 
  filter(season >= params$min_season) %>% 
  arrange(league) %>% 
  group_by(league) %>% 
  summarise('n' = n()) %>%  
  ungroup() %>% 
  filter(n <= 10000) %>% 
  mutate('small_id' = 1:nrow(.))

df_counts <- 
  df_model %>% 
  filter(season >= params$min_season) %>% 
  arrange(league) %>% 
  group_by(league) %>% 
  count()

df_counts %>% 
  arrange(-n) %>% 
  xtable::xtable(align = 'ccc', caption = '# of Games') %>% 
  print(format.args = list(big.mark = ","), include.rownames = F) 


### Model 2 (non hierarhical HA estimates) 
df_diff <- 
  df_HS %>% 
  distinct('league' = state_home) %>% 
  arrange(league) %>% 
  mutate('league_id' = 1:nrow(.)) %>% 
  left_join(df_small, by = 'league') %>% 
  mutate('slope_model_2' = NA,
         'ha_model_2' = NA,
         'slope_model_2h' = NA,
         'ha_model_2h' = NA,
         'slope_model_2h_small' = NA,
         'ha_model_2h_small' = NA)



### Load Model 2H
model_2h <- read_rds('stan_results/model_2_hierarchical.rds')
posterior_2h <- extract(model_2h) 
intercepts_2h <- colMeans(posterior_2h$alpha_intercept)
trends_2h <- colMeans(posterior_2h$alpha_trend)

small_model_2h <- read_rds('stan_results/model_2_hierarchical_small.rds')
small_posterior_2h <- extract(small_model_2h) 
small_intercepts_2h <- colMeans(small_posterior_2h$alpha_intercept)
small_trends_2h <- colMeans(small_posterior_2h$alpha_trend)

for(i in 1:nrow(df_diff)) {
  ### Model 2
  file <- paste0('/n/holyscratch01/haneuse_lab/lbenz/stan_results/model_2/', df_diff$league[i], ".rds")
  stan_object <- read_rds(file)
  posterior <- extract(stan_object)
  
  ### When we fit the model, the min-season (t0) is season 1 so the current 
  ### season shoudl be t_current - t0 + 1
  t0 <- max(df_min_seasons |> filter(league == df_diff$league[i]) |> pull(min_season), params$min_season)
  alpha_current <- posterior$alpha_intercept + posterior$alpha_trend * (params$max_season - t0 + 1)
  
  df_diff$ha_model_2[i] <- mean(alpha_current)
  df_diff$slope_model_2[i] <- mean(posterior$alpha_trend)
  
  df_diff$slope_model_2h[i] <- trends_2h[i]
  df_diff$ha_model_2h[i] <- intercepts_2h[i] + trends_2h[i] * (params$max_season - t0 + 1)
  
  if(!is.na(df_diff$small_id[i])) {
    df_diff$slope_model_2h_small[i] <- small_trends_2h[df_diff$small_id[i]]
    df_diff$ha_model_2h_small[i] <- small_intercepts_2h[df_diff$small_id[i]] + trends_2h[df_diff$small_id[i]] * (params$max_season - t0 + 1)
    
  }
  
}

ggplot(df_diff, aes(x = ha_model_2, y = ha_model_2h)) + 
  geom_abline(col = 'red', slope = 1, intercept = 0, lty = 2, lwd = 1.2) + 
  scale_x_continuous(limits = c(0.5, 2.8)) + 
  scale_y_continuous(limits = c(0.5, 2.8)) + 
  geom_point() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 18),
        legend.position = "bottom") + 
  labs(x = '2023 HA (Model 2)',
       y = '2023 HA (Model 2 Hierachical)',
       title = 'Comparison Between Model 2 HA Estimates',
       subtitle = 'All 50 States')

ggsave('visualization/results/model2_HA_comp.jpg', height = 9/1.2, width = 16/1.2)


ggplot(df_diff, aes(x = ha_model_2, y = ha_model_2h_small)) + 
  geom_abline(col = 'red', slope = 1, intercept = 0, lty = 2, lwd = 1.2) + 
  scale_x_continuous(limits = c(0.5, 2.8)) + 
  scale_y_continuous(limits = c(0.5, 2.8)) + 
  geom_text(aes(label = league), nudge_y = 0.075, color = 'blue') +
  geom_point() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 18),
        legend.position = "bottom") + 
  labs(x = '2023 HA (Model 2)',
       y = '2023 HA (Model 2 Hierachical)',
       title = 'Comparison Between Model 2 HA Estimates',
       subtitle = 'States w/ Fewer than 10,000 Games')

ggsave('visualization/results/model2_HA_comp_small.jpg', height = 9/1.2, width = 16/1.2)


ggplot(df_diff, aes(x = slope_model_2, y = slope_model_2h)) + 
  geom_abline(col = 'red', slope = 1, intercept = 0, lty = 2, lwd = 1.2) + 
  scale_x_continuous(limits = c(-0.085 , 0.085)) + 
  scale_y_continuous(limits = c(-0.085 , 0.085)) + 
  geom_vline(xintercept = 0, lty = 2, lwd = 1.2, alpha = 0.2) + 
  geom_hline(yintercept = 0, lty = 2, lwd = 1.2, alpha = 0.2) + 
  geom_point(alpha = 0.5) + 
  geom_label(data = filter(df_diff, league %in% c('TX', 'CA', 'OH', 'PA')), 
             aes(label = league), 
             color = 'blue',
             nudge_y = -0.001) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 18),
        legend.position = "bottom") + 
  labs(x = 'HA Linear Trend (Model 2)',
       y = 'HA Linear Trend (Model 2 Hierachical)',
       title = 'Comparison Between Model 2 HA Linear Trends',
       subtitle = 'All 50 States')

ggsave('visualization/results/model2_HA_trend_comp.jpg', height = 9/1.2, width = 16/1.2)

ggplot(df_diff, aes(x = slope_model_2, y = slope_model_2h_small)) + 
  geom_abline(col = 'red', slope = 1, intercept = 0, lty = 2, lwd = 1.2) + 
  geom_vline(xintercept = 0, lty = 2, lwd = 1.2, alpha = 0.2) + 
  geom_hline(yintercept = 0, lty = 2, lwd = 1.2, alpha = 0.2) + 
  scale_x_continuous(limits = c(-0.085 , 0.085)) + 
  scale_y_continuous(limits = c(-0.085 , 0.085)) + 
  geom_point() + 
  geom_text(aes(label = league), nudge_y = 0.005, color = 'blue') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 18),
        legend.position = "bottom") + 
  labs(x = 'HA Linear Trend (Model 2)',
       y = 'HA Linear Trend (Model 2 Hierachical)',
       title = 'Comparison Between Model 2 HA Linear Trends',
       subtitle = 'States w/ Fewer than 10,000 Games')

ggsave('visualization/results/model2_HA_trend_comp_small.jpg', height = 9/1.2, width = 16/1.2)
