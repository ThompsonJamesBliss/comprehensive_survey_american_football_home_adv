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
  lower_bound = 0.025,
  upper_bound = 0.975,
  leagues_plot_1 = c("NFL", "FBS", "FCS", "Div II",
              "Div III", "AL", "CA", "DE",
              "FL", "KY", "LA", "MD",
              "NC", "NH", "TN", "TX")
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


df_leagues <- bind_rows(df_HS |>
                          rename(league = state_home) |>
                          select(league) |>
                          mutate(level = "High School") |>
                          select(league, level) |>
                          distinct(),
                        
                        df_NCAA |>
                          rename(league = division) |>
                          mutate(level = "College") |>
                          select(league, level) |>
                          distinct(),
                        
                        
                        df_NFL |>
                          mutate(level = "NFL",
                                 league = "NFL") |>
                          select(league, level) |>
                          distinct(),
                        
)


df_alpha_vals_trend <- data.frame()

df_alpha_vals_cat <- data.frame()

df_alpha_vals_constant <- data.frame()

for(l in df_leagues$league){
  
  for(n in params$models){
  
    if(n == "model_2"){
  
      model <- readRDS(paste0("stan_results/", n, "/",  l, ".rds"))
      
      model_results <- extract(model)
      
      df_alpha_vals_trend <- df_alpha_vals_trend |>
        bind_rows(
          data.frame(alpha_trend = model_results$alpha_trend,
                     alpha_intercept = model_results$alpha_intercept,
                     league = l)
        )
    } else if(n == "model_3"){
      
      model <- readRDS(paste0("stan_results/", n, "/",  l, ".rds"))
      
      model_results <- extract(model)
      
      MINSEASON_temp <- max(c(df_min_seasons |>
        filter(league == l) |>
        pull(min_season),
        params$min_season))
        
      df_alpha_vals_cat <- df_alpha_vals_cat |>
        bind_rows(
          model_results$alpha |>
            as.data.frame() |>
            gather(key = "season", value = "alpha") |>
            mutate(season = MINSEASON_temp - 1 + as.integer(substr(season, 2, 3)),
                   league = l)
        )
      
    } else if(n == "model_1"){
      
      model <- readRDS(paste0("stan_results/", n, "/", l, ".rds"))
      
      model_results <- extract(model)
      
      df_alpha_vals_constant <- df_alpha_vals_constant |>
        bind_rows(
          data.frame(alpha = model_results$alpha,
                     league = l)
        )
      
    }
    
    
    rm(model)
    
    
  }
  
  
}





p1 <- ggplot() +
  
  geom_hline(data = df_alpha_vals_constant |>
               filter(league %in% params$leagues_plot_1) |>
               group_by(league) |>
               summarise(
                 alpha = mean(alpha)
               ) |>
               ungroup() |>
               mutate(col_value = "Constant HA (Model 1)",
                      league = factor(league, params$leagues_plot_1)),
             
             aes(yintercept = alpha, color = col_value),
             linetype = "dashed",
             lwd = 2) +
  
  geom_line(
    data = df_alpha_vals_trend |>
      filter(league %in% params$leagues_plot_1) |>
      group_by(league) |>
      summarise_all(~mean(.)) |>
      ungroup() |>
      mutate(season = list(c(params$min_season,
                             params$max_season))) |>
      unnest() |>
      mutate(alpha = alpha_intercept + alpha_trend* (season - params$min_season + 1),
             col_value = "Linear HA (Model 2)",
             league = factor(league, params$leagues_plot_1)),
    aes(season, y = alpha, color = col_value),
    lwd = 2
  ) +
  geom_point(data = df_alpha_vals_cat |>
               anti_join(bind_rows(df_HS |>
                           group_by(season, state_home) |>
                           filter(mean(satisfies_cutoff) == 0) |>
                           ungroup() |>
                           select(season, league = state_home) |>
                           distinct(),
                           df_NFL |>
                             group_by(season) |>
                             filter(mean(satisfies_cutoff) == 0) |>
                             ungroup() |>
                             select(season) |>
                             mutate(league = "NFL") |>
                             distinct(),
                           df_NCAA |>
                             group_by(season, division) |>
                             filter(mean(satisfies_cutoff) == 0) |>
                             ungroup() |>
                             select(season, league = division) |>
                             distinct()),
                         by = c("season", "league"))  |>
               inner_join(df_leagues) |>
               group_by(season, league) |>
               summarise(alpha_med = mean(alpha)) |>
               ungroup() |>
               mutate(col_value = "Time-Varying HA (Model 3)",
                      league = factor(league, params$leagues_plot_1)) |>
               filter(!is.na(league)),
             
             aes(season, alpha_med, color = col_value),
             alpha = 0.8,
             size = 4) +
  geom_point(data = bind_rows(df_HS |>
                                rename(league = state_home) |>
                                filter(satisfies_cutoff) |>
                                distinct(),
                              
                              df_NCAA   |>
                                filter(satisfies_cutoff)|>
                                rename(league = division),
                              
                              
                              df_NFL  |>
                                filter(satisfies_cutoff)|>
                                mutate(league = "NFL"),
                              
  ) |>
    filter(season >= 2004,
           league %in% params$leagues_plot_1) |>
    group_by(season, league) |>
    summarise(alpha = mean(home_score - away_score, na.rm = TRUE)) |>
    ungroup() |>
    mutate(col_value = "Empirical HA (Unadjusted)",
           league = factor(league, params$leagues_plot_1)),
  
  aes(season, alpha, color = col_value),
  size = 4,
  alpha = 0.8,
  shape = 17) +
  
  facet_wrap(~league, ncol = 4, nrow = 4) +
  theme_bw() +
  scale_color_brewer(palette = "Set1",
                     breaks = c("Constant HA (Model 1)",
                                "Time-Varying HA (Model 3)",
                               "Linear HA (Model 2)",
                               "Empirical HA (Unadjusted)")) +
  theme(strip.background = element_rect(fill = "black"),
        legend.position = "bottom",
        panel.spacing.y = unit(0, "lines"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Home Advantage by League and Season") +
  xlab("Season") +
  ylab("Home Advantage (Points)") +
  labs(color = "") +
  scale_y_continuous(breaks = seq(0,6, 3)) +
  coord_cartesian(ylim = c(-1, 6)) +
  scale_x_continuous(breaks = c(2004, 2013, 2022),
                     labels = c("'04",
                                "'13",
                                "'22"))  +
  theme(text = element_text(size = 22),
        strip.text = element_text(size = 18,
                                  color = "white")) +
  guides(color = guide_legend(nrow = 2))


ggsave("results/all_models_filtered.png", p1, width = 12, height = 12)




df_distances <- df_HS   |>
  rename(league = state_home) |>
  filter(satisfies_cutoff) |>
  bind_rows(
    df_NCAA |>
      rename(league = division)) |>
  bind_rows(df_NFL |>
              mutate(league = "NFL")) |>
  group_by(league, season) |>
  summarise(distance = mean(distance, na.rm = TRUE)) |>
  ungroup()

p2 <- df_alpha_vals_cat |>
  group_by(league, season) |>
  summarise(alpha = median(alpha)) |>
  ungroup() |>
  group_by(league) |>
  mutate(alpha = scale(alpha)[,1]) |>
  inner_join(df_distances  |>
               group_by(league) |>
               mutate(distance = scale(distance)[,1])) |>
  inner_join(df_leagues) |>
  mutate(level = factor(level,
                        c("NFL", "NCAA", "High School"))) |>
  ggplot(aes(distance, alpha, color = level)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", lwd = 1.25, se = FALSE,
              color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Average Distance Traveled (Z-Score)") +
  ylab("Estimated HA (Z-Score)") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "bottom") +
  
  labs(color = "", subtitle = bquote(League-Season~Estimates~(hat(α)[kt]))) +
  ggtitle("Home Advantage vs Distance Travelled")


ggsave("visualization/results/distance_alpha_z.png", p2, width = 6, height = 4)


ggplot(df_alpha_vals_trend |>
         group_by(league) |>
         mutate(median_trend = median(alpha_trend)) |>
         ungroup() |>
         mutate(league_f = fct_reorder(league, median_trend, .desc = T)) |> 
         inner_join(df_leagues), aes(x = alpha_trend, y = league_f)) + 
  geom_vline(lty = 2, xintercept = 0) +
  geom_density_ridges(aes(fill = level), 
                      alpha = 0.5, 
                      quantiles = 0.5, 
                      quantile_lines = T, 
                      rel_min_height = 0.01) + 
  scale_x_continuous(limits = c(-0.2, 0.3), 
                     breaks = seq(-0.5, 0.5, 0.05)) +
  labs(x = 'Change in Home Advantage (Points/Year)',
       y = 'League',
       fill = '',
       title = 'Posterior Distributions for HA Linear Trend') +
  theme(text = element_text(size = 18))

ggsave(paste0('visualization/results/ridge_plots.png'),
       height = 16,
       width = 8)







post_intervals <- df_alpha_vals_trend |> 
  group_by(league) |> 
  summarise('lower' = quantile(alpha_trend, 0.025),
            'upper' = quantile(alpha_trend, 0.975),
            'median' =  quantile(alpha_trend, 0.5),
            'mean' = mean(alpha_trend)) |> 
  mutate('contains_0' = map2_lgl(lower, upper, ~between(0, .x, .y)))

post_intervals |>
  write.csv("results/post_intervals.csv", row.names = FALSE)


