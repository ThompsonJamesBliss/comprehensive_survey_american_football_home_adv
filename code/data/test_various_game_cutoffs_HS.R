library(tidyverse)
library(viridis)

params <- list(final_choice_plot_minseason = 1,
  final_choice_plot_mingames = 7)

test_parameters = expand.grid(minseason = seq(1, 25),
                              mingames = seq(1, 16))

data <- data.frame(file_name = list.files("data/final/", pattern = "hs_games", full.names = T)) |>
  mutate(data = map(file_name, read_csv)) |>
  unnest() |>
  distinct() |>
  select(-c("date"))


df_results <- data.frame()

for(i in seq(1, nrow(test_parameters))){

  row <- test_parameters[i,]
  
  data_temp <- data
  
  row_count = nrow(data)
  row_count_prev = Inf
  
  while(row_count != row_count_prev){
    
    row_count_prev = row_count
    
    data_by_team_temp <- data_temp %>%
      #filter(!row$instateonly | in_state) %>%
      gather(key = "side", value = "team", home_team, away_team) %>%
      select(-c("side")) %>%
      group_by(team, season) %>%
      filter(n() >= row$mingames) %>%
      group_by(team) %>%
      filter(length(unique(season)) >= row$minseason) %>%
      ungroup() %>%
      select(team, season) %>%
      distinct()
    
    
    data_temp <- data_temp %>%
      inner_join(data_by_team_temp, by = c("home_team" = "team", "season"))%>%
      inner_join(data_by_team_temp, by = c("away_team" = "team", "season"))
    
    row_count = nrow(data_temp)
    
  }

  df_results <- row %>% bind_cols(data_temp %>%
    gather(key = "side", value = "team", home_team, away_team) %>%
    select(-c("side")) %>%
    summarise(team_count = length(unique(team)))) %>%
    bind_rows(df_results)
  
  
}



df_results %>%
  group_by(minseason, mingames) %>% #, instateonly) %>% 
  summarise(team_count = sum(team_count)) %>%
  ungroup() %>%
  bind_rows(test_parameters |>
              mutate(team_count = 0)) |>
  group_by(minseason, mingames) |>
  filter(team_count == max(team_count)) |>
  ungroup() |>
  mutate(team_count = team_count / max(team_count),
         is_red_box = ifelse(minseason == params$final_choice_plot_minseason &
                               mingames == params$final_choice_plot_mingames,
                             "firebrick1",
                             "black")) %>%
  arrange(is_red_box) %>%
  ggplot(aes(minseason, mingames, fill = team_count, label = round(100 * team_count))) +
  geom_tile(aes(color = is_red_box, lwd = is_red_box)) +
  geom_text(size = 4) +
  scale_fill_viridis() +
  scale_color_identity() +
  scale_linewidth_manual(values = c(0.2, 0.5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust =0.5),
        plot.subtitle = element_text(hjust =0.5)) +
  theme(legend.position = "none") +
  xlab("Minimum Season Threshold") +
  ylab("Minimum Game Threshold") +
  labs(size = "% of Teams Retained", subtitle = "(total unique teams = 35,460)") +
  scale_x_continuous(breaks = seq(1, max(df_results$minseason)), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(1, max(df_results$mingames)), expand = c(0, 0)) +
  ggtitle("% of Unique High School Teams Retained by Parameters")

ggsave("results/hs_teams_retained_param.png", width = 6, height = 4.5)
