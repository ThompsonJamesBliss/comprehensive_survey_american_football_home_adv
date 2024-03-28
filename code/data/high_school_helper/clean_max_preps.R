df_games <- data.frame()
df_club <- data.frame()

# getting all states
states <- state.abb |>
  tolower() |>
  append("dc")


for (s in params$seasons) {
  
  df_club <- read_csv(paste0("data/raw/high_school/club_", s, ".csv")) |>
    mutate(season = s) |>
    bind_rows(df_club)
  
  for (state in params$states) {
    df_games <- bind_rows(
      df_games,
      read_csv(paste0("data/raw/high_school/", s, "/Games_", state, ".csv")) |>
        mutate(state = state)
    )
  }
}

df_club <- df_club |>
  bind_rows(
    anti_join(
      df_games |>
        select(team1, team2) |>
        gather(key = "type", value = "team_id") |>
        select(-c("type")) |>
        drop_na() |>
        distinct(),
      df_club
    )
  )

df_addresses_final <- read_csv("data/raw/high_school/long_lat_geo_code.csv") |>
  rename(lng = long)

df_club_clean <- df_club |>
  mutate_all(~ str_trim(iconv(., "UTF-8", "UTF-8", sub = ""))) |>
  mutate(
    colors = gsub("([a-z])([A-Z])", "\\1; \\2", colors),
    team_id_original = team_id
  ) |>
  left_join(df_addresses_final, by = c("team_id_original" = "team_id")) |>
  mutate(team_id = gsub(" ", "_", str_trim(gsub("/", " ", team_id)))) |>
  separate(team_id, sep = "_", into = c("state_merge", "city_merge"), remove = F, extra = "drop") |>
  group_by(state_merge, city_merge, hs_name) |>
  mutate(
    team_id = ifelse(length(unique(team_id)) > 1 &
      !is.na(hs_name),
    last(team_id),
    team_id
    )
  ) |>
  ungroup() |>
  group_by(team_id) |>
  fill(colors, mascot, school_type, hs_name, .direction = "updown") |>
  mutate(
    colors = last(colors),
    mascot = last(mascot),
    school_type = last(school_type),
    hs_name = last(hs_name),
    address = last(address)
  ) |>
  ungroup() |>
  select(-c("season")) |>
  distinct() |>
  select(team_id, hs_name, address, school_type, colors, mascot, lng, lat, team_id_original) |>
  distinct()

df_games2 <- df_games |>
  filter(level == "varsity") |>
  select(-c("level")) |>
  select(team1, team2, score1, score2, state, game_type, location, season, date, date_fill, team2_fill) |>
  distinct() |>
  filter(
    !is.na(score1),
    !is.na(score2),
    score1 != 1 | ((score2 == 6) | score2 >= 8),
    score2 != 1 | ((score1 == 6) | score1 >= 8),
    !(score1 == 0 & score2 == 0),
    score1 < 1000, score2 < 1000
  ) |>
  left_join(df_club_clean |> select(team_id, team_id_original), by = c("team1" = "team_id_original")) |>
  select(-c("team1")) |>
  rename(team1 = team_id) |>
  left_join(df_club_clean |> select(team_id, team_id_original), by = c("team2" = "team_id_original")) |>
  select(-c("team2")) |>
  rename(team2 = team_id) |>
  mutate(team2_fill = str_trim(tolower(team2_fill))) |>
  group_by(team2_fill, state) |>
  fill(team2, .direction = "updown") |>
  ungroup() |>
  select(-c("team2_fill", "state")) |>
  filter(!is.na(team2)) |>
  distinct() |>
  separate(date, sep = "/freshman-football", into = c("date"), extra = "drop") |>
  separate(date, sep = "-", into = c("month", "day", "year")) |>
  mutate(
    date = ifelse(as.integer(year) >= 2004,
      paste0(year, "-", str_pad(width = 2, side = "left", pad = "0", month), "-", str_pad(width = 2, side = "left", pad = "0", day)),
      NA_character_
    ),
    season = ifelse(!is.na(date),
      ifelse(as.integer(month) >= 7,
        year,
        as.character(as.integer(year) - 1)
      ),
      NA_character_
    ),
    game_type = case_when(
      grepl("Playoff", game_type) ~ "Playoff",
      grepl("Tournament", game_type) ~ "Tournament",
      grepl("Non-", game_type) ~ "Non-Conference",
      T ~ "Conference"
    )
  ) |>
  filter(!is.na(date), !is.na(year), season != 2003) |>
  select(-c("year", "month", "day", "date_fill")) |>
  distinct() |>
  mutate(
    switch = team2 > team1,
    temp = team1,
    team1 = ifelse(switch,
      team2,
      team1
    ),
    team2 = ifelse(switch,
      temp,
      team2
    ),
    temp = score1,
    score1 = ifelse(switch,
      score2,
      score1
    ),
    score2 = ifelse(switch,
      temp,
      score2
    ),
    location = case_when(
      switch & location == "Away" ~ "Home",
      switch & location == "Home" ~ "Away",
      T ~ location
    )
  ) |>
  group_by(team1, team2, score1, score2, date) |>
  mutate(
    location = case_when(
      length(unique(location)) == 1 ~ location,
      length(unique(location)) == 3 ~ "Neutral",
      sum(location == "Home" & !switch) > 0 |
        sum(location == "Away" & switch) > 0 ~ "Home",
      T ~ "Neutral"
    ),
    game_type = case_when(
      sum(game_type == "Playoff") > 1 ~ "Playoff",
      sum(game_type == "Tournament") > 1 ~ "Tournament",
      sum(game_type == "Non-Conference") > 1 ~ "Non-Conference",
      T ~ "Conference"
    )
  ) |>
  ungroup() |>
  select(-c("temp", "switch")) |>
  distinct() |>
  mutate(
    home_team = case_when(
      location %in% c("Home", "Neutral") ~ team1,
      location == "Away" ~ team2
    ),
    away_team = case_when(
      location %in% c("Home", "Neutral") ~ team2,
      location == "Away" ~ team1
    ),
    home_score = case_when(
      location %in% c("Home", "Neutral") ~ score1,
      location == "Away" ~ score2
    ),
    away_score = case_when(
      location %in% c("Home", "Neutral") ~ score2,
      location == "Away" ~ score1
    ),
    location = ifelse(location == "Neutral",
      "Neutral",
      "Home"
    )
  ) |>
  filter(away_team != home_team) |>
  select(season, date, location, game_type, home_team, away_team, home_score, away_score) |>
  arrange(season, date, location, game_type) |>
  distinct()

df_club_clean2 <- df_club_clean |>
  select(-c("team_id_original")) |>
  distinct() |>
  arrange(team_id)



df_games_clean <- df_games2 |>
  as.data.frame() |>
  filter(home_score < 1000, away_score < 1000) |>
  left_join(df_club_clean2 |>
              as.data.frame(),
            by = c("home_team" = 'team_id'))|>
  left_join(df_club_clean2 |>
              as.data.frame(),
            by = c("away_team" = 'team_id'),
            suffix = c("_home", '_away')) |> 
  
  mutate(state_home = toupper(substr(home_team, 1, 2)),
         
         state_home = ifelse(state_home == "DC", 
                             "MD",
                             state_home),
         
         state_away = toupper(substr(away_team, 1, 2)),
         
         state_away = ifelse(state_away == "DC", 
                             "MD",
                             state_away),
         
         in_state = state_home == state_away,
         
         distance = ifelse(location == "Neutral",
                      0,
                      params$meters_to_miles * geosphere::distVincentyEllipsoid(cbind(lng_home, lat_home),
                                                                                cbind(lng_away, lat_away))
    )
  )


df_games_clean <- df_games_clean |>
  filter(level == "varsity" )|>
  select(-c("level"))


df_HS_temp <- df_games_clean  |>
  select(home_team, away_team, in_state, season, state_home, state_away)

row_count = nrow(df_HS_temp)
row_count_prev = Inf

while(row_count != row_count_prev){
  
  row_count_prev = row_count
  
  df_HS_by_team_temp <- df_HS_temp |>
    filter(in_state,
           !(season %in% params$seasons_to_remove)) |>
    gather(key = "side", value = "team", home_team, away_team) |>
    select(-c("side")) |>
    filter(!(paste0(state_home, "_", season) %in% params$state_season_combos_to_remove),
           !(paste0(state_away, "_", season) %in% params$state_season_combos_to_remove)) |>
    group_by(team, season) |>
    filter(n() >= params$mingames) |>
    group_by(team) |>
    filter(length(unique(season)) >= params$minseason) |>
    ungroup() |>
    select(team, season) |>
    distinct()
  
  
  df_HS_temp <- df_HS_temp |>
    inner_join(df_HS_by_team_temp, by = c("home_team" = "team", "season"))|>
    inner_join(df_HS_by_team_temp, by = c("away_team" = "team", "season"))
  
  row_count = nrow(df_HS_temp)
  
}

#making changes
df_games_clean2 <- df_games_clean|>
  left_join(df_HS_by_team_temp |> mutate(satisfies_cutoff_home = T) , by = c("home_team" = "team", "season"))|>
  left_join(df_HS_by_team_temp |> mutate(satisfies_cutoff_away = T) , by = c("away_team" = "team", "season")) |>
  mutate(satisfies_cutoff = ifelse(!is.na(satisfies_cutoff_home) & !is.na(satisfies_cutoff_away) & in_state,
                                   T,
                                   F)) |>
  select(-c("satisfies_cutoff_home", "satisfies_cutoff_away")) |>
  select(season, date, location, game_type, home_team, away_team,
         home_score, away_score,
         state_home, state_away, lng_home, lat_home, lng_away, lat_away, distance,
         satisfies_cutoff, in_state) 

for(s in params$seasons){
  
  df_games_clean2 |>
    filter(season == s) |>
    write.csv(paste0("data/final/hs_games_", s, ".csv"), row.names = F)
  
}
