library(rvest)
library(zipcodeR)
library(janitor)
library(tidyverse)

params <- list(
  seasons = seq(2023, 2023),
  seasons_to_remove = 2020,
  meters_to_miles = 0.00062137119
)

df_scores <- data.frame()

df_club <- data.frame()

for (s in params$seasons) {
  df_scores <- read_csv(paste0("https://masseyratings.com/scores.php?s=cf", s, "&sub=11604&all=1&mode=2&format=1"), col_names = F) |>
    mutate(division = "FBS") |>
    bind_rows(read_csv(paste0("https://masseyratings.com/scores.php?s=cf", s, "&sub=11605&all=1&mode=2&format=1"), col_names = F) |>
      mutate(division = "FCS")) |>
    bind_rows(read_csv(paste0("https://masseyratings.com/scores.php?s=cf", s, "&sub=11606&all=1&mode=2&format=1"), col_names = F) |>
      mutate(division = "Div II")) |>
    bind_rows(read_csv(paste0("https://masseyratings.com/scores.php?s=cf", s, "&sub=11620&all=1&mode=2&format=1"), col_names = F) |>
      mutate(division = "Div III")) |>
    rename(
      date = X2,
      team1_id = X3,
      team1_home = X4,
      score1 = X5,
      team2_id = X6,
      team2_home = X7,
      score2 = X8
    ) |>
    select(date, team1_id, team1_home, score1, team2_id, team2_home, score2, division) |>
    mutate(season = s) |>
    bind_rows(df_scores)



  df_club <- read_csv(paste0("https://masseyratings.com/scores.php?s=cf", s, "&sub=11604&all=1&mode=2&format=2"), col_names = F) |>
    mutate(division = "FBS") |>
    bind_rows(read_csv(paste0("https://masseyratings.com/scores.php?s=cf", s, "&sub=11605&all=1&mode=2&format=2"), col_names = F) |>
      mutate(division = "FCS")) |>
    bind_rows(read_csv(paste0("https://masseyratings.com/scores.php?s=cf", s, "&sub=11606&all=1&mode=2&format=2"), col_names = F) |>
      mutate(division = "Div II")) |>
    bind_rows(read_csv(paste0("https://masseyratings.com/scores.php?s=cf", s, "&sub=11620&all=1&mode=2&format=2"), col_names = F) |>
      mutate(division = "Div III")) |>
    mutate(season = s) |>
    rename(team_id = X1, team_name = X2) |>
    bind_rows(df_club)
}

df_link <- data.frame()

for (f in list.files("data/raw/ncaa", full.names = T, pattern = "html")) {
  page <- read_html(f)

  links <- page |>
    html_nodes("a") |>
    as.character()

  links <- links[str_count(links, pattern = "<") <= 2]

  links <- links[grepl("masseyratings.com/cf", links) &
    !grepl("masseyratings.com/cf/", links)]

  df_link <- data.frame(links = links) |>
    separate(links, sep = "<a href=\"|\">|</a>", into = c("DROP", "link", "team_name", "DROP0")) |>
    separate(link, sep = "/", into = c("DROP1", "DROP2", "DROP3", "DROP4", "team_website_id", "DROP5")) |>
    select(team_website_id, team_name) |>
    bind_rows(df_link)
}

df_link2 <- df_link |>
  mutate(
    team_name = gsub(" ", "_", team_name),
    team_name = gsub("&amp;", "&", team_name)
  ) |>
  distinct() |>
  filter(team_name %in% df_club$team_name)


df_longlat <- data.frame()

for (id in df_link2$team_website_id) {
  page <- read_html(paste0("https://masseyratings.com/", id))

  id_place <- page |>
    html_nodes("a") |>
    html_attr("href")

  id_place <- id_place[grepl("/place\\?v=", id_place)]

  id_place <- gsub("/place\\?v=", "", id_place)

  page2 <- read_html(paste0("https://masseyratings.com/map.php?v=", id_place))

  df_longlat <- data.frame(lng_lat = page2 |>
    html_text()) |>
    separate(lng_lat,
      sep = "mypts.push\\(new myPoint\\(",
      into = c("DROP", "lng_lat")
    ) |>
    select(-c("DROP")) |>
    separate(lng_lat,
      into = c("lat", "lng", "DROP"), sep = ",", extra = "merge"
    ) |>
    select(-c("DROP")) |>
    mutate(team_website_id = id) |>
    bind_rows(df_longlat)
}



df_club_long_lat <- df_club |>
  inner_join(df_link |>
    mutate(
      team_name = gsub(" ", "_", team_name),
      team_name = gsub("&amp;", "&", team_name)
    ) |>
    distinct()) |>
  inner_join(df_longlat)


df_scores_clean <- df_scores |>
  mutate(
    home_team_id = ifelse(team1_home %in% c(1, 0),
      team1_id,
      team2_id
    ),
    away_team_id = ifelse(team1_home %in% c(1, 0),
      team2_id,
      team1_id
    ),
    home_score = ifelse(team1_home %in% c(1, 0),
      score1,
      score2
    ),
    away_score = ifelse(team1_home %in% c(1, 0),
      score2,
      score1
    ),
    location = ifelse(team1_home != 0,
      "Home",
      "Neutral"
    ),
    date = paste0(
      substr(date, 1, 4),
      "-",
      substr(date, 5, 6),
      "-",
      substr(date, 7, 8)
    )
  ) |>
  select(season, date, location, division, home_team_id, away_team_id, home_score, away_score) |>
  left_join(df_club_long_lat |> select(team_id, team_name, lng, lat, season, division), by = c("home_team_id" = "team_id", "season", "division")) |>
  left_join(df_club_long_lat |> select(team_id, team_name, lng, lat, season, division),
    by = c("away_team_id" = "team_id", "season", "division"),
    suffix = c("_home", "_away")
  ) |>
  rename(
    home_team = team_name_home,
    away_team = team_name_away
  ) |>
  select(season, date, location, division, home_team, away_team, home_score, away_score, lng_home, lat_home, lng_away, lat_away) |>
  mutate_at(.vars = c("lng_home", "lng_away", "lat_home", "lat_away"), .funs = ~ as.numeric(.)) |>
  rowwise() |>
  mutate(
    distance = ifelse(location == "Neutral",
      0,
      params$meters_to_miles * geosphere::distVincentyEllipsoid(c(lng_home, lat_home), c(lng_away, lat_away))
    )
  ) |>
  as.data.frame() |>
  mutate(
    day_of_week = weekdays(as.Date(date)),
    satisfies_cutoff = !(season %in% params$seasons_to_remove)
  )


df_scores_clean |>
  write.csv(paste0("data/final/NCAA_games.csv"), row.names = F)
