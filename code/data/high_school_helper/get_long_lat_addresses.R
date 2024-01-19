library(dplyr)
library(readr)

state_bounds <- zipcodeR::zip_code_db |>
  group_by(state) |>
  summarise(
    lng_min = min(bounds_east, na.rm = TRUE) - 1,
    lng_max = max(bounds_west, na.rm = TRUE) + 1,
    lat_min = min(bounds_south, na.rm = TRUE) - 1,
    lat_max = max(bounds_north, na.rm = TRUE) + 1
  )

df_club <- data.frame()

for (s in params$seasons) {
  df_club <- read_csv(paste0("data/raw/high_school/club_", s, ".csv")) |>
    mutate(season = s) |>
    select(hs_name, team_id, contains("address"))  |>
    bind_rows(df_club)
}

df_club <- df_club |>
  sample_n(10)

df_addresses <- df_club |>
  filter(!is.na(address), !is.na(hs_name)) |>
  select(team_id, address, hs_name) |>
  mutate(address = paste0(hs_name, ", ", address)) |>
  select(team_id, address) |>
  distinct() |>
  geocode(
    address = address,
    method = "arcgis",
    full_results = TRUE
  )

df_addresses2 <- df_club |>
  filter(!is.na(hs_name)) |>
  select(team_id, hs_name) |>
  tidyr::separate(team_id, into = c("DROP", "state", "city"), remove = FALSE, sep = "/") |>
  mutate(address = paste0(hs_name, ", ", gsub("-", " ", tools::toTitleCase(city)), ", ", toupper(state))) |>
  distinct() |>
  geocode(
    address = address,
    method = "arcgis",
    full_results = TRUE
  )

df_addresses3 <- df_club |>
  filter(!is.na(address),
         str_count(address, ",") > 2 |
           !is.na(as.integer(substr(address, 1,1)))) |>
  select(team_id, address) |>
  distinct() |>
  geocode(
    address = address,
    method = "arcgis",
    full_results = TRUE
  )


df_addresses4 <- df_club |>
  filter(!is.na(address), !is.na(hs_name),
         grepl("/", hs_name)) |>
  mutate(hs_name = stringi::stri_reverse(hs_name)) |>
  tidyr::separate(hs_name, sep = "/", into = "hs_name", remove = FALSE) |>
  mutate(hs_name = stringi::stri_reverse(hs_name)) |>
  select(team_id, address, hs_name) |>
  mutate(address = paste0(hs_name, ", ", address)) |>
  select(team_id, address) |>
  distinct() |>
  geocode(
    address = address,
    method = "arcgis",
    full_results = TRUE
  )

df_addresses5 <- df_club |>
  filter(!is.na(address), !is.na(hs_name),
         grepl("\\[", hs_name)) |>
  mutate(hs_name = gsub("\\[[^][]*]", "", hs_name)) |>
  select(team_id, address, hs_name) |>
  mutate(address = paste0(hs_name, ", ", address)) |>
  select(team_id, address) |>
  distinct() |>
  geocode(
    address = address,
    method = "arcgis",
    full_results = TRUE
  )

df_addresses6 <- df_club |>
  tidyr::separate(team_id, into = c("DROP", "state", "city"), remove = FALSE, sep = "/") |>
  mutate(address = paste0(gsub("-", " ", tools::toTitleCase(city)), ", ", toupper(state))) |>
  distinct() |>
  geocode(
    address = address,
    method = "arcgis",
    full_results = TRUE
  )


df_addresses_final <- df_addresses |>
  mutate(style = 1) |>
  bind_rows(
    df_addresses2 |>
      mutate(style = 2)
  ) |>
  bind_rows(
    df_addresses3 |>
      mutate(style = 3)
  ) |>
  bind_rows(
    df_addresses4 |>
      mutate(style = -1)
  ) |>
  bind_rows(
    df_addresses5 |>
      mutate(style = -2)
  ) |>
  bind_rows(df_addresses6 |>
              mutate(style = -3)) |>
  tidyr::separate(team_id, into = c("DROP", "state"), remove = FALSE, sep = "/") |>
  mutate(state = toupper(state)) |>
  left_join(state_bounds, by = "state") |>
  filter((long >= lng_min & long <= lng_max & lat >= lat_min & lat <= lat_max)) |>
  group_by(team_id) |>
  arrange( desc(score), desc(style)) |>
  filter(row_number() == 1) |>
  ungroup() |>
  select(team_id, lat, long)

write.csv(df_addresses_final, "data/raw/high_school/long_lat_geo_code.csv", row.names = FALSE)