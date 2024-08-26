# for each season
for (season in params$seasons) {
  season <- substr(season, 3, 4)
  
  df_club <- data.frame()
  
  
  # iterating through states
  for (state in params$states) {
    # games
    df_games <- data.frame()
    
    
    print(paste0("Season: 20", season, "; State: ", state))
    
    i <- 1
    
    urls_hs <- c()
    
    page_data <- -1
    
    # while there still is data
    while ((length(urls_hs) > 0 | i == 1) & !is.na(page_data)) {
      print(i)
      
      page_data <- retry_this(
        read_html(paste0(
          "https://www.maxpreps.com/",
          state,
          "/football/",
          season,
          "-",
          ifelse(as.integer(season) + 1 < 10,
                 paste0("0", as.integer(season) + 1),
                 as.integer(season) + 1
          ),
          "/rankings/",
          i
        )),
        max_tries = 20,
        sleep_time = 5,
        silent = TRUE
      )
      
      
      i <- i + 1
      
      
      if (!is.na(page_data)) {
        # grabbing URLs to HS
        urls_hs <- page_data |>
          html_nodes("table") |>
          html_nodes("a") |>
          html_attr("href")
        
        # selecting URLS for HS only
        urls_hs <- urls_hs[grepl("football", urls_hs)]
        
        if (length(urls_hs) > 0) {
          # iterating through HS
          for (u in urls_hs) {
            # Max Preps Team Id
            team_id <- gsub("football/", "", u)
            
            team_id <- gsub(
              paste0(
                season,
                "-",
                ifelse(as.integer(season) + 1 < 10,
                       paste0("0", as.integer(season) + 1),
                       as.integer(season) + 1
                ),
                "/schedule/"
              ),
              "",
              team_id
            )
            
            df_club_temp <- data.frame(
              team_id = team_id,
              state = state
            )
            
            club_page <- retry_this(read_html(paste0("https://www.maxpreps.com", team_id)),
                                    max_tries = 20,
                                    sleep_time = 3,
                                    silent = TRUE
            )
            
            
            club_page_data <- data.frame()
            
            if (!is.na(club_page)) {
              hs_name <- club_page |>
                html_nodes("h1") |>
                html_text() |>
                str_trim()
              
              text <- club_page |>
                html_nodes("dl") |>
                html_text() |>
                as.character()
              
              if (length(text) > 0) {
                text <- gsub("Mascot", ";mascot:", text)
                
                text <- gsub("Colors", ";colors:", text)
                
                text <- gsub("School Type", ";school_type:", text)
                
                text <- gsub("Athletic Director", ";athletic_director:", text)
                
                text <- str_split(string = text, pattern = ";")[[1]]
              } else {
                text <- ""
              }
              
              
              club_page_links <- club_page |>
                html_nodes("a") |>
                html_attr("href")
              
              address_link <- unique(club_page_links[grepl("google.com/maps", club_page_links)])
              
              if(length(address_link) == 1){
                
                address = gsub("%2C", ",", 
                               gsub("%20", " ",
                                    gsub("https://www.google.com/maps/search/?api=1&query=", "",
                                         address_link,
                                         fixed = TRUE),
                                    fixed = TRUE),
                               fixed = TRUE)
                
              } else{
                
                address <- NA
                
              }
              
              club_page_data <- data.frame(
                hs_name = hs_name,
                text = text,
                address = address
              ) |>
                filter(text != "") |>
                separate(text, sep = ":", into = c("desc", "value")) |>
                group_by(desc) |>
                filter(row_number() == 1) |>
                ungroup() |>
                spread(key = desc, value = value)
            }
            
            
            df_club_temp <- df_club_temp |>
              bind_cols(club_page_data)
            
            df_club <- bind_rows(
              df_club,
              df_club_temp
            )
            
            schedule_page <- retry_this(
              read_html(paste0(
                "https://www.maxpreps.com",
                team_id,
                "football/",
                season,
                "-",
                ifelse(as.integer(season) + 1 < 10,
                       paste0("0", as.integer(season) + 1),
                       as.integer(season) + 1
                ),
                "/schedule"
              )),
              sleep_time = 1,
              max_tries = 3,
              silent = TRUE
            )
            
            try(
              if (!is.na(schedule_page)) {
                # getting Games info
                schedule_page_links <- schedule_page |>
                  html_nodes("table") |>
                  html_nodes("tr") |>
                  html_nodes("a") |>
                  html_attr("href") # as above
                
                schedule_page_links_first <- schedule_page |>
                  html_nodes("table") |>
                  html_nodes("tr") |>
                  html_node("a") |>
                  html_attr("href") # just the first url in each tr
                
                
                if (length(schedule_page_links) > 0) {
                  df_schedule_page_links <- data.frame(
                    url = schedule_page_links, # full list
                    group = cumsum(schedule_page_links %in% schedule_page_links_first), # grouping indicator by tr
                    stringsAsFactors = FALSE
                  ) |>
                    distinct() |>
                    mutate(type = case_when(
                      grepl("/football/", url) ~ "team2",
                      grepl("/games/", url) ~ "game_url"
                    )) |>
                    spread(key = type, value = url)
                  
                  if ("team2" %in% colnames(df_schedule_page_links)) {
                    df_schedule_page_links <- df_schedule_page_links |>
                      separate(team2,
                               into = c("team2", "REMOVE", "REMOVE2"), extra = "merge",
                               sep = "https://www.maxpreps.com|football/"
                      ) |>
                      select(-c("REMOVE", "REMOVE2"))
                  } else {
                    df_schedule_page_links <- df_schedule_page_links |>
                      mutate(team2 = NA)
                  }
                  
                  
                  
                  if ("game_url" %in% colnames(df_schedule_page_links)) {
                    df_schedule_page_links <- df_schedule_page_links |>
                      separate(game_url,
                               into = c("REMOVE", "date",   "REMOVE2"), extra = "merge",
                               sep = "https://www.maxpreps.com/games/|/games/|/football-|/jv-football-|/freshmen-football-|/freshman-football-",
                               remove = FALSE
                      ) |>
                      select(-c("REMOVE", "REMOVE2"))
                  } else {
                    df_schedule_page_links <- df_schedule_page_links |>
                      mutate(date = NA)
                  }
                  
                  df_neutral <- data.frame()
                  
                  for (gu in (df_schedule_page_links |> select(game_url) |> drop_na() |> pull(game_url))) {
                    game_page <- retry_this(read_html(paste0("https://www.maxpreps.com",gu)),
                                            sleep_time = 1,
                                            max_tries = 3,
                                            silent = TRUE
                    )
                    
                    if (!is.na(game_page)) {
                      df_neutral <- data.frame(
                        is_neutral = game_page |>
                          html_nodes("p.contest-description") |>
                          html_text() |>
                          grepl(pattern = "neutral", ignore.case = TRUE),
                        game_url = gu
                      ) |>
                        bind_rows(df_neutral)
                    }
                  }
                  
                  schedule_table <- schedule_page |>
                    html_table()
                  
                  schedule_table <- schedule_table[[1]] |>
                    filter(!grepl("Opponent TBA", Opponent))
                  
                  if (nrow(schedule_table) == nrow(df_schedule_page_links)) {
                    schedule_table2 <- bind_cols(
                      schedule_table,
                      df_schedule_page_links |>
                        arrange(group) |>
                        select(-c("group"))
                    )
                  } else {
                    schedule_table2 <- schedule_table |>
                      mutate(
                        date = NA,
                        team2 = NA
                      )
                  }
                  
                  df_games_temp <- schedule_table2 |>
                    left_join(df_neutral, by = "game_url") |>
                    mutate(
                      location = case_when(
                        !is.na(is_neutral) & is_neutral ~ "Neutral",
                        substr(Opponent, 1, 1) == "@" ~ "Away",
                        substr(Opponent, 1, 2) == "vs" ~ "Home"
                      ),
                      game_type = case_when(
                        substr(Opponent, nchar(Opponent) - 2, nchar(Opponent)) == "***" ~ "Tournament",
                        substr(Opponent, nchar(Opponent) - 1, nchar(Opponent)) == "**" ~ "Playoffs",
                        substr(Opponent, nchar(Opponent), nchar(Opponent)) == "*" ~ "Conference",
                        TRUE ~ "Non-Conference"
                      ),
                      won = toupper(substr(Result, 1, 1)) == "W",
                      score = gsub("\\s*\\([^\\)]+\\)", "", Result),
                      score = gsub("[a-z A-Z]", "", score),
                      season = paste0("20", season),
                      team1 = team_id,
                      level = "varsity"
                    ) |>
                    separate(score, sep = "-", into = c("score1", "score2")) |>
                    mutate(
                      temp = score1,
                      score1 = ifelse(won, score1, score2),
                      score2 = ifelse(won, score2, temp),
                      score1 = ifelse(is.na(score2), NA_character_, score1),
                      score2 = ifelse(is.na(score1), NA_character_, score2),
                      team2_fill = case_when(
                        substr(Opponent, 1, 1) == "@" ~ substr(Opponent, 2, nchar(Opponent)),
                        substr(Opponent, 1, 2) == "vs" ~ substr(Opponent, 3, nchar(Opponent))
                      ),
                      team2_fill = case_when(
                        substr(team2_fill, nchar(team2_fill) - 2, nchar(team2_fill)) == "***" ~ substr(team2_fill, 1, nchar(team2_fill) - 3),
                        substr(team2_fill, nchar(team2_fill) - 1, nchar(team2_fill)) == "**" ~ substr(team2_fill, 1, nchar(team2_fill) - 2),
                        substr(team2_fill, nchar(team2_fill), nchar(team2_fill)) == "*" ~ substr(team2_fill, 1, nchar(team2_fill) - 1),
                        TRUE ~ team2_fill
                      )
                    ) |>
                    rename(date_fill = Date) |>
                    select(team1, team2, score1, score2, season, date, game_type, location, level, team2_fill, date_fill)
                  
                  
                  df_games <- df_games_temp |>
                    bind_rows(df_games)
                }
              },
              silent = T
            )
          }
        }
      }
    }
    
    df_club <- distinct(df_club)
    
    write.csv(df_club, paste0("data/raw/high_school/club_20", season, ".csv"), row.names = F)
    
    
    df_games <- distinct(df_games)
    
    
    if (!file.exists(paste0("data/raw/high_school/20", season))) {
      dir.create(paste0("data/raw/20", season))
    }
    
    write.csv(df_games, paste0("data/raw/high_school/20", season, "/games_", state, ".csv"), row.names = F)
  }
  
}
