pbp_schedule <- nflfastR::load_pbp(2023)

pbp_filter_schedule <- pbp_schedule %>% 
  mutate(special_teams_play_real = ifelse(play_type %in% c("kickoff", "punt", "extra_point", "field_goal"), 1, 0)) %>% 
  mutate(epa = round(epa, digits = 10)) %>% 
  mutate(wpa = round(wpa, digits = 10)) %>% 
  mutate(
    # Extract general weather information (everything before 'Temp')
    weather_gen = str_extract(weather, "^[^T]+"), 
    # Extract humidity (the number after 'Humidity:')
    humidity = str_extract(weather, "(?<=Humidity: )\\d+")
  ) %>% 
  mutate(weather_category = case_when(
    str_detect(roof, regex("dome|closed", ignore_case = TRUE)) ~ "Indoors",
    is.na(weather_gen) == TRUE ~ "Unknown",
    str_detect(weather, regex("rain|drizzle|showers", ignore_case = TRUE)) ~ "Rain/Drizzle",
    str_detect(weather, regex("snow|flurries|snow showers", ignore_case = TRUE)) ~ "Snow/Flurries",
    str_detect(weather, regex("clear|sunny|sunshine|blue skies|nice", ignore_case = TRUE)) ~ "Clear/Sunny",
    str_detect(weather, regex("cloud|overcast|cound|clould|clound|clouid|coudy", ignore_case = TRUE)) ~ "Cloudy/Overcast",
    str_detect(weather, regex("fair", ignore_case = TRUE)) ~ "Fair",
    str_detect(weather, regex("fog|haze|hazy|mist", ignore_case = TRUE)) ~ "Foggy/Hazey",
    str_detect(weather, regex("windy|breezy|gusty|high winds|gusts of 22", ignore_case = TRUE)) ~ "Windy/Breezy",
    str_detect(weather, regex("cold|chilly|freezing|frigid|cool|upper 40s to", ignore_case = TRUE)) ~ "Cold/Frigid",
    str_detect(weather, regex("warm|hot|mild|unseasonably warm", ignore_case = TRUE)) ~ "Warm/Hot",
    TRUE ~ "Other"
  )) %>% 
  mutate(
    # Assign 70 if indoors and temp is NA, otherwise keep existing temp
    temp = ifelse(roof %in% c("indoors", "dome", "closed"), 70, temp),
    # Replace remaining NAs with extracted temperature from string
    temp = ifelse(is.na(temp), as.numeric(gsub("°", "", str_extract(weather, "\\d+°"))), temp)) %>%
  mutate(humidity = coalesce(as.numeric(humidity),50)) %>% 
  mutate(wind = ifelse(roof %in% c("indoors", "dome", "closed"), 0, wind),
         wind = ifelse(is.na(wind), as.numeric(str_extract(weather, "\\d+(?= mph)")), wind)) %>% 
  mutate(start_time = str_sub(start_time, -8),  # Keep only the last 8 characters (HH:MM:SS)
         start_time = strptime(start_time, format = "%H:%M:%S", tz = "EST"),
         start_time = hms::as_hms(start_time)) %>%
  mutate(surface = if_else(surface == "", "grass", surface)) %>%
  mutate(surface = if_else(str_detect(surface, "grass"), "grass", "turf")) %>% 
  mutate(div_game = if_else(div_game == 1, "Division", "Non-Division")) %>% 
  select(
    ## game description stuff
    play_id, game_id, old_game_id, home_team, away_team, season, season_type, week, game_date, start_time, stadium, roof, surface, weather,
    weather_gen, weather_category, temp, humidity, wind, game_stadium, location, result, total, spread_line, total_line, div_game,
    ## play description
    posteam, defteam, desc, play_type, sp, play_type_nfl, field_goal_attempt, extra_point_attempt, two_point_attempt, rush_attempt, pass_attempt, punt_attempt, 
    special_teams_play_real, play_deleted, aborted_play,
    ## game statistics
    away_score, home_score, total, result,
    ## epa & wpa metrics
    epa, wpa,
    ## play statistics
    touchdown, td_team, penalty, penalty_yards, penalty_team, 
    ## offense
    two_point_conv_result, complete_pass, incomplete_pass, first_down, pass_touchdown, rush_touchdown,
    passing_yards, receiving_yards, rushing_yards, yards_gained, 
    ## defense
    sack, interception, safety, fumble, fumble_forced, fumble_not_forced, fumble_lost, lateral_recovery,
    ## special teams
    field_goal_result, punt_blocked, kick_distance, extra_point_result, return_touchdown, punt_inside_twenty, punt_in_endzone, punt_fair_catch,
    return_yards,
    ## drive statistics
    drive, fixed_drive, fixed_drive_result,  drive_first_downs, drive_inside20, drive_ended_with_score, drive_play_count, drive_time_of_possession,
    drive_yards_penalized, drive_end_transition,
    ## series statistics
    series, series_success, series_result
  ) %>% 
  filter(play_deleted == 0 & aborted_play == 0)

pbp_game_detail_schedule <- pbp_filter_schedule %>% 
  select(
    ## game description stuff
    game_id, old_game_id, home_team, away_team, season, season_type, week, game_date, start_time, stadium, roof, surface, weather,
    weather_gen, weather_category, temp, humidity, wind, game_stadium, location, spread_line, result, total_line, total, div_game,
    ## game statistics
    away_score, home_score, total, result) %>% 
  distinct()

pbp_play_stats_schedule <- pbp_filter_schedule %>% 
  select(
    ## game description stuff
    play_id, game_id, old_game_id, home_team, away_team,
    ## play description
    posteam, defteam, desc, play_type, sp, play_type_nfl, field_goal_attempt, extra_point_attempt, two_point_attempt, rush_attempt, pass_attempt, punt_attempt, 
    special_teams_play_real, play_deleted, aborted_play,
    ## epa & wpa metrics
    epa, wpa,
    ## play statistics
    touchdown, td_team, penalty, penalty_yards, penalty_team, 
    ## offense
    two_point_conv_result, complete_pass, incomplete_pass, first_down, pass_touchdown, rush_touchdown,
    passing_yards, receiving_yards, rushing_yards, yards_gained, 
    ## defense
    sack, interception, safety, fumble, fumble_forced, fumble_not_forced, fumble_lost, lateral_recovery,
    ## special teams
    field_goal_result, punt_blocked, kick_distance, extra_point_result, return_touchdown, punt_inside_twenty, punt_in_endzone, punt_fair_catch,
    return_yards,
    ## drive statistics
    drive, fixed_drive, fixed_drive_result,  drive_first_downs, drive_inside20, drive_ended_with_score, drive_play_count, drive_time_of_possession,
    drive_yards_penalized, drive_end_transition,
    ## series statistics
    series, series_success, series_result)

pbp_agg_home_schedule <- pbp_filter_schedule %>% 
  select(
    ## game description stuff
    game_id, season, week, home_team, season_type,
    ## play description
    posteam, defteam, special_teams_play_real,
    ## epa & wpa metrics
    epa, wpa
  ) %>% 
  filter(is.na(posteam) == FALSE & is.na(defteam) == FALSE) %>% 
  group_by(game_id, season, week, season_type, home_team) %>%
  summarise(
    
    # Count of offensive and defensive plays
    off_play_count = sum(posteam == home_team & special_teams_play_real == 0, na.rm = TRUE),  # Count offensive plays
    def_play_count = sum(defteam == home_team & special_teams_play_real == 0, na.rm = TRUE),   # Count defensive plays
    
    # Home team total offensive EPA/WPA
    off_epa = round(sum(epa[posteam == home_team & special_teams_play_real == 0], na.rm = TRUE), 10),
    off_wpa = round(sum(wpa[posteam == home_team & special_teams_play_real == 0], na.rm = TRUE), 10),
    
    # Home team total defensive EPA/WPA
    def_epa = round(sum(-epa[defteam == home_team & special_teams_play_real == 0], na.rm = TRUE), 10),
    def_wpa = round(sum(-wpa[defteam == home_team & special_teams_play_real == 0], na.rm = TRUE), 10),
    
    # Home team offensive EPA/WPA (plays where home team is the posteam)
    #    off_avg_epa = round(mean(epa[posteam == home_team & special_teams_play_real == 0], na.rm = TRUE), 10),
    #    off_avg_wpa = round(mean(wpa[posteam == home_team & special_teams_play_real == 0], na.rm = TRUE), 10),
    
    # Home team defensive EPA/WPA (plays where home team is the defteam)
    #    def_avg_epa = round(mean(-epa[defteam == home_team & special_teams_play_real == 0], na.rm = TRUE), 10),  # Negate EPA for defense
    #    def_avg_wpa = round(mean(-wpa[defteam == home_team & special_teams_play_real == 0], na.rm = TRUE), 10)  # Negate WPA for defense
  )

pbp_agg_away_schedule <- pbp_filter_schedule %>% 
  select(
    ## game description stuff
    game_id, season, week, away_team, season_type,
    ## play description
    posteam, defteam, special_teams_play_real,
    ## epa & wpa metrics
    epa, wpa
  ) %>% 
  filter(is.na(posteam) == FALSE & is.na(defteam) == FALSE) %>% 
  group_by(game_id, season, week, season_type, away_team) %>%
  summarise(
    
    # Count of offensive and defensive plays
    off_play_count = sum(posteam == away_team & special_teams_play_real == 0, na.rm = TRUE),  # Count offensive plays
    def_play_count = sum(defteam == away_team & special_teams_play_real == 0, na.rm = TRUE),   # Count defensive plays
    
    # Home team total offensive EPA/WPA
    off_epa = round(sum(epa[posteam == away_team & special_teams_play_real == 0], na.rm = TRUE), 10),
    off_wpa = round(sum(wpa[posteam == away_team & special_teams_play_real == 0], na.rm = TRUE), 10),
    
    # Home team total defensive EPA/WPA
    def_epa = round(sum(-epa[defteam == away_team & special_teams_play_real == 0], na.rm = TRUE), 10),
    def_wpa = round(sum(-wpa[defteam == away_team & special_teams_play_real == 0], na.rm = TRUE), 10),
    
    # Away team offensive EPA/WPA (plays where away team is the posteam)
    #    off_avg_epa = round(mean(epa[posteam == away_team & special_teams_play_real == 0], na.rm = TRUE), 10),
    #    off_avg_wpa = round(mean(wpa[posteam == away_team & special_teams_play_real == 0], na.rm = TRUE), 10),
    
    # Away team defensive EPA/WPA (plays where away team is the defteam)
    #    def_avg_epa = round(mean(-epa[defteam == away_team & special_teams_play_real == 0], na.rm = TRUE), 10),  # Negate EPA for defense
    #    def_avg_wpa = round(mean(-wpa[defteam == away_team & special_teams_play_real == 0], na.rm = TRUE), 10)   # Negate WPA for defense
  )

pbp_agg_all_schedule <- bind_rows(pbp_agg_home_schedule, pbp_agg_away_schedule) %>% 
  mutate(team = coalesce(home_team, away_team)) %>%  # Create a new column 'team'
  select(-away_team, -home_team) %>%  # Drop away_team column
  arrange(game_id) %>% 
  select(game_id, season_type, season, week, team, off_play_count, def_play_count, off_epa, def_epa, off_wpa, 
         def_wpa) %>% 
  group_by(season, team) %>% 
  mutate(
    total_off_play_count = lag(cumsum(off_play_count), default = 0),
    total_def_play_count = lag(cumsum(def_play_count), default = 0),
    total_off_epa = lag(cumsum(off_epa), default = 0),
    total_def_epa = lag(cumsum(def_epa), default = 0),
    total_off_wpa = lag(cumsum(off_wpa), default = 0),
    total_def_wpa = lag(cumsum(def_wpa), default = 0)
  ) %>% 
  ungroup() %>% 
  mutate(
    off_avg_epa = round(coalesce(total_off_epa/total_off_play_count,0),10),
    def_avg_epa = round(coalesce(total_def_epa/total_def_play_count,0),10),
    off_avg_wpa = round(coalesce(total_off_wpa/total_off_play_count,0),10),
    def_avg_wpa = round(coalesce(total_def_wpa/total_def_play_count,0),10)
  ) %>% 
  select(season, week, season_type, team, off_avg_epa, def_avg_epa, off_avg_wpa, def_avg_wpa)

year_week_team_schedule <- lapply(unique(pbp_agg_all_schedule$season), function(y) {
  expand.grid(
    season = y,
    week = unique(pbp_agg_all_schedule$week),
    team = unique(pbp_agg_all_schedule$team)
  )
}) %>% 
  bind_rows() %>% 
  filter((season <= 2020 & week <= 21) | (season >= 2021 & week <= 22))

full_epa_wpa_rank_schedule <- year_week_team_schedule %>% 
  left_join(pbp_agg_all_schedule, by = c("season", "week", "team")) %>% 
  group_by(team) %>%
  arrange(season, week) %>%  # Ensure data is ordered by year and week
  fill(off_avg_epa, def_avg_epa, off_avg_wpa, def_avg_wpa, .direction = "down") %>%
  mutate(
    season_type = ifelse(is.na(season_type), "BYE", season_type)
  ) %>%
  ungroup() %>% 
  group_by(season, week) %>%
  mutate(
    # Rankings for the 4 metrics, using descending order (higher values ranked higher)
    off_rank_epa = rank(-off_avg_epa, ties.method = "min"),  # rank 1 is highest
    def_rank_epa = rank(-def_avg_epa, ties.method = "min"),  # rank 1 is highest
    off_rank_wpa = rank(-off_avg_wpa, ties.method = "min"),  # rank 1 is highest
    def_rank_wpa = rank(-def_avg_wpa, ties.method = "min")   # rank 1 is highest
  ) %>%
  ungroup() %>% 
  filter(week != 1) %>% 
  group_by(season, week) %>% 
  mutate(
    off_rank = rank(((off_rank_epa+off_rank_wpa)/2), ties.method = "min"),
    def_rank = rank(((def_rank_epa+def_rank_wpa)/2), ties.method = "min")
  ) %>% 
  ungroup() %>% 
  select(-off_avg_epa, -def_avg_epa, -off_avg_wpa, -def_avg_wpa)

schedule <- pbp_game_detail_schedule %>% 
  left_join(full_epa_wpa_rank_schedule, by = c("season" = "season", "week" = "week", "home_team" = "team")) %>% 
  #filter(week != 1) %>% 
  select(-season_type.y) %>% 
  rename(
    home_off_rank_epa = off_rank_epa,
    home_def_rank_epa = def_rank_epa,
    home_off_rank_wpa = off_rank_wpa,
    home_def_rank_wpa = def_rank_wpa,
    home_off_rank = off_rank,
    home_def_rank = def_rank
  ) %>% 
  left_join(full_epa_wpa_rank_schedule, by = c("season" = "season", "week" = "week", "away_team" = "team")) %>% 
  filter(week != 1) %>% 
  select (-season_type) %>% 
  rename(
    season_type = season_type.x,
    away_off_rank_epa = off_rank_epa,
    away_def_rank_epa = def_rank_epa,
    away_off_rank_wpa = off_rank_wpa,
    away_def_rank_wpa = def_rank_wpa,
    away_off_rank = off_rank,
    away_def_rank = def_rank
  ) %>% 
  # select(-humidity,) %>% # known direct correlation with temperature, not necessary
  select(-weather, -weather_gen) %>% ## already broken down into individual parts, not needed anymore
  mutate(total_delta = total - total_line) %>% 
  mutate(spread_delta = result - spread_line) %>% 
  select(-game_id, -old_game_id, -home_team, -away_team, -home_score, -away_score,
         -total, -start_time, -game_stadium) %>% 
  mutate(favored_team = ifelse(spread_line<0, "Away Favored", "Home Favored")) %>% 
  mutate(day_of_week = weekdays(as.Date(game_date))) %>% 
  select(-game_date) %>% 
  mutate(temp = coalesce(temp, 70)) %>% 
  mutate(wind = coalesce(wind, 0)) 

## Clearing up environment by removing unnecessary mid-way data frames
rm(pbp_agg_home_schedule)
rm(pbp_agg_away_schedule)
rm(pbp_agg_all_schedule)
rm(full_epa_wpa_rank_schedule)
rm(year_week_team_schedule)
rm(pbp_filter_schedule)
rm(pbp_play_stats_schedule)
rm(pbp_game_detail_schedule)
rm(pbp_schedule)
