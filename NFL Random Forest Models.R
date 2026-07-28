
## FINAL FORMAT - OVER/UNDER TRAINING DATA
full_data_ou <- full_data %>% 
  select(season, season_type, week, day_of_week, stadium, roof, surface, weather_category, temp, humidity, wind, location, div_game,
         spread_line, favored_team, total_line, home_off_rank_epa, home_off_rank_wpa, home_def_rank_epa, home_def_rank_wpa, 
         away_off_rank_epa, away_off_rank_wpa, away_def_rank_epa, away_def_rank_wpa, total_delta)

## FINAL FORMAT - SPREAD TRAINING DATA
full_data_spread <- full_data %>% 
  select(season, season_type, week, day_of_week, stadium, roof, surface, weather_category, temp, humidity, wind, location, div_game,
         spread_line, favored_team, total_line, home_off_rank_epa, home_off_rank_wpa, home_def_rank_epa, home_def_rank_wpa, 
         away_off_rank_epa, away_off_rank_wpa, away_def_rank_epa, away_def_rank_wpa, spread_delta)

## FINAL FORMAT - OVER/UNDER TEST DATA
schedule_ou <- schedule %>% 
  select(season, season_type, week, day_of_week, stadium, roof, surface, weather_category, temp, humidity, wind, location, div_game,
         spread_line, favored_team, total_line, home_off_rank_epa, home_off_rank_wpa, home_def_rank_epa, home_def_rank_wpa, 
         away_off_rank_epa, away_off_rank_wpa, away_def_rank_epa, away_def_rank_wpa, total_delta)

## FINAL FORMAT - SPREAD TEST DATA
schedule_spread <- schedule %>% 
  select(season, season_type, week, day_of_week, stadium, roof, surface, weather_category, temp, humidity, wind, location, div_game,
         spread_line, favored_team, total_line, home_off_rank_epa, home_off_rank_wpa, home_def_rank_epa, home_def_rank_wpa, 
         away_off_rank_epa, away_off_rank_wpa, away_def_rank_epa, away_def_rank_wpa, spread_delta)

## Random Forest Model - OU
set.seed(123)
RFM_OU = randomForest(total_delta ~ ., data = full_data_ou)

# Evaluating Model Accuracy
total_delta_pred = predict(RFM_OU, schedule_ou)
schedule_ou$total_delta_pred = round(total_delta_pred,3)
View(schedule_ou)
write.xlsx(schedule_ou, "C:/Users/jbjbj/Documents/KSU Masters/STAT 7940 - Applied Analysis Project/Test Data Schedule Random Forest OU.xlsx")

## Random Forest Model - Spread
set.seed(123)
RFM_Spread = randomForest(spread_delta ~ ., data=full_data_spread)

# Evaluating Model Accuracy
spread_delta_pred = predict(RFM_Spread, schedule_spread)
schedule_spread$spread_delta_pred = round(spread_delta_pred,3)
View(schedule_spread)
write.xlsx(schedule_spread, "C:/Users/jbjbj/Documents/KSU Masters/STAT 7940 - Applied Analysis Project/Test Data Schedule Random Forest Spread.xlsx")