
all_data <- rbind(full_data, schedule)

all_data_num <- all_data %>% 
  ## excluding stadium due to likely unimportance and difficulty to make numeric
  select(season, season_type, week, day_of_week, roof, surface, weather_category, temp, humidity, wind, location, div_game,
         spread_line, favored_team, total_line, home_off_rank_epa, home_off_rank_wpa, home_def_rank_epa, home_def_rank_wpa, 
         away_off_rank_epa, away_off_rank_wpa, away_def_rank_epa, away_def_rank_wpa, total_delta, spread_delta)
lab <- all_data_num[,25]
dummy <- dummyVars(" ~ .", data=all_data_num[,-25])
newdata <- data.frame(predict(dummy, newdata = all_data_num[,-25]))
all_data_num <- cbind(newdata, lab)
rm(dummy)
rm(lab)
rm(newdata)
rm(all_data)

## FINAL FORMAT - OVER/UNDER TRAINING DATA
full_data_num_ou <- all_data_num %>% 
  filter(season < 2023) %>% 
  select(-spread_delta)

## FINAL FORMAT - OVER/UNDER TEST DATA
schedule_num_ou <- all_data_num %>% 
  filter(season == 2023) %>% 
  select(-spread_delta)

## FINAL FORMAT - SPREAD TRAINING DATA
full_data_num_spread <- all_data_num %>% 
  filter(season < 2023) %>% 
  select(-total_delta)

## FINAL FORMAT - SPREAD TEST DATA
schedule_num_spread <- all_data_num %>% 
  filter(season == 2023) %>% 
  select(-total_delta)

## XG BOOST OU MODEL
set.seed(123)

grid_tune <- expand.grid(
  nrounds = 1500,
  max_depth = 2,
  eta = 0.1,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 0.5
)

train_control <- trainControl(method = "cv",
                              number=3,
                              verboseIter = TRUE,
                              allowParallel = TRUE
)

xgb_ou_tune <- train(x = full_data_num_ou[,-49],
                     y = full_data_num_ou[,49],
                     trControl = train_control,
                     tuneGrid = grid_tune,
                     method = "xgbTree",
                     verbose = TRUE
)

xgb_ou_tune

total_delta_pred = predict(xgb_ou_tune, schedule_num_ou)
schedule_ou$total_delta_pred = round(total_delta_pred,3)

write.xlsx(schedule_ou, "C:/Users/jbjbj/Documents/KSU Masters/STAT 7940 - Applied Analysis Project/Test Data Schedule XGBoost OU.xlsx")

## XG BOOST SPREAD MODEL
set.seed(123)

xgb_spread_tune <- train(x = full_data_num_spread[,-49],
                     y = full_data_num_spread[,49],
                     trControl = train_control,
                     tuneGrid = grid_tune,
                     method = "xgbTree",
                     verbose = TRUE
)

xgb_spread_tune

spread_delta_pred = predict(xgb_spread_tune, schedule_num_spread)
schedule_spread$spread_delta_pred = round(spread_delta_pred,3)

write.xlsx(schedule_spread, "C:/Users/jbjbj/Documents/KSU Masters/STAT 7940 - Applied Analysis Project/Test Data Schedule XGBoost Spread.xlsx")







