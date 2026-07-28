library(openxlsx)

## Random Forest Model - OU
set.seed(123)
RFM_OU = randomForest(total_delta ~ ., data = full_data_ou)

# Evaluating Model Accuracy
total_delta_pred = predict(RFM_OU, schedule)
schedule_ou$total_delta_pred = round(total_delta_pred,3)
View(schedule_ou)
write.xlsx(schedule_ou, "C:/Users/jbjbj/Documents/KSU Masters/STAT 7940 - Applied Analysis Project/Test Data Schedule Random Forest.xlsx")

## Random Forest Model - Spread
RFM_Spread = randomForest()

## XGBoost Model