set.seed(123)  # Set a seed for reproducibility
library(caret)
library(randomForest)
#-------------------------------------------------------------------------------#
data  = read.csv("~/Desktop/ECNS460PROJECT/MasterData2.csv")
str(data)
summary(data)
#-------------------------------------------------------------------------------#
# data processing 
#-------------------------------------------------------------------------------#
data <- data %>%
  mutate(
    Avalanche.binary. = as.factor(Avalanche.binary.),
    date = as.Date(date, format = "%m/%d/%y")
  )

# Aggregating features for the last 7, 15, and 30 days
data <- data %>%
  mutate(
    tmean_7day_avg = rowMeans(select(., starts_with("tmean_lead0"):starts_with("tmean_lead6")), na.rm = TRUE),
    tmean_15day_avg = rowMeans(select(., starts_with("tmean_lead0"):starts_with("tmean_lead14")), na.rm = TRUE),
    tmean_30day_avg = rowMeans(select(., starts_with("tmean_lead0"):starts_with("tmean_lead29")), na.rm = TRUE),
    
    tmax_7day_max = apply(select(., starts_with("tmax_lead0"):starts_with("tmax_lead6")), 1, max, na.rm = TRUE),
    tmax_15day_max = apply(select(., starts_with("tmax_lead0"):starts_with("tmax_lead14")), 1, max, na.rm = TRUE),
    tmax_30day_max = apply(select(., starts_with("tmax_lead0"):starts_with("tmax_lead29")), 1, max, na.rm = TRUE),
    
    tmin_7day_min = apply(select(., starts_with("tmin_lead0"):starts_with("tmin_lead6")), 1, min, na.rm = TRUE),
    tmin_15day_min = apply(select(., starts_with("tmin_lead0"):starts_with("tmin_lead14")), 1, min, na.rm = TRUE),
    tmin_30day_min = apply(select(., starts_with("tmin_lead0"):starts_with("tmin_lead29")), 1, min, na.rm = TRUE),
    
    ppt_7day_sum = rowSums(select(., starts_with("ppt_lead0"):starts_with("ppt_lead6")), na.rm = TRUE),
    ppt_15day_sum = rowSums(select(., starts_with("ppt_lead0"):starts_with("ppt_lead14")), na.rm = TRUE),
    ppt_30day_sum = rowSums(select(., starts_with("ppt_lead0"):starts_with("ppt_lead29")), na.rm = TRUE)
  )
#-------------------------------------------------------------------------------#
# Splitting the data into training (80%) and testing (20%) sets
#-------------------------------------------------------------------------------#
train_index <- createDataPartition(data$Avalanche.binary., p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
#-------------------------------------------------------------------------------#
# Train the random forest model
#-------------------------------------------------------------------------------#
rf_model <- randomForest(Avalanche.binary. ~ ., data = train_data, ntree = 100, importance = TRUE)

# Display feature importance
varImpPlot(rf_model)

# Get predicted probabilities for the avalanche (class "1") on the test set
rf_prob <- predict(rf_model, newdata = test_data, type = "prob")

# Prob. of avalanche > 0.6, risk index = high
# 0.2 < prob of avalanche < 0.6, risk index = moderate
# Prob. of avalanche < 0.2, risk index = low
test_data <- test_data %>%
  mutate(Risk_Level = case_when(
    rf_prob[, "1"] > 0.6 ~ "High",
    rf_prob[, "1"] > 0.2 & rf_prob[, "1"] <= 0.6 ~ "Moderate",
    TRUE ~ "Low"
  ))

table(test_data$Risk_Level)

# Predict binary outcomes
rf_pred <- predict(rf_model, newdata = test_data)

# Confusion matrix for binary classification
confusionMatrix(rf_pred, test_data$Avalanche.binary.)

# Create a new data frame with just the coordinates and risk level
coordinates_risk <- test_data %>%
  select(latitude, longitude, date, Risk_Level)

# Display the first few rows to verify
head(coordinates_risk)

#-------------------------------------------------------------------------------#
# Calculate error rate and display misclassified observations
#-------------------------------------------------------------------------------#

# Add a column to indicate whether the prediction was accurate based on actual avalanche occurrences
test_data <- test_data %>%
  mutate(
    Prediction_Error = case_when(
      Avalanche.binary. == 1 & Risk_Level %in% c("Low", "Moderate") ~ "Error",
      Avalanche.binary. == 0 & Risk_Level == "High" ~ "Error",
      TRUE ~ "Correct"
    )
  )

# Calculate and print the error rate
error_rate <- mean(test_data$Prediction_Error == "Error")
cat("Error Rate:", error_rate * 100, "%\n")

# Separate correctly classified and incorrectly classified observations
correct_observations <- test_data %>%
  filter(Prediction_Error == "Correct") %>%
  select(date, latitude, longitude, Avalanche.binary., Risk_Level, Prediction_Error)

misclassified_observations <- test_data %>%
  filter(Prediction_Error == "Error") %>%
  select(date, latitude, longitude, Avalanche.binary., Risk_Level, Prediction_Error)

# Print summary and details for correctly classified observations
cat("Correctly Classified Observations:\n")
print(correct_observations)

# Print summary and details for incorrectly classified observations
cat("\nMisclassified Observations:\n")
print(misclassified_observations)


#-------------------------------------------------------------------------------#
# adjusted Error Rate
#-------------------------------------------------------------------------------#
test_data <- test_data %>%
  mutate(
    Prediction_Error = case_when(
      Avalanche.binary. == 1 & Risk_Level == "Low" ~ "Error",   # Avalanche occurred but risk predicted as Low
      Avalanche.binary. == 0 & Risk_Level == "High" ~ "Error",  # No avalanche but risk predicted as High
      TRUE ~ "Correct"  # All other cases, including Moderate, are considered correct
    )
  )

# Calculate and print the adjusted error rate
error_rate <- mean(test_data$Prediction_Error == "Error")
cat("Adjusted Error Rate:", error_rate * 100, "%\n")

# Create a new data frame with date, latitude, longitude, actual avalanche occurrence, predicted risk level, and prediction accuracy
adjusted_results_df <- test_data %>%
  select(date, latitude, longitude, Avalanche.binary., Risk_Level, Prediction_Error)

# Display the first few rows of the new data frame to verify
head(adjusted_results_df)
#-------------------------------------------------------------------------------#
