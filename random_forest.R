set.seed(123)  # Set a seed for reproducibility
library(caret)
library(randomForest)
library(dplyr)
#-------------------------------------------------------------------------------#
data  = read.csv("~/Desktop/ECNS460PROJECT/MasterData2.csv")

#-------------------------------------------------------------------------------#
# Data processing
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
set.seed(123)  # For reproducibility
train_index <- createDataPartition(data$Avalanche.binary., p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

#-------------------------------------------------------------------------------#
# Hyperparameter tuning with caret
#-------------------------------------------------------------------------------#
# Define a tuning grid
tune_grid <- expand.grid(
  mtry = c(2, 3, 4, 5)  # Number of variables to randomly sample
)

# Train control with 10-fold cross-validation
train_control <- trainControl(
  method = "cv",              # Cross-validation
  number = 10,                # 10 folds
  verboseIter = FALSE,        # Suppress verbose output
  sampling = "up"           # Apply downsampling
)

# Train the model using caret's train function
set.seed(123)  # For reproducibility
rf_model_tuned <- train(
  Avalanche.binary. ~ .,      # Model formula
  data = train_data,          # Training data
  method = "rf",              # Random forest
  trControl = train_control,  # Cross-validation settings
  tuneGrid = tune_grid,       # Hyperparameter grid
  importance = TRUE           # Calculate variable importance
)

# Display the best parameters
cat("Best parameters:\n")
print(rf_model_tuned$bestTune)

#-------------------------------------------------------------------------------#
# Evaluate the model on the test set
#-------------------------------------------------------------------------------#
rf_pred <- predict(rf_model_tuned, newdata = test_data)

# Confusion matrix for binary classification
conf_matrix <- confusionMatrix(rf_pred, test_data$Avalanche.binary.)
print(conf_matrix)


#-------------------------------------------------------------------------------#
# Calculate error rate and risk levels
#-------------------------------------------------------------------------------#
rf_prob <- predict(rf_model_tuned, newdata = test_data, type = "prob")

test_data <- test_data %>%
  mutate(Risk_Level = case_when(
    rf_prob[, "1"] > 0.6 ~ "High",
    rf_prob[, "1"] > 0.2 & rf_prob[, "1"] <= 0.6 ~ "Moderate",
    TRUE ~ "Low"
  ))

#-------------------------------------------------------------------------------#
# Save the tuned model
#-------------------------------------------------------------------------------#
saveRDS(rf_model_tuned, "rf_tuned_model.rds")
cat("Tuned model saved as 'rf_tuned_model.rds'\n") 

results_df <- data.frame(
  Date = test_data$date,
  Latitude = test_data$latitude,
  Longitude = test_data$longitude,
  Actual = test_data$Avalanche.binary.,
  Predicted = rf_pred,
  Probability_0 = rf_prob[, "0"],
  Probability_1 = rf_prob[, "1"]
)

# Step 3: Add Risk_Level based on predicted probabilities (optional)
results_df <- results_df %>%
  mutate(
    Risk_Level = case_when(
      Probability_1 > 0.6 ~ "High",
      Probability_1 > 0.2 & Probability_1 <= 0.6 ~ "Moderate",
      TRUE ~ "Low"
    ),
    Correct = ifelse(Actual == Predicted, "Yes", "No")
  )



accurately_predicted_df <- results_df %>%
  filter(Correct == "Yes")

# Create a data frame with all inaccurately predicted observations
inaccurately_predicted_df <- results_df %>%
  filter(Correct == "No")

# View the first few rows of each data frame
cat("Accurately Predicted Observations:\n")
head(accurately_predicted_df)

cat("\nInaccurately Predicted Observations:\n")
head(inaccurately_predicted_df)

#-------------------------------------------------------------------------------#
# Calculate weighted risk score
#-------------------------------------------------------------------------------#

calculate_weighted_score <- function(actual, predicted_risk) {
  # Initialize score vector
  score <- numeric(length(actual))
  
  # Loop through each prediction
  for (i in seq_along(actual)) {
    if (predicted_risk[i] == "Moderate") {
      # 'Moderate' predictions get 0.65 points regardless of actual outcome
      score[i] <- 0.65
    } else if (predicted_risk[i] == "High" && actual[i] == "1") {
      # Correct 'High' prediction when avalanche occurs
      score[i] <- 1
    } else if (predicted_risk[i] == "Low" && actual[i] == "0") {
      # Correct 'Low' prediction when no avalanche occurs
      score[i] <- 1
    } else {
      # Incorrect 'High' or 'Low' prediction
      score[i] <- 0
    }
  }
  
  # Return the score vector
  return(score)
}

# Apply the scoring func  tion to your data
results_df$Weighted_Score <- calculate_weighted_score(
  actual = results_df$Actual,
  predicted_risk = results_df$Risk_Level
)

# Calculate total weighted score
total_weighted_score <- sum(results_df$Weighted_Score)

# Calculate maximum possible score
max_score <- nrow(results_df)  # Each observation could get a maximum of 1 point

# Calculate weighted accuracy
weighted_accuracy <- total_weighted_score / max_score

# Display the weighted accuracy
cat("Weighted Accuracy:", round(weighted_accuracy * 100, 2), "%\n")
