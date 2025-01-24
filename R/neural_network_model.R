library(nnet)
library(dplyr)
library(tidyr)
library(caret)
library(keras)
library(Metrics)
library(lubridate)
library(zoo)
library(tibble)

data <- readRDS("data/germany.rds")

sum8 = function(x) { sum(x[1:8])}

# Function to calculate win rate and last 8 games win rate
calculate_win_rates <- function(data) {
  
  # Helper function to calculate sum for the last 8 games
  sum8 <- function(x) { sum(x[1:8], na.rm = TRUE) }
  
  # Prepare the data with match IDs and ensure chronological order
  data <- data %>%
    mutate(match_id = row_number()) %>%
    arrange(Date)
  
  # Calculate cumulative win, draw, and loss rates excluding the current game
  data <- data %>%
    group_by(HomeTeam) %>%
    mutate(cum_home_wins = lag(cumsum(ifelse(FTHG > FTAG, 1, 0)), default = 0),
           cum_home_draws = lag(cumsum(ifelse(FTHG == FTAG, 1, 0)), default = 0),
           cum_home_losses = lag(cumsum(ifelse(FTHG < FTAG, 1, 0)), default = 0),
           cum_home_games = lag(row_number(), default = 0),
           home_team_win_rate = ifelse(cum_home_games > 0, cum_home_wins / cum_home_games, NA),
           home_team_draw_rate = ifelse(cum_home_games > 0, cum_home_draws / cum_home_games, NA),
           home_team_loss_rate = ifelse(cum_home_games > 0, cum_home_losses / cum_home_games, NA)) %>%
    ungroup() %>%
    group_by(AwayTeam) %>%
    mutate(cum_away_wins = lag(cumsum(ifelse(FTAG > FTHG, 1, 0)), default = 0),
           cum_away_draws = lag(cumsum(ifelse(FTAG == FTHG, 1, 0)), default = 0),
           cum_away_losses = lag(cumsum(ifelse(FTAG < FTHG, 1, 0)), default = 0),
           cum_away_games = lag(row_number(), default = 0),
           away_team_win_rate = ifelse(cum_away_games > 0, cum_away_wins / cum_away_games, NA),
           away_team_draw_rate = ifelse(cum_away_games > 0, cum_away_draws / cum_away_games, NA),
           away_team_loss_rate = ifelse(cum_away_games > 0, cum_away_losses / cum_away_games, NA)) %>%
    ungroup()
  
  # Calculate the last 8 games win rates
  data <- data %>%
    group_by(HomeTeam) %>%
    mutate(last_8_home_wins = rollapply(ifelse(FTHG > FTAG, 1, 0), 9, sum8, fill = NA, align = "right"),
           last_8_home_draws = rollapply(ifelse(FTHG == FTAG, 1, 0), 9, sum8, fill = NA, align = "right"),
           last_8_home_losses = rollapply(ifelse(FTHG < FTAG, 1, 0), 9, sum8, fill = NA, align = "right"),
           last_8_home_games = rollapply(!is.na(FTHG), 9, sum8, fill = NA, align = "right"),
           home_team_win_rate_last_8 = last_8_home_wins / last_8_home_games,
           home_team_draw_rate_last_8 = last_8_home_draws / last_8_home_games,
           home_team_loss_rate_last_8 = last_8_home_losses / last_8_home_games) %>%
    ungroup() %>%
    group_by(AwayTeam) %>%
    mutate(last_8_away_wins = rollapply(ifelse(FTAG > FTHG, 1, 0), 9, sum8, fill = NA, align = "right"),
           last_8_away_draws = rollapply(ifelse(FTAG == FTHG, 1, 0), 9, sum8, fill = NA, align = "right"),
           last_8_away_losses = rollapply(ifelse(FTAG < FTHG, 1, 0), 9, sum8, fill = NA, align = "right"),
           last_8_away_games = rollapply(!is.na(FTAG), 9, sum8, fill = NA, align = "right"),
           away_team_win_rate_last_8 = last_8_away_wins / last_8_away_games,
           away_team_draw_rate_last_8 = last_8_away_draws / last_8_away_games,
           away_team_loss_rate_last_8 = last_8_away_losses / last_8_away_games) %>%
    ungroup()
  
  # Remove unnecessary columns for cleaner output
  data <- data %>%
    select(-starts_with("cum"), -ends_with("wins"), -ends_with("draws"), -ends_with("losses"), -ends_with("games"))
  
  return(data)
}

# Call the function
data <- calculate_win_rates(data)

# Preprocessing (example: creating a target variable)
data$HomeWin <- ifelse(data$FTHG > data$FTAG, 1, 0) # 1 if home team wins, 0 otherwise
data$Draw <- ifelse(data$FTHG == data$FTAG, 1, 0)  # 1 if draw, 0 otherwise
data$AwayWin <- ifelse(data$FTHG < data$FTAG, 1, 0) # 1 if away team wins, 0 otherwise

# Create one-hot encoded features
data <- data %>%
  mutate(HomeTeam = as.factor(HomeTeam), AwayTeam = as.factor(AwayTeam)) 
data <- data %>%
  mutate(HomeTeam = as.numeric(HomeTeam),
         AwayTeam = as.numeric(AwayTeam))

data <- data[!(is.na(data$home_team_win_rate_last_8)), ]
data <- data[!(is.na(data$away_team_win_rate_last_8)), ]

data_train <- data %>%
  filter(Date < as.Date("2017-07-01") |
           month(Date) >= 7)

data_test <- data %>%
  filter(Date >= as.Date("2017-07-01") &
           month(Date) < 7)

# Normalizing the data (important for neural networks)
train_features <- as.matrix(data_train[, c("home_team_win_rate_last_8",
                                           "home_team_draw_rate_last_8",
                                           "home_team_loss_rate_last_8",
                                           "home_team_win_rate",
                                           "home_team_draw_rate",
                                           "home_team_loss_rate",
                                           "away_team_win_rate_last_8",
                                           "away_team_draw_rate_last_8",
                                           "away_team_loss_rate_last_8",
                                           "away_team_win_rate",
                                           "away_team_draw_rate",
                                           "away_team_loss_rate",
                                           "Season"
                                           )])
train_labels <- as.matrix(data_train[, c("HomeWin", "Draw", "AwayWin")])

test_features <- as.matrix(data_test[, c("home_team_win_rate_last_8",
                                          "home_team_draw_rate_last_8",
                                          "home_team_loss_rate_last_8",
                                          "home_team_win_rate",
                                          "home_team_draw_rate",
                                          "home_team_loss_rate",
                                          "away_team_win_rate_last_8",
                                          "away_team_draw_rate_last_8",
                                          "away_team_loss_rate_last_8",
                                          "away_team_win_rate",
                                          "away_team_draw_rate",
                                          "away_team_loss_rate",
                                         "Season"
)])
test_labels <- as.matrix(data_test[, c("HomeWin", "Draw", "AwayWin")])

# Standardize training features
train_features_scaled <- scale(train_features)

# If you have validation or test features, you can scale them using the mean and SD from the training features
test_features_scaled <- scale(test_features, center = attr(train_features_scaled, "scaled:center"), 
                              scale = attr(train_features_scaled, "scaled:scale"))

model <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = 'relu', input_shape = dim(train_features_scaled)[2], kernel_regularizer = regularizer_l2(0.01)) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 3, activation = 'softmax')
optimizer <- optimizer_adam(lr = 0.0001)

# Compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer,
  metrics = c('accuracy')
)

# Fit the model
model %>% fit(train_features_scaled, train_labels, epochs = 100, batch_size = 10)

# Example predictions from your Keras model (probabilities)
predictions <- model %>% predict(test_features_scaled)

# Determine actual outcomes
actual_outcomes <- ifelse(
  data_test$FTHG > data_test$FTAG, "home_win",
  ifelse(data_test$FTHG < data_test$FTAG, "away_win", "draw")
)

# Calculate Brier Score
brier_scores <- sapply(1:nrow(data_test), function(i) {
  actual <- c(home_win = 0, draw = 0, away_win = 0)
  actual[actual_outcomes[i]] <- 1
  sum((predictions[i, ] - actual)^2)
})

mean_brier_score <- mean(brier_scores)
cat("Mean Brier Score:", mean_brier_score, "\n")

predictions_t <- predictions %>%
  as.data.frame() %>%
  rownames_to_column() %>% 
  pivot_longer(!rowname, names_to = "col1", values_to = "col2") %>% 
  pivot_wider(names_from = "rowname", values_from = "col2")

predictions_t <- predictions_t[,-1]

# Calculate Cumulative Probabilities for Predicted Outcomes
cumulative_probs_pred <- t(apply(predictions_t, 2, function(x) cumsum(c(0, x))))[,2:4]

# Calculate Cumulative Probabilities for Actual Outcomes
cumulative_probs_actual <- matrix(0, nrow = length(actual_outcomes), ncol = 3)
for (i in 1:length(actual_outcomes)) {
  if (actual_outcomes[i] == "home_win") {
    cumulative_probs_actual[i, ] <- c(1, 1, 1)  # Home Win
  } else if (actual_outcomes[i] == "draw") {
    cumulative_probs_actual[i, ] <- c(0, 1, 1)  # Draw
  } else if (actual_outcomes[i] == "away_win") {
    cumulative_probs_actual[i, ] <- c(0, 0, 1)  # Away Win
  }
}

# Calculate Ranked Probability Score (RPS)
rps_scores <- rowSums((cumulative_probs_pred - cumulative_probs_actual)^2)

# Calculate Mean RPS
mean_rps <- mean(rps_scores)
cat("Mean Ranked Probability Score:", mean_rps, "\n")

# Save data for the brier score
output_path = "RMD/neural_network/brier_score_nn.rds"
saveRDS(mean_brier_score, file = output_path)

# Save data for the RPS
output_path = "RMD/neural_network/rps_nn.rds"
saveRDS(mean_rps, file = output_path)

