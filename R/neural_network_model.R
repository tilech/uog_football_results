library(nnet)
library(dplyr)
library(tidyr)
library(caret)
library(keras)
library(Metrics)
library(lubridate)
library(zoo)

data <- readRDS("data/germany.rds")

sum9 = function(x) { sum(x[1:9])}

# Function to calculate win rate and last 9 games win rate
calculate_win_rates <- function(data) {
  # Total wins per team
  total_wins <- data %>% 
    mutate(Winning_Team = case_when(
      FTHG > FTAG ~ HomeTeam,
      FTHG < FTAG ~ AwayTeam,
      TRUE ~ NA_character_)) %>%
    group_by(Winning_Team) %>%
    summarize(total_wins = n(), .groups = 'drop') %>%
    rename(Team = Winning_Team)
  
  # Total games per team
  total_games <- data %>%
    select(HomeTeam, AwayTeam) %>%
    gather(key = "side", value = "Team") %>%
    group_by(Team) %>%
    summarize(total_games = n(), .groups = 'drop')
  
  # Merge total wins and total games to calculate win rate
  team_stats <- total_wins %>%
    left_join(total_games, by = "Team") %>%
    mutate(win_rate = total_wins / total_games) %>%
    select(Team, win_rate)
  
  # Calculate the last 10 games win rate
  data <- data %>%
    mutate(match_id = row_number()) %>% # To maintain order
    
    # To ensure we get the last 10 games according to Date
    arrange(Date) %>%
    group_by(HomeTeam) %>%
    mutate(last_9_home_wins = rollapply(ifelse(FTHG > FTAG, 1, 0), 10, sum9, fill = NA, align = "right"),
           last_9_home_draws = rollapply(ifelse(FTHG == FTAG, 1, 0), 10, sum9, fill = NA, align = "right"),
           last_9_home_losses = rollapply(ifelse(FTHG < FTAG, 1, 0), 10, sum9, fill = NA, align = "right"),
           last_9_home_games = rollapply(!is.na(FTHG), 10, sum9, fill = NA, align = "right"),
           home_team_win_rate_last_9 = last_9_home_wins / last_9_home_games,
           home_team_draw_rate_last_9 = last_9_home_draws / last_9_home_games,
           home_team_loss_rate_last_9 = last_9_home_losses / last_9_home_games) %>%
    ungroup() %>%
    group_by(AwayTeam) %>%
    mutate(last_9_away_wins = rollapply(ifelse(FTAG > FTHG, 1, 0), 10, sum9, fill = NA, align = "right"),
           last_9_away_draws = rollapply(ifelse(FTAG == FTHG, 1, 0), 10, sum9, fill = NA, align = "right"),
           last_9_away_losses = rollapply(ifelse(FTAG < FTHG, 1, 0), 10, sum9, fill = NA, align = "right"),
           last_9_away_games = rollapply(!is.na(FTAG), 10, sum9, fill = NA, align = "right"),
           away_team_win_rate_last_9 = last_9_away_wins / last_9_away_games,
           away_team_draw_rate_last_9 = last_9_away_draws / last_9_away_games,
           away_team_loss_rate_last_9 = last_9_away_losses / last_9_away_games) %>%
    ungroup() %>%
    left_join(team_stats, by = c("HomeTeam" = "Team")) %>%
    rename(c('home_team_win_rate'='win_rate')) %>%
    left_join(team_stats, by = c("AwayTeam" = "Team")) %>%
    rename(c('away_team_win_rate'='win_rate')) %>%
    select(-ends_with("wins"), -ends_with("games"))
  
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

data <- data[!(is.na(data$home_team_win_rate_last_9)), ]
data <- data[!(is.na(data$away_team_win_rate_last_9)), ]

data_train <- data %>%
  filter(Date < as.Date("2017-07-01") |
           month(Date) >= 7)

data_test <- data %>%
  filter(Date >= as.Date("2017-07-01") &
           month(Date) < 7)

# Normalizing the data (important for neural networks)
train_features <- as.matrix(data_train[, c("home_team_win_rate_last_9",
                                           "home_team_draw_rate_last_9",
                                           "home_team_loss_rate_last_9",
                                           "home_team_win_rate",
                                           "away_team_win_rate_last_9",
                                           "away_team_draw_rate_last_9",
                                           "away_team_loss_rate_last_9",
                                           "away_team_win_rate")])
train_labels <- as.matrix(data_train[, c("HomeWin", "Draw", "AwayWin")])

test_features <- as.matrix(data_test[, c("home_team_win_rate_last_9",
                                         "home_team_draw_rate_last_9",
                                         "home_team_loss_rate_last_9",
                                         "home_team_win_rate",
                                         "away_team_win_rate_last_9",
                                         "away_team_draw_rate_last_9",
                                         "away_team_loss_rate_last_9",
                                         "away_team_win_rate")])
test_labels <- as.matrix(data_test[, c("HomeWin", "Draw", "AwayWin")])

model <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = 'relu', input_shape = dim(train_features)[2], kernel_regularizer = regularizer_l2(0.01)) %>%
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
model %>% fit(train_features, train_labels, epochs = 100, batch_size = 10)

# Example predictions from your Keras model (probabilities)
predictions <- model %>% predict(test_features)

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



