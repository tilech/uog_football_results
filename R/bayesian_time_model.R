library(rstan)

data <- readRDS("data/germany.rds")

# Encode teams

teams <- unique(c(data$HomeTeam, data$AwayTeam))
team_ids <- setNames(1:length(teams), teams)
data <- data %>%
  mutate(home_team_id = team_ids[HomeTeam],
         away_team_id = team_ids[AwayTeam],
         year_id = Season - min(Season) + 1)

data_train <- data %>%
  filter(Date < as.Date("2017-07-01") |
           month(Date) >= 7)

data_test <- data %>%
  filter(Date >= as.Date("2017-07-01") &
           month(Date) < 7)

# Prepare updated Stan data
stan_data_time <- list(
  N = nrow(data_train),
  N_teams = length(teams),
  N_years = length(unique(data_train$year_id)),
  home_team = data_train$home_team_id,
  away_team = data_train$away_team_id,
  home_goals = data_train$FTHG,
  away_goals = data_train$FTAG,
  year = data_train$year_id
)

# Compile the updated model
stan_model_time <- stan_model("R/bayesian_time.stan")

# Fit the model
fit_time <- sampling(
  stan_model_time,
  data = stan_data_time,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  seed = 42
)

# Extract results
post_attack_time <- extract(fit_time, pars = "attack")$attack
post_defense_time <- extract(fit_time, pars = "defense")$defense
post_home_advantage_time <- extract(fit_time, pars = "home_advantage")

# Predict outcomes for 2019-2023
predict_time_match <- function(home_team_id, away_team_id, year_id, attack, defense, home_advantage) {
  lambda_home <- exp(home_advantage$home_advantage + attack[, home_team_id, year_id] - defense[, away_team_id, year_id])
  lambda_away <- exp(attack[, away_team_id, year_id] - defense[, home_team_id, year_id])
  list(
    home_goals = rpois(length(lambda_home), lambda_home),
    away_goals = rpois(length(lambda_away), lambda_away)
  )
}

# Predict each test match (2019–2023)
predictions_time <- lapply(1:nrow(data_test), function(i) {
  predict_time_match(
    data_test$home_team_id[i],
    data_test$away_team_id[i],
    data_test$year_id[i],
    post_attack_time,
    post_defense_time,
    post_home_advantage_time
  )
})

# Aggregate predictions
predicted_goals_time <- data.frame(
  home_goals_pred = sapply(predictions_time, function(x) mean(x$home_goals)),
  away_goals_pred = sapply(predictions_time, function(x) mean(x$away_goals))
)

# Combine with actual results
test_results_time <- data_test %>%
  select(HomeTeam, AwayTeam, FTHG, FTAG) %>%
  cbind(predicted_goals_time)

# Calculate MAE for home and away goals
mae_home_time <- mean(abs(test_results_time$FTHG - test_results_time$home_goals_pred))
mae_away_time <- mean(abs(test_results_time$FTAG - test_results_time$away_goals_pred))
mae_total_time <- (mae_home_time + mae_away_time) / 2

cat("Mean Absolute Error (Home):", mae_home_time, "\n")
cat("Mean Absolute Error (Away):", mae_away_time, "\n")
cat("Mean Absolute Error (Total):", mae_total_time, "\n")


# Calculate probabilities for each outcome
outcome_probs_time <- sapply(1:nrow(test_results_time), function(i) {
  home_wins <- mean(predictions_time[[i]]$home_goals > predictions_time[[i]]$away_goals)
  away_wins <- mean(predictions_time[[i]]$home_goals < predictions_time[[i]]$away_goals)
  draws <- mean(predictions_time[[i]]$home_goals == predictions_time[[i]]$away_goals)
  c(home_win = home_wins, draw = draws, away_win = away_wins)
})

# Determine actual outcomes
actual_outcomes_time <- ifelse(
  test_results_time$FTHG > test_results_time$FTAG, "home_win",
  ifelse(test_results_time$FTHG < test_results_time$FTAG, "away_win", "draw")
)

# Calculate Brier Score
brier_scores_time <- sapply(1:nrow(test_results_time), function(i) {
  actual <- c(home_win = 0, draw = 0, away_win = 0)
  actual[actual_outcomes_time[i]] <- 1
  mean((outcome_probs_time[, i] - actual)^2)
})

mean_brier_score_time <- mean(brier_scores_time)
cat("Mean Brier Score:", mean_brier_score_time, "\n")
