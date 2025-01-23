library(rstan)
library(tidyverse)
library(ggplot2)

data <- readRDS("data/germany.rds")

# Encode teams
teams <- unique(c(data$HomeTeam, data$AwayTeam))
team_ids <- setNames(1:length(teams), teams)
data <- data %>%
  mutate(home_team_id = team_ids[HomeTeam],
         away_team_id = team_ids[AwayTeam])

data_train <- data %>%
  filter(Date < as.Date("2017-07-01") |
           month(Date) >= 7)

data_test <- data %>%
  filter(Date >= as.Date("2017-07-01") &
           month(Date) < 7)

# Prepare data for Stan
stan_data <- list(
  n_games = nrow(data_train),
  n_teams = length(teams),
  home_team = data_train$home_team_id,
  away_team = data_train$away_team_id,
  home_goals = data_train$FTHG,
  away_goals = data_train$FTAG
)

# Compile the model
stan_model <- stan_model("R/biao_blangiardo.stan")

# Fit the model
fit <- sampling(
  stan_model,
  data = stan_data,
  iter = 2000,
  chains = 4,
  seed = 42
)

# Extract results
results <- rstan::extract(fit)

# Extract posterior distributions
post_attack <- rstan::extract(fit, pars = "attack")$attack
post_defense <- rstan::extract(fit, pars = "defense")$defense
post_home_advantage <- rstan::extract(fit, pars = "home_advantage")$home_advantage

# Prepare to store outcome probabilities
n_matches <- nrow(data_test)
n_samples <- nrow(post_attack)
home_win_probs <- numeric(n_matches)
draw_probs <- numeric(n_matches)
away_win_probs <- numeric(n_matches)

# Simulate outcomes based on posterior samples
for (i in 1:n_matches) {
  home_team <- data_test$home_team[i]
  away_team <- data_test$away_team[i]
  
  home_goals <- numeric(n_samples)
  away_goals <- numeric(n_samples)
  
  for (j in 1:n_samples) {
    home_goals[j] <- rpois(1, exp(post_attack[j, home_team] + 
                                    post_defense[j, away_team] + 
                                    post_home_advantage[j]))
    away_goals[j] <- rpois(1, exp(post_attack[j, away_team] + 
                                    post_defense[j, home_team]))
  }
  
  # Calculate probabilities
  home_win_probs[i] <- mean(home_goals > away_goals)
  draw_probs[i] <- mean(home_goals == away_goals)
  away_win_probs[i] <- mean(home_goals < away_goals)
}

# Create a data frame for the results
predictions <- data.frame(
  home_win = home_win_probs,
  draw = draw_probs,
  away_win = away_win_probs
)

# Determine actual outcomes
actual_outcomes <- ifelse(
  data_test$FTHG > data_test$FTAG, "home_win",
  ifelse(data_test$FTHG < data_test$FTAG, "away_win", "draw")
)

# Calculate Brier Score
brier_scores <- sapply(1:nrow(data_test), function(i) {
  actual <- c(home_win = 0, draw = 0, away_win = 0)
  actual[actual_outcomes[i]] <- 1
  sum((predictions[i,] - actual)^2)
})

mean_brier_score <- mean(brier_scores)
cat("Mean Brier Score:", mean_brier_score, "\n")

# Calculate Cumulative Probabilities for Predicted Outcomes
cumulative_probs_pred <- t(apply(outcome_probs, 2, function(x) cumsum(c(0, x))))[,2:4]

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


