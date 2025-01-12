library(rstan)
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
  N = nrow(data_train),
  N_teams = length(teams),
  home_team = data_train$home_team_id,
  away_team = data_train$away_team_id,
  home_goals = data_train$FTHG,
  away_goals = data_train$FTAG
)

# Compile the model
stan_model <- stan_model("R/bayesian.stan")

# Fit the model
fit <- sampling(
  stan_model,
  data = stan_data,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  seed = 42
)

# Extract results
results <- rstan::extract(fit)

# Extract posterior distributions
post_attack <- rstan::extract(fit, pars = "attack")$attack
post_defense <- rstan::extract(fit, pars = "defense")$defense
post_home_advantage <- rstan::extract(fit, pars = "home_advantage")$home_advantage

# Summarize attack and defense strengths
attack_summary <- summary(fit, pars = "attack")$summary
defense_summary <- summary(fit, pars = "defense")$summary

# Visualize home advantage
home_advantage_posterior <- results$home_advantage
ggplot(data.frame(home_advantage_posterior), aes(x = home_advantage_posterior)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Posterior Distribution of Home Advantage", x = "Home Advantage", y = "Density")


# Predict outcomes for the test data
predict_match <- function(home_team_id, away_team_id, attack, defense, home_advantage) {
  lambda_home <- exp(home_advantage + attack[, home_team_id] - defense[, away_team_id])
  lambda_away <- exp(attack[, away_team_id] - defense[, home_team_id])
  list(
    home_goals = rpois(length(lambda_home), lambda_home),
    away_goals = rpois(length(lambda_away), lambda_away)
  )
}

# Simulate predictions for each match in the test set
predictions <- lapply(1:nrow(data_test), function(i) {
  predict_match(
    data_test$home_team_id[i],
    data_test$away_team_id[i],
    post_attack,
    post_defense,
    post_home_advantage
  )
})

# Aggregate predictions
predicted_goals <- data.frame(
  home_goals_pred = sapply(predictions, function(x) mean(x$home_goals)),
  away_goals_pred = sapply(predictions, function(x) mean(x$away_goals))
)

# Combine with actual goals
test_results <- data_test %>%
  select(HomeTeam, AwayTeam, FTHG, FTAG) %>%
  cbind(predicted_goals)

# Calculate probabilities for each outcome
outcome_probs <- sapply(1:nrow(test_results), function(i) {
  home_wins <- mean(predictions[[i]]$home_goals > predictions[[i]]$away_goals)
  away_wins <- mean(predictions[[i]]$home_goals < predictions[[i]]$away_goals)
  draws <- mean(predictions[[i]]$home_goals == predictions[[i]]$away_goals)
  c(home_win = home_wins, draw = draws, away_win = away_wins)
})

# Determine actual outcomes
actual_outcomes <- ifelse(
  test_results$FTHG > test_results$FTAG, "home_win",
  ifelse(test_results$FTHG < test_results$FTAG, "away_win", "draw")
)

# Calculate Brier Score
brier_scores <- sapply(1:nrow(test_results), function(i) {
  actual <- c(home_win = 0, draw = 0, away_win = 0)
  actual[actual_outcomes[i]] <- 1
  sum((outcome_probs[, i] - actual)^2)
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


