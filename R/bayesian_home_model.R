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
stan_data_home <- list(
  N = nrow(data_train),
  N_teams = length(teams),
  home_team = data_train$home_team_id,
  away_team = data_train$away_team_id,
  home_goals = data_train$FTHG,
  away_goals = data_train$FTAG
)

# Compile the model
stan_model_home <- stan_model("R/bayesian_home.stan")

# Fit the model
fit_home <- sampling(
  stan_model_home,
  data = stan_data_home,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  seed = 42
)

# Extract results
results <- extract(fit)

# Extract posterior distributions
post_attack_home <- extract(fit_home, pars = "attack")$attack
post_defense_home <- extract(fit_home, pars = "defense")$defense
post_home_advantage_home <- extract(fit_home, pars = "home_advantage")$home_advantage

# Compute mean and credible intervals
home_adv_summary <- apply(post_home_advantage_home, 2, function(x) {
  c(mean = mean(x), 
    lower = quantile(x, 0.025), 
    upper = quantile(x, 0.975))
})

# Check row names
rownames(home_adv_summary) <- c("mean", "lower", "upper")

# Create a dataframe for visualization
home_adv_df <- data.frame(
  team = levels(factor(data_train$HomeTeam)),
  mean = home_adv_summary["mean", ],
  lower = home_adv_summary["lower", ],
  upper = home_adv_summary["upper", ]
)

# Plot results
ggplot(home_adv_df, aes(x = reorder(team, mean), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(title = "Team-Specific Home Advantage",
       x = "Team", y = "Home Advantage") +
  theme_minimal()

# Predict outcomes for the test data
predict_match_home <- function(home_team_id, away_team_id, attack, defense, home_advantage) {
  lambda_home <- exp(home_advantage[, home_team_id] + attack[, home_team_id] - defense[, away_team_id])
  lambda_away <- exp(attack[, away_team_id] - defense[, home_team_id])
  list(
    home_goals = rpois(length(lambda_home), lambda_home),
    away_goals = rpois(length(lambda_away), lambda_away)
  )
}

# Simulate predictions for each match in the test set
predictions_home <- lapply(1:nrow(data_test), function(i) {
  predict_match(
    data_test$home_team_id[i],
    data_test$away_team_id[i],
    post_attack_home,
    post_defense_home,
    post_home_advantage_home
  )
})

# Aggregate predictions
predicted_goals_home <- data.frame(
  home_goals_pred = sapply(predictions_home, function(x) mean(x$home_goals)),
  away_goals_pred = sapply(predictions_home, function(x) mean(x$away_goals))
)

# Combine with actual goals
test_results_home <- data_test %>%
  select(HomeTeam, AwayTeam, FTHG, FTAG) %>%
  cbind(predicted_goals_home)

# Calculate probabilities for each outcome
outcome_probs_home <- sapply(1:nrow(test_results_home), function(i) {
  home_wins <- mean(predictions_home[[i]]$home_goals > predictions_home[[i]]$away_goals)
  away_wins <- mean(predictions_home[[i]]$home_goals < predictions_home[[i]]$away_goals)
  draws <- mean(predictions_home[[i]]$home_goals == predictions_home[[i]]$away_goals)
  c(home_win = home_wins, draw = draws, away_win = away_wins)
})

# Determine actual outcomes
actual_outcomes_home <- ifelse(
  test_results_home$FTHG > test_results_home$FTAG, "home_win",
  ifelse(test_results_home$FTHG < test_results_home$FTAG, "away_win", "draw")
)

# Calculate Brier Score
brier_scores_home <- sapply(1:nrow(test_results_home), function(i) {
  actual <- c(home_win = 0, draw = 0, away_win = 0)
  actual[actual_outcomes_home[i]] <- 1
  sum((outcome_probs_home[, i] - actual)^2)
})

mean_brier_score_home <- mean(brier_scores_home)
cat("Mean Brier Score:", mean_brier_score_home, "\n")

# Calculate Cumulative Probabilities for Predicted Outcomes
cumulative_probs_pred <- t(apply(outcome_probs, 2, function(x) cumsum(c(0, x))))[,2:4]

# Calculate Cumulative Probabilities for Actual Outcomes
cumulative_probs_actual <- matrix(0, nrow = length(actual_outcomes), ncol = 3)
for (i in 1:length(actual_outcomes)) {
  if (actual_outcomes[i] == "home_win") {
    cumulative_probs_actual[i, ] <- c(1, 1, 1)  # Home Win
  } else if (actual_outcomes[i] == "draw") {
    cumulative_probs_actual[i, ] <- c(1, 1, 0)  # Draw
  } else if (actual_outcomes[i] == "away_win") {
    cumulative_probs_actual[i, ] <- c(1, 0, 0)  # Away Win
  }
}

# Calculate Ranked Probability Score (RPS)
rps_scores <- rowSums((cumulative_probs_pred - cumulative_probs_actual)^2)

# Calculate Mean RPS
mean_rps <- mean(rps_scores)
cat("Mean Ranked Probability Score:", mean_rps, "\n")
