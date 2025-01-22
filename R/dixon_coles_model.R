library(dplyr)
library(readr)
library(stats4)
library(lubridate)

data <- readRDS("data/germany.rds")

data_train <- data %>%
  filter(Date < as.Date("2017-07-01") |
           month(Date) >= 7)

data_test <- data %>%
  filter(Date >= as.Date("2017-07-01") &
           month(Date) < 7)

# Get list of unique teams
teams <- unique(c(data_train$HomeTeam, data_train$AwayTeam))

# Create a named vector of initial parameters for all teams
# Each team gets an attack and defense parameter
initial_params <- c(
  attack = setNames(rep(0, length(teams)), teams),
  defense = setNames(rep(0, length(teams)), teams),
  home_advantage = 0,  # Add home advantage parameter
  rho = 0  # Dixon-Coles adjustment parameter
)

# Define the Dixon-Coles likelihood function with team-specific parameters
dixon_coles_likelihood <- function(params, data, teams) {
  # Extract attack and defense parameters for teams
  attack <- params[paste0("attack.", teams)]
  defense <- params[paste0("defense.", teams)]
  home_advantage <- params["home_advantage"]  # Home advantage parameter
  rho <- params["rho"]
  
  likelihood <- 0
  for (i in 1:nrow(data)) {
    home_team <- data$HomeTeam[i]
    away_team <- data$AwayTeam[i]
    home_goals <- data$FTHG[i]
    away_goals <- data$FTAG[i]
    
    # Calculate lambda (expected goals for home team) and mu (away team)
    lambda <- exp(attack[paste0("attack.", home_team)] + home_advantage - defense[paste0("defense.", away_team)])
    mu <- exp(attack[paste0("attack.", away_team)] - defense[paste0("defense.", home_team)])
    
    # Poisson probabilities
    prob_home <- dpois(home_goals, lambda)
    prob_away <- dpois(away_goals, mu)
    
    # Dixon-Coles adjustment for low-score interactions
    if (home_goals == 0 & away_goals == 0) {
      adj_factor <- 1 - lambda*mu*rho
    } else if (home_goals == 1 & away_goals == 0) {
      adj_factor <- 1 + mu*rho
    } else if (home_goals == 0 & away_goals == 1) {
      adj_factor <- 1 + lambda*rho
    } else if (home_goals == 1 & away_goals == 1) {
      adj_factor <- 1 - rho
    } else {
      adj_factor <- 1
    }
    
    likelihood <- likelihood + log(prob_home * prob_away * adj_factor)
  }
  
  -likelihood  # Negative log-likelihood for minimization
}

# Output optimized parameters
opt <- optim(
  par = initial_params,
  fn = dixon_coles_likelihood,
  data = data_train,
  teams = teams,
  method = "L-BFGS-B",
  lower = c(rep(-Inf, length(initial_params) - 1), -0.2),  # Rho lower bound
  upper = c(rep(Inf, length(initial_params) - 1), 0.2),     # Rho upper bound
)

# Extract optimized parameters
opt_params <- opt$par
print(opt_params)

# Function to calculate the Dixon-Coles probabilities for match outcomes
calculate_probabilities <- function(lambda, mu, rho) {
  # Probabilities for specific scorelines
  p_home_win <- 0
  p_draw <- 0
  p_away_win <- 0
  max_goals <- 10  # Reasonable upper limit for Poisson summation
  
  for (x in 0:max_goals) {
    for (y in 0:max_goals) {
      # Dixon-Coles adjustment for low-score interactions
      adjustment <- ifelse(
        (x == 0 & y == 0), 1 - lambda * mu * rho,
        ifelse((x == 1 & y == 0), 1 + mu * rho,
               ifelse((x == 0 & y == 1), 1 + lambda * rho,
                      ifelse((x == 1 & y == 1), 1 - rho, 1)))
      )
      prob <- dpois(x, lambda) * dpois(y, mu) * adjustment
      
      if (x > y) {
        p_home_win <- p_home_win + prob
      } else if (x == y) {
        p_draw <- p_draw + prob
      } else {
        p_away_win <- p_away_win + prob
      }
    }
  }
  
  # Return probabilities
  c(home_win = p_home_win, draw = p_draw, away_win = p_away_win)
}

# Add predicted probabilities to the test data
data_test <- data_test %>%
  rowwise() %>%
  mutate(
    lambda = exp(opt_params[paste0("attack.", HomeTeam)] + opt_params["home_advantage"] - opt_params[paste0("defense.", AwayTeam)]),
    mu = exp(opt_params[paste0("attack.", AwayTeam)] - opt_params[paste0("defense.", HomeTeam)]),
    rho = opt_params["rho"],
    gamma = opt_params["home_advantage"],
    probs = list(calculate_probabilities(lambda, mu, rho))
  ) %>%
  unnest_wider(probs, names_sep = ".")

# Add actual outcomes as binary vectors
data_test <- data_test %>%
  mutate(
    actual_home_win = ifelse(FTHG > FTAG, 1, 0),
    actual_draw = ifelse(FTHG == FTAG, 1, 0),
    actual_away_win = ifelse(FTHG < FTAG, 1, 0)
  )

# Compute Brier scores for each match
data_test <- data_test %>%
  mutate(
    brier_home_win = (probs.home_win - actual_home_win)^2,
    brier_draw = (probs.draw - actual_draw)^2,
    brier_away_win = (probs.away_win - actual_away_win)^2,
    brier_score = brier_home_win + brier_draw + brier_away_win
  )

# Compute the overall Brier score for the test dataset
overall_brier_score <- mean(data_test$brier_score)

# Print the overall Brier score
print(overall_brier_score)

# Compute cumulative probabilities and observed outcomes
data_test <- data_test %>%
  mutate(
    # Cumulative probabilities
    cum_prob_1 = probs.home_win,
    cum_prob_2 = probs.home_win + probs.draw,
    cum_prob_3 = 1,
    
    # Cumulative observed outcomes
    cum_obs_1 = actual_home_win,
    cum_obs_2 = actual_home_win + actual_draw,
    cum_obs_3 = 1,
    
    # RPS for each match
    rps = (cum_prob_1 - cum_obs_1)^2 +
      (cum_prob_2 - cum_obs_2)^2 +
      (cum_prob_3 - cum_obs_3)^2
  )

# Compute the overall RPS for the test dataset
overall_rps <- mean(data_test$rps)

# Print the overall RPS
print(overall_rps)

# Plots

# Extract attack and defense strengths into a data frame
attack_strength <- opt_params[grepl("attack", names(opt_params))]
defense_strength <- opt_params[grepl("defense", names(opt_params))]

# Create a data frame for plotting
data_dc_plot <- data.frame(
  Team = teams,
  Attack = c(attack_strength, rep(NA, length(defense_strength) - length(attack_strength))),
  Defense = c(rep(NA, length(attack_strength) - length(defense_strength)), defense_strength)
)

# Create the plot
ggplot(data_dc_plot, aes(x = Defense, y = Attack, label = Team)) + 
  geom_point() + 
  geom_text(vjust = -0.5) +
  geom_hline(yintercept = 0, linewidth=1) +
  geom_vline(xintercept = 0, linewidth=1) + 
  theme_minimal() +
  ggtitle("Attack and Defense Strength of Teams")

# Save data for the attack/defense plot
output_path = "RMD/dixon_coles/team_strength.rds"
saveRDS(data_dc_plot, file = output_path)

# Save data for the home team advantage
output_path = "RMD/dixon_coles/home_team_factor.rds"
saveRDS(opt_params["home_advantage"], file = output_path)

# Save data for the low-score adjustment factor
output_path = "RMD/dixon_coles/low_score_factor.rds"
saveRDS(opt_params["rho"], file = output_path)

# Save data for the brier score
output_path = "RMD/dixon_coles/brier_score.rds"
saveRDS(overall_brier_score, file = output_path)

# Save data for the RPS
output_path = "RMD/dixon_coles/rps.rds"
saveRDS(overall_rps, file = output_path)
