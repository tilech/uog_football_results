# Functions to run the extended Bayesian model for a specific season
run_model <- function(data, start_season, prediction_season) {
  # Prepare Data for Stan
  data <- data %>%
    mutate(XabiHome = ifelse(HomeTeam == "Leverkusen" & Date > as.Date("2022-10-05"), 1, 0)) %>%
    mutate(XabiAway = ifelse(AwayTeam == "Leverkusen" & Date > as.Date("2022-10-05"), 1, 0))
  
  data_train <- data %>%
    filter(Season >= start_season &
             (Season < prediction_season |
                (Season == prediction_season & month(Date) >= 7)))
  
  teams <- unique(c(data_train$HomeTeam, data_train$AwayTeam))
  team_idx <- setNames(seq_along(teams), teams)
  
  # Compile the Stan model
  stan_model <- stan_model("R/bayesian/bayesian.stan")
  
  # Stan model data
  stan_data <- list(
    n_games = nrow(data_train),
    n_teams = length(teams),
    home_team = team_idx[data_train$HomeTeam],
    away_team = team_idx[data_train$AwayTeam],
    home_goals = data_train$FTHG,
    away_goals = data_train$FTAG,
    xabi_alonso_home = data_train$XabiHome,
    xabi_alonso_away = data_train$XabiAway
  )
  
  init_fun <- function() {
    list(
      attack_free = rnorm(length(teams) - 1, 0, 0.1),
      defense_free = rnorm(length(teams) - 1, 0, 0.1),
      home_advantage = rnorm(length(teams), 0, 0.1),
      mu_attack = 0,
      mu_defense = 0,
      tau_attack = abs(rnorm(1, 1, 0.1)),  # Ensure positive values
      tau_defense = abs(rnorm(1, 1, 0.1)), # Ensure positive values
      rho = 0,  # Ensure within bounds
      xabi_effect = 0
    )
  }
  
  # Run inference with Stan
  fit <- sampling(stan_model, data = stan_data, init = init_fun, iter = 2000, chains = 4, seed = 42)
  
  # Extract results
  results <- rstan::extract(fit)
  
  # Extract posterior distributions
  post_attack <- rstan::extract(fit, pars = "attack")$attack
  post_defense <- rstan::extract(fit, pars = "defense")$defense
  post_home_advantage <- rstan::extract(fit, pars = "home_advantage")$home_advantage
  post_rho <- rstan::extract(fit, pars = "rho")$rho
  post_xabi_effect <- rstan::extract(fit, pars = "xabi_effect")$xabi_effect
  
  data_test <- data %>%
    filter(Season == prediction_season &
             month(Date) < 7)
  
  # Prepare to store outcome probabilities
  n_matches <- nrow(data_test)
  n_samples <- nrow(post_attack)
  home_win_probs <- numeric(n_matches)
  draw_probs <- numeric(n_matches)
  away_win_probs <- numeric(n_matches)
  predicted_home_goals <- numeric(n_matches)
  predicted_away_goals <- numeric(n_matches)
  
  # Simulate outcomes based on posterior samples
  for (i in 1:n_matches) {
    home_team <- team_idx[data_test$HomeTeam[i]]
    away_team <- team_idx[data_test$AwayTeam[i]]
    
    home_goals <- numeric(n_samples)
    away_goals <- numeric(n_samples)
    
    for (j in 1:n_samples) {
      home_goals[j] <- rpois(1, exp(post_attack[j, home_team] - 
                                      post_defense[j, away_team] + 
                                      post_home_advantage[j, home_team] +
                                      post_xabi_effect * data_test$XabiHome[i] -
                                      post_xabi_effect * data_test$XabiAway[i]))
      away_goals[j] <- rpois(1, exp(post_attack[j, away_team] - 
                                      post_defense[j, home_team] -
                                      post_xabi_effect * data_test$XabiHome[i] +
                                      post_xabi_effect * data_test$XabiAway[i]))
    }
    
    # Calculate probabilities
    home_win_probs[i] <- mean(home_goals > away_goals)
    draw_probs[i] <- mean(home_goals == away_goals)
    away_win_probs[i] <- mean(home_goals < away_goals)
    
    # Store predicted goals
    predicted_home_goals[i] <- mean(home_goals)
    predicted_away_goals[i] <- mean(away_goals)
  }
  
  # Create a data frame for the results
  predictions <- data.frame(
    probs.home_win = home_win_probs,
    probs.draw = draw_probs,
    probs.away_win = away_win_probs,
    predicted_home_goals = predicted_home_goals,
    predicted_away_goals = predicted_away_goals
  )
  
  data_test <- cbind(data_test, predictions)
  
  # Add actual outcomes as binary vectors
  data_test <- data_test %>%
    mutate(
      actual_home_win = ifelse(FTHG > FTAG, 1, 0),
      actual_draw = ifelse(FTHG == FTAG, 1, 0),
      actual_away_win = ifelse(FTHG < FTAG, 1, 0)
    )
  
  return(list("teams" = teams, "fit" = fit, "params" = results, "prediction" = data_test))
  
}


