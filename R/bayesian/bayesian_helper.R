# Functions to run the extended Bayesian model for a specific season
run_model_bay <- function(data, start_season, prediction_season) {
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion",sample.kind="Rejection")
  # Prepare Data for Stan
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
    away_goals = data_train$FTAG
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
      rho = 0  # Ensure within bounds
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
      #home_goals[j] <- rpois(1, exp(post_attack[j, home_team] - 
      #                                post_defense[j, away_team] + 
      #                                post_home_advantage[j, home_team]))
      #away_goals[j] <- rpois(1, exp(post_attack[j, away_team] - 
      #                                post_defense[j, home_team]))
      lambda <- exp(post_attack[j, home_team] - post_defense[j, away_team] + post_home_advantage[j, home_team])
      mu <- exp(post_attack[j, away_team] - post_defense[j, home_team])
      rho_adj <- post_rho[j]  
      
      # Compute Poisson probabilities
      p_00 <- dpois(0, lambda) * dpois(0, mu)
      p_10 <- dpois(1, lambda) * dpois(0, mu)
      p_01 <- dpois(0, lambda) * dpois(1, mu)
      p_11 <- dpois(1, lambda) * dpois(1, mu)
      
      # Apply Dixon-Coles adjustments
      p_00 <- p_00 * (1 - lambda * mu * rho_adj)
      p_10 <- p_10 * (1 + mu * rho_adj)
      p_01 <- p_01 * (1 + lambda * rho_adj)
      p_11 <- p_11 * (1 - rho_adj)
      
      # Ensure probabilities are non-negative
      p_00 <- max(p_00, 0)
      p_10 <- max(p_10, 0)
      p_01 <- max(p_01, 0)
      p_11 <- max(p_11, 0)
      
      # Compute probability of falling into the low-score category
      p_low_score <- p_00 + p_10 + p_01 + p_11
      
      # Normalize low-score probabilities
      p_00 <- p_00 / p_low_score
      p_10 <- p_10 / p_low_score
      p_01 <- p_01 / p_low_score
      p_11 <- p_11 / p_low_score
      
      # Decide if we should sample from low-score outcomes
      u <- runif(1)
      if (u < p_low_score) {
        v <- runif(1)
        if (v < p_00) {
          home_goals[j] <- 0
          away_goals[j] <- 0
        } else if (v < p_00 + p_10) {
          home_goals[j] <- 1
          away_goals[j] <- 0
        } else if (v < p_00 + p_10 + p_01) {
          home_goals[j] <- 0
          away_goals[j] <- 1
        } else {
          home_goals[j] <- 1
          away_goals[j] <- 1
        }
      } else {
        # If outside low-score adjustment, sample from regular Poisson distributions
        home_goals[j] <- rpois(1, lambda)
        away_goals[j] <- rpois(1, mu)
      }
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
  
  data_test <- data_test %>%
    rowwise() %>%
    mutate(
      predicted_outcome = which.max(c(probs.home_win, probs.draw, probs.away_win)),
      predicted_home_win = as.integer(predicted_outcome == 1),
      predicted_draw = as.integer(predicted_outcome == 2),
      predicted_away_win = as.integer(predicted_outcome == 3)
    ) %>%
    ungroup()
  
  return(list("teams" = teams, "fit" = fit, "params" = results, "prediction" = data_test))
  
}


