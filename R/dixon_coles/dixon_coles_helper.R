# Functions to run the Dixon-Coles model for a specific season
run_model_dc <- function(data, season_start, season_prediction, time_decay) {
  data_train <- data %>%
    filter(Season >= season_start &
             (Season < season_prediction |
                (Season == season_prediction & month(Date) >= 7)))
  
  data_test <- data %>%
    filter(Season == season_prediction &
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
  dixon_coles_likelihood <- function(params, data, teams, time_decay) {
    # Convert Date column to Date format if it's not already
    data$Date <- as.Date(data$Date)
    
    # Extract attack and defense parameters for teams
    attack <- params[paste0("attack.", teams)]
    defense <- params[paste0("defense.", teams)]
    home_advantage <- params["home_advantage"]
    rho <- params["rho"]
    
    # Calculate the difference in days from each match to the most recent one
    max_date <- max(data$Date)
    days_diff <- as.numeric(max_date - data$Date)
    
    # Create a time decay weight for each match based on days difference
    time_weights <- exp(-time_decay * days_diff)
    
    # Pre-compute expected goals (lambda and mu) for all matches
    lambda <- exp(attack[paste0("attack.", data$HomeTeam)] + home_advantage - defense[paste0("defense.", data$AwayTeam)])
    mu <- exp(attack[paste0("attack.", data$AwayTeam)] - defense[paste0("defense.", data$HomeTeam)])
    
    # Poisson probabilities for all matches
    prob_home <- dpois(data$FTHG, lambda)
    prob_away <- dpois(data$FTAG, mu)

    adj_factor <- ifelse(data$FTHG == 0 & data$FTAG == 0, 1 - lambda * mu * rho,
                         ifelse(data$FTHG == 1 & data$FTAG == 0, 1 + mu * rho,
                                ifelse(data$FTHG == 0 & data$FTAG == 1, 1 + lambda * rho,
                                       ifelse(data$FTHG == 1 & data$FTAG == 1, 1 - rho,
                                              1))))
    
    # Compute likelihoods, apply time weights, and sum them up
    likelihood <- sum(log(prob_home * prob_away * adj_factor) * time_weights)
    
    -likelihood  # Negative log-likelihood for minimization
  }
  
  # Output optimized parameters
  opt <- optim(
    par = initial_params,
    fn = dixon_coles_likelihood,
    data = data_train,
    teams = teams,
    time_decay = time_decay,
    method = "BFGS",
    lower = -Inf,
    upper = Inf,
    #lower = c(rep(-Inf, length(initial_params) - 1), -0.2),  # Rho lower bound
    #upper = c(rep(Inf, length(initial_params) - 1), 0.2),     # Rho upper bound
  )
  
  # Extract optimized parameters
  opt_params <- opt$par
  
  # Function to calculate the Dixon-Coles probabilities for match outcomes
  calculate_probabilities <- function(lambda, mu, rho, max_goals = 10) {
    # Initialize probabilities for the match outcomes
    p_home_win <- 0
    p_draw <- 0
    p_away_win <- 0
    
    # Initialize a matrix to store probabilities of specific scorelines
    score_probs <- matrix(0, nrow = max_goals + 1, ncol = max_goals + 1)
    
    # Calculate probabilities for each possible scoreline
    for (x in 0:max_goals) {
      for (y in 0:max_goals) {
        # Dixon-Coles adjustment for low-score interactions
        adjustment <- ifelse(
          (x == 0 & y == 0), 1 - lambda * mu * rho,
          ifelse((x == 1 & y == 0), 1 + mu * rho,
                 ifelse((x == 0 & y == 1), 1 + lambda * rho,
                        ifelse((x == 1 & y == 1), 1 - rho, 1)))
        )
        
        # Calculate the probability for the specific scoreline
        prob <- dpois(x, lambda) * dpois(y, mu) * adjustment
        score_probs[x + 1, y + 1] <- prob
        
        # Accumulate outcome probabilities
        if (x > y) {
          p_home_win <- p_home_win + prob
        } else if (x == y) {
          p_draw <- p_draw + prob
        } else {
          p_away_win <- p_away_win + prob
        }
      }
    }
    
    # Determine the most likely scoreline
    max_prob_index <- which(score_probs == max(score_probs), arr.ind = TRUE)
    predicted_home_goals <- max_prob_index[1] - 1
    predicted_away_goals <- max_prob_index[2] - 1
    
    # Return a list comprising overall outcome probabilities and the most likely scoreline
    return(list(
      home_win = p_home_win,
      draw = p_draw,
      away_win = p_away_win,
      predicted_home_goals = predicted_home_goals,
      predicted_away_goals = predicted_away_goals
    ))
  }
  
  # Modify the test data results to include score predictions
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
  
  data_test <- data_test %>%
    rowwise() %>%
    mutate(
      predicted_outcome = which.max(c(probs.home_win, probs.draw, probs.away_win)),
      predicted_home_win = as.integer(predicted_outcome == 1),
      predicted_draw = as.integer(predicted_outcome == 2),
      predicted_away_win = as.integer(predicted_outcome == 3)
    ) %>%
    ungroup()
  
  return(list("teams" = teams, "params" = opt_params, "prediction" = data_test))
  
}

# Update league table based on home/away match results without goals scored
plot_parameters_dc <- function(teams, opt_params) {
  attack_strength <- opt_params[grepl("attack", names(opt_params))]
  defense_strength <- opt_params[grepl("defense", names(opt_params))]
  
  # Create a data frame for plotting
  data_dc_plot <- data.frame(
    Team = teams,
    Attack = c(attack_strength, rep(NA, length(defense_strength) - length(attack_strength))),
    Defense = c(rep(NA, length(attack_strength) - length(defense_strength)), defense_strength)
  )
  
  # Create the plot
  plot <- ggplot(data_dc_plot, aes(x = Defense, y = Attack, label = Team)) + 
    geom_point(aes(color = Team), size = 3) +    # Only the points have color
    geom_text_repel(vjust = -0.5, hjust = 0.5, max.overlaps = 10) +
    geom_hline(yintercept = 0, linewidth = 1) +
    geom_vline(xintercept = 0, linewidth = 1) + 
    theme_minimal() +
    ggtitle("Attack and Defense Strength of Teams") +
    scale_color_manual(values = team_colors)  # Apply team colors to points only
  
  return(plot)

}

plot_grid_search_dc <- function(data) {
  plot <- ggplot(data=data, aes(x=time_decay, y=rps)) +
    geom_line()+
    geom_point() +
    scale_x_continuous(breaks=seq(0.001, 0.01, 0.001)) +
    theme_minimal() +
    labs(title="Grid Search for Time Decay Parameter",x="Time Decay", y = "RPS")
  
  return(plot)
}

plot_time_decay_dc <- function(data, season_start, season_prediction, time_decay) {
  data_train <- data %>%
    filter(Season >= season_start &
             (Season < season_prediction |
                (Season == season_prediction & month(Date) >= 7)))
  
  dates <- seq(min(data_train$Date), max(data_train$Date), 10)
  x <- as.numeric(max(data_train$Date) - dates)
  y <- exp(-time_decay*x)
  
  plot <- ggplot(data.frame(dates, y), aes(x = dates, y = y)) + 
    geom_line() + 
    theme_minimal() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title="Time Decay", x="", y = "Effect on Opimization")
}
