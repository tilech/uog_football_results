# Compute Brier scores for each match
# Function to compute RPS for a given model
compute_brier <- function(data) {
  data <- data %>%
    mutate(
      brier_home_win = (probs.home_win - actual_home_win)^2,
      brier_draw = (probs.draw - actual_draw)^2,
      brier_away_win = (probs.away_win - actual_away_win)^2,
      brier_score = brier_home_win + brier_draw + brier_away_win
    )
  
  return(mean(data$brier_score))
}

# Function to compute RPS for a given model
compute_rps <- function(data) {
  data <- data %>%
    mutate(
      cum_prob_1 = probs.home_win,
      cum_prob_2 = probs.home_win + probs.draw,
      cum_prob_3 = 1,
      
      cum_obs_1 = actual_home_win,
      cum_obs_2 = actual_home_win + actual_draw,
      cum_obs_3 = 1,
      
      rps = (cum_prob_1 - cum_obs_1)^2 +
        (cum_prob_2 - cum_obs_2)^2 +
        (cum_prob_3 - cum_obs_3)^2
    )
  
  return(list("mean" = mean(data$rps), "list" = data$rps))
}

# Function to compute accuracy
compute_accuracy <- function(data) {
  data <- data %>%
    mutate(
      correct_prediction = ifelse(actual_home_win == predicted_home_win &
                                    actual_draw == predicted_draw &
                                    actual_away_win == predicted_away_win, 1, 0)
    )
  return(mean(data$correct_prediction))
}

# Initialize league table
initialize_league_table <- function(data, teams) {
  # Calculate results for home teams
  home_results <- data %>%
    filter(HomeTeam %in% teams) %>%
    group_by(team = HomeTeam) %>%
    summarize(
      played_home = n(),
      won_home = sum(FTHG > FTAG),
      drawn_home = sum(FTHG == FTAG),
      lost_home = sum(FTHG < FTAG),
      goals_scored_home = sum(FTHG),
      goals_conceded_home = sum(FTAG)
    )
  
  # Calculate results for away teams
  away_results <- data %>%
    filter(AwayTeam %in% teams) %>%
    group_by(team = AwayTeam) %>%
    summarize(
      played_away = n(),
      won_away = sum(FTAG > FTHG),
      drawn_away = sum(FTAG == FTHG),
      lost_away = sum(FTAG < FTHG),
      goals_scored_away = sum(FTAG),
      goals_conceded_away = sum(FTHG)
    )
  
  # Combine home and away results
  league_table <- home_results %>%
    full_join(away_results, by = "team") %>%
    mutate(
      played = played_home + played_away,
      won = won_home + won_away,
      drawn = drawn_home + drawn_away,
      lost = lost_home + lost_away,
      goals_scored = goals_scored_home + goals_scored_away,
      goals_conceded = goals_conceded_home + goals_conceded_away,
      goal_difference = goals_scored - goals_conceded,
      points = 3 * won + drawn
    ) %>%
    select(team, played, won, drawn, lost, goals_scored, goals_conceded, goal_difference, points) %>%
    arrange(desc(points), desc(goal_difference), desc(goals_scored)) %>%
    mutate(rank = row_number())
  
  return(league_table)
}

# Update league table based on home/away match results without goals scored
update_league_table <- function(initial_league_table, predicted_data) {
  # Calculate points based on predictions for the second half of the season
  predicted_results <- predicted_data %>%
    mutate(
      home_points = ifelse(probs.home_win > probs.draw & probs.home_win > probs.away_win, 3, 
                           ifelse(probs.draw > probs.home_win & probs.draw > probs.away_win, 1, 0)),
      away_points = ifelse(probs.away_win > probs.home_win & probs.away_win > probs.draw, 3, 
                           ifelse(probs.draw > probs.home_win & probs.draw > probs.away_win, 1, 0))
    )
  
  # Summarize results for home teams
  home_updates <- predicted_results %>%
    group_by(HomeTeam) %>%
    summarize(
      played = n(),
      won = sum(home_points == 3),
      drawn = sum(home_points == 1),
      lost = sum(home_points == 0),
      points = sum(home_points)
    ) %>%
    rename(team = HomeTeam)
  
  # Summarize results for away teams
  away_updates <- predicted_results %>%
    group_by(AwayTeam) %>%
    summarize(
      played = n(),
      won = sum(away_points == 3),
      drawn = sum(away_points == 1),
      lost = sum(away_points == 0),
      points = sum(away_points)
    ) %>%
    rename(team = AwayTeam)
  
  # Combine home and away updates
  updates <- home_updates %>%
    full_join(away_updates, by = "team", suffix = c(".home", ".away")) %>%
    mutate(
      played = played.home + played.away,
      won = won.home + won.away,
      drawn = drawn.home + drawn.away,
      lost = lost.home + lost.away,
      points = points.home + points.away
    ) %>%
    select(team, played, won, drawn, lost, points)
  
  # Update the existing league table with new results
  final_league_table <- initial_league_table %>%
    full_join(updates, by = "team", suffix = c(".orig", ".update")) %>%
    mutate(
      played = played.orig + played.update,
      won = won.orig + won.update,
      drawn = drawn.orig + drawn.update,
      lost = lost.orig + lost.update,
      points = points.orig + points.update
    ) %>%
    select(team, played, won, drawn, lost, goals_scored, goals_conceded, goal_difference, points) %>%
    arrange(desc(points), desc(goal_difference), desc(goals_scored)) %>%
    mutate(rank = row_number())
  
  return(final_league_table)
}

# Plots

plot_grid_search <- function(data) {
  plot <- ggplot(data=data, aes(x=start_season, y=rps)) +
    geom_line()+
    geom_point() +
    scale_x_continuous(breaks=seq(min(data$start_season), max(data$start_season), 1)) +
    theme_minimal() +
    labs(title="Grid Search for determining the best start season", x="Start Season for Training", y = "RPS")
  
  return(plot)
}

# Update league table based on home/away match results without goals scored
plot_parameters <- function(teams, opt_params) {
  attack_strength <- apply(opt_params$attack, 2, mean)
  defense_strength <- apply(opt_params$defense, 2, mean)
  
  # Create a data frame for plotting
  data_dc_plot <- data.frame(
    Team = teams,
    Attack = c(attack_strength, rep(NA, length(defense_strength) - length(attack_strength))),
    Defense = c(rep(NA, length(attack_strength) - length(defense_strength)), defense_strength)
  )
  
  # Create the plot
  plot <- ggplot(data_dc_plot, aes(x = Defense, y = Attack, label = Team)) + 
    geom_point(aes(color = Team), size = 3) +  # Color the points
    geom_text_repel(vjust = -0.5, hjust = 0.5, max.overlaps = 10) +  # Keep text in default color
    geom_hline(yintercept = 0, linewidth = 1) +
    geom_vline(xintercept = 0, linewidth = 1) + 
    theme_minimal() +
    ggtitle("Attack and Defense Strength of Teams") +
    scale_color_manual(values = team_colors)  # Apply team colors to points only
  
  return(plot)
}

plot_bar_comparison_league_table <- function(comparison_table) {
  # Visualization: Bar Plot for Comparing Points
  plot <- ggplot(comparison_table, aes(x = reorder(team, points.actual))) +
    geom_bar(aes(y = points.predicted, fill = team), stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
    geom_bar(aes(y = points.actual, color = team), stat = "identity", position = position_dodge(width = 0.9), fill = NA, size = 1) +
    scale_fill_manual(values = team_colors) +
    scale_color_manual(values = team_colors, guide = "none") +
    labs(title = "Comparison of Predicted (filled) vs Actual (framed) Points by Team",
         x = "Team",
         y = "Points",
         fill = "Team") +
    theme_minimal() +
    coord_flip()
  
  return(plot)
}
  
plot_scatter_comparison_league_table <- function(comparison_table) {
  # Scatter Plot for Rank Differences
  ggplot(comparison_table, aes(x = rank.actual, y = rank.predicted, label = team)) +
    geom_point(aes(color = team), size = 3) +
    geom_text(vjust = 1.5, hjust = 0.5, size = 3, check_overlap = TRUE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    scale_color_manual(values = team_colors) +  # Use team colors for points
    labs(title = "Rank Comparison: Predicted vs Actual", 
         x = "Actual Rank", 
         y = "Predicted Rank",
         color = "Team") +  # Keep only the color legend
    theme_minimal()
}

compute_mae_rank <- function(comparison_table) {
  return(mean(abs(comparison_table$rank.actual - comparison_table$rank.predicted)))
}

compute_mae_points <- function(comparison_table) {
  return(mean(abs(comparison_table$points.actual - comparison_table$points.predicted)))
}


plot_home_team_advantage <- function(opt_params) {
  # Home advantage
  home_advantage_df <- as.data.frame(opt_params$home_advantage)
  
  plot <- ggplot(home_advantage_df, aes(x = opt_params$home_advantage)) +
    geom_density(fill = "steelblue", alpha = 0.7) +
    labs(title = "Posterior Distribution of Home Advantage", x = "Home Advantage", y = "Density") +
    theme_minimal()
  return(plot)
}

plot_home_team_advantage_per_team <- function(opt_params, teams) {
  
  # Extract and prepare attack and defense data
  hta_df <- as.data.frame(opt_params$home_advantage)
  
  colnames(hta_df) <- teams
  
  # Melt data to prepare for plotting
  hta_long <- melt(hta_df, variable.name = "Team", value.name = "HomeTeamAdvantage")
  
  # Ensure that 'Team' has correct labels
  hta_long$Team <- factor(hta_long$Team, labels = teams)
  
  # Plotting attack strengths as a boxplot
  plot_hta <- ggplot(hta_long, aes(x = Team, y = HomeTeamAdvantage)) +
    geom_boxplot(fill = "green", alpha = 0.7) +
    labs(title = "Boxplot of Home Team Advantage by Team", x = "Team", y = "Home Team Advantage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(plot_hta)
}

plot_rho <- function(opt_params) {
  # Home advantage
  rho_df <- as.data.frame(opt_params$rho)
  
  plot <- ggplot(rho_df, aes(x = opt_params$rho)) +
    geom_density(fill = "steelblue", alpha = 0.7) +
    labs(title = "Posterior Distribution of DC-adjustment Parameter", x = "Rho", y = "Density") +
    theme_minimal()
  return(plot)
}

plot_attack_defense_strength <- function(opt_params, teams) {
  
  # Extract and prepare attack and defense data
  attack_df <- as.data.frame(opt_params$attack)
  defense_df <- as.data.frame(opt_params$defense)
  
  colnames(attack_df) <- teams
  colnames(defense_df) <- teams
  
  # Melt data to prepare for plotting
  attack_long <- melt(attack_df, variable.name = "Team", value.name = "AttackStrength")
  defense_long <- melt(defense_df, variable.name = "Team", value.name = "DefenseStrength")
  
  # Ensure that 'Team' has correct labels
  attack_long$Team <- factor(attack_long$Team, labels = teams)
  defense_long$Team <- factor(defense_long$Team, labels = teams)
  
  # Plotting attack strengths as a boxplot
  plot_attack <- ggplot(attack_long, aes(x = Team, y = AttackStrength)) +
    geom_boxplot(fill = "red", alpha = 0.7) +
    labs(title = "Boxplot of Attack Strengths by Team", x = "Team", y = "Attack Strength") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plotting defense strengths as a boxplot
  plot_defense <- ggplot(defense_long, aes(x = Team, y = DefenseStrength)) +
    geom_boxplot(fill = "blue", alpha = 0.7) +
    labs(title = "Boxplot of Defense Strengths by Team", x = "Team", y = "Defense Strength") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list("attack" = plot_attack, "defense" = plot_defense))
}
  
