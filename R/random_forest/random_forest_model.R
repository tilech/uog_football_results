library(dplyr)
library(lubridate)
library(randomForest)

source("R/random_forest/random_forest_helper.R")
source("R/utils.R")
source("R/constants.R")

# Load data
data <- readRDS("data/germany.rds")

# Parameter settings
start_season <- 2006
prediction_season <- 2024
grid_search <- FALSE
prediction_season_grid_search <- prediction_season - 1
grid_search_from <- 2006
grid_search_to <- prediction_season_grid_search

run_random_forest_model <- function(
    data,
    start_season, 
    prediction_season,
    grid_search,
    grid_search_from,
    grid_search_to,
    prediction_season_grid_search
) {

  if (grid_search) {
    cat("Start grid seach for season ", prediction_season_grid_search, "\n")
    # Placeholder to store RPS results
    rps_results <- data.frame(start_season = numeric(), rps = numeric())
    
    # Grid search over specified range
    for (season in seq(grid_search_from, grid_search_to, by = 1)) {
      cat("Running for start season =", season, "\n")
      result <- run_model_rf(data, start_season, prediction_season_grid_search)
      prediction <- result$prediction
      
      # Compute RPS for the current configuration
      rps <- compute_rps(prediction)
      rps <- rps$mean
      cat("RPS for start season =", season, ":", rps, "\n")
      
      # Store the result
      rps_results <- rbind(rps_results, data.frame(start_season = season, rps = rps))
    }
    
    # Find the best time_decay
    best_season <- rps_results[which.min(rps_results$rps), ]$start_season
    print("Finished grid search.")
    cat("Best paramter is ", best_season, "\n")
    start_season <- best_season + 1
    cat("Set new start season to ", start_season, "\n")
  }
  
  if (grid_search) {grid_search_plot <- plot_grid_search(rps_results)}
  
  result <- run_model_rf(data, start_season, prediction_season)
  
  prediction <- result$prediction
  model <- result$model
  
  # RPS
  rps <- compute_rps(prediction)
  
  # Compare league table
  data_first_half <- data %>%
    filter(Season == prediction_season &
             month(Date) > 7)
  
  # Get list of unique teams
  teams_current_season <- unique(c(data_first_half$HomeTeam, data_first_half$AwayTeam))
  
  # Get table after first half of season
  initial_league_table <- initialize_league_table(data_first_half, teams_current_season)
  
  # Get final predicted outcome of league table
  final_predicted_league_table <- update_league_table(initial_league_table, prediction)
  
  # Get final actual outcome of league table
  data_full_season <- data %>%
    filter(Season == prediction_season)
  
  final_actual_league_table <- initialize_league_table(data_full_season, teams_current_season)
  
  comparison_table <- left_join(final_predicted_league_table, final_actual_league_table, by = "team", suffix = c(".predicted", ".actual"))
  
  # Create a table to compare
  comparison_table <- comparison_table %>%
    select(team, points.predicted, rank.predicted, points.actual, rank.actual)
  
  bar_comp_plot <- plot_bar_comparison_league_table(comparison_table)
  scatter_comp_plot <- plot_scatter_comparison_league_table(comparison_table)
  mae_rank <- compute_mae_rank(comparison_table)
  mae_points <- compute_mae_points(comparison_table)
  
  return(list(
    "prediction" = prediction,
    "rps" = rps,
    "league_table" = final_predicted_league_table,
    "mae_rank" = mae_rank,
    "mae_points" = mae_points,
    "bar_comp_plot" = bar_comp_plot,
    "scatter_comp_plot" = scatter_comp_plot
  ))
}
  
  
  
