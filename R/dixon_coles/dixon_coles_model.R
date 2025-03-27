library(dplyr)
library(tidyr)
library(readr)
library(stats4)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(alabama)

source("R/dixon_coles/dixon_coles_helper.R")
source("R/utils.R")
source("R/constants.R")

# Load data
data <- readRDS("data/germany.rds")

# Parameter settings
start_season <- 2006
prediction_season <- 2020
grid_search <- TRUE
grid_search_from <- 0.001
grid_search_to <- 0.01
grid_search_by <- 0.001
prediction_season_grid_search <- 2019
time_decay <- 0.002

run_dixon_coles_model <- function(
    data,
    start_season, 
    prediction_season,
    grid_search,
    grid_search_from,
    grid_search_to,
    grid_search_by,
    prediction_season_grid_search,
    time_decay
    ) {

  if (grid_search) {
    cat("Start grid seach for season ", prediction_season_grid_search, "\n")
    # Placeholder to store RPS results
    rps_results <- data.frame(time_decay = numeric(), rps = numeric())
    
    # Grid search over specified range
    for (td in seq(grid_search_from, grid_search_to, by = grid_search_by)) {
      cat("Running for time_decay =", td, "\n")
      result <- run_model_dc(data, start_season, prediction_season_grid_search, td)
      prediction <- result$prediction
      
      # Compute RPS for the current configuration
      rps <- compute_rps(prediction)
      rps <- rps$mean
      cat("RPS for time_decay =", td, ":", rps, "\n")
      
      # Store the result
      rps_results <- rbind(rps_results, data.frame(time_decay = td, rps = rps))
    }
    
    # Find the best time_decay
    time_decay <- rps_results[which.min(rps_results$rps), ]$time_decay
    print("Finished grid search.")
    cat("Best paramter is ", time_decay, "\n")
  }
  
  result <- run_model_dc(data, start_season, prediction_season, time_decay)
  
  teams <- result$teams
  prediction <- result$prediction
  opt_params <- result$params
  
  # Plots
  params_plot <- plot_parameters_dc(teams, opt_params)
  grid_search_plot <- plot_grid_search_dc(rps_results)
  time_decay_plot <- plot_time_decay_dc(data, start_season, prediction_season, time_decay)
  
  # RPS
  rps <- compute_rps(prediction)
  accuracy <- compute_accuracy(prediction)
  
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
  
  bar_comp_plot <- plot_point_comparison_league_table(comparison_table)
  scatter_point_comp_plot <- plot_scatter_comparison_points(comparison_table)
  scatter_comp_plot <- plot_scatter_comparison_league_table(comparison_table)
  mae_rank <- compute_mae_rank(comparison_table)
  mae_points <- compute_mae_points(comparison_table)
  
  return(list(
    "prediction" = prediction,
    "teams" = teams,
    "opt_params" = opt_params,
    "comparison_table" = comparison_table,
    "rps" = rps,
    "accuracy" = accuracy,
    "league_table" = final_predicted_league_table,
    "mae_rank" = mae_rank,
    "mae_points" = mae_points,
    "bar_comp_plot" = bar_comp_plot,
    "scatter_point_comp_plot" = scatter_point_comp_plot,
    "scatter_comp_plot" = scatter_comp_plot,
    "params_plot" = params_plot,
    "grid_search_plot" = grid_search_plot,
    "time_decay_plot" = time_decay_plot
  ))
}
