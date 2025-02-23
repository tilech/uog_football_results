library(dplyr)
library(tidyr)
library(readr)
library(stats4)
library(lubridate)
library(ggplot2)
library(ggrepel)

source("R/dixon_coles/dixon_coles_helper.R")

# Load data
data <- readRDS("data/germany.rds")

# Parameter settings
start_season <- 2006
prediction_season <- 2021
grid_search <- TRUE
grid_search_from <- 0.001
grid_search_to <- 0.001
grid_search_by <- 0.01
prediction_season_grid_search <- 2020
time_decay <- 0.002

if (grid_search) {
  cat("Start grid seach for season ", prediction_season_grid_search, "\n")
  # Placeholder to store RPS results
  rps_results <- data.frame(time_decay = numeric(), rps = numeric())
  
  # Grid search over specified range
  for (td in seq(grid_search_from, grid_search_to, by = grid_search_by)) {
    cat("Running for time_decay =", td, "\n")
    result <- run_model(data, start_season, prediction_season_grid_search, td)
    prediction <- result$prediction
    
    # Compute RPS for the current configuration
    rps <- compute_rps(prediction)
    cat("RPS for time_decay =", td, ":", rps, "\n")
    
    # Store the result
    rps_results <- rbind(rps_results, data.frame(time_decay = td, rps = rps))
  }
  
  # Find the best time_decay
  time_decay <- rps_results[which.min(rps_results$rps), ]$time_decay
  print("Finished grid search.")
  cat("Best paramter is ", time_decay, "\n")
}

result <- run_model(data, start_season, prediction_season, time_decay)

teams <- result$teams
prediction <- result$prediction
opt_params <- result$params

# Plots

params_plot <- plot_parameters(teams, opt_params)
grid_search_plot <- plot_grid_search(rps_results)
time_decay_plot <- plot_time_decay(data, start_season, prediction_season, time_decay)

# Compare league table
data_first_half <- data %>%
  filter(Season == prediction_season &
           month(Date) > 7)

# Get list of unique teams
teams <- unique(c(data_first_half$HomeTeam, data_first_half$AwayTeam))

# Get table after first half of season
initial_league_table <- initialize_league_table(data_first_half, teams)

# Get final predicted outcome of league table
final_predicted_league_table <- update_league_table(initial_league_table, prediction)

# Get final actual outcome of league table
data_full_season <- data %>%
  filter(Season == prediction_season)

final_actual_league_table <- initialize_league_table(data_full_season, teams)

