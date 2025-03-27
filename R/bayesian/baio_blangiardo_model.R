library(rstan)
library(dplyr)
library(tidyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(ggrepel)

source("R/bayesian/baio_blangiardo_helper.R")
source("R/utils.R")
source("R/constants.R")

# Load data
data <- readRDS("data/germany.rds")

# Parameter settings
start_season <- 2006
prediction_season <- 2024
grid_search <- TRUE
prediction_season_grid_search <- prediction_season - 1
grid_search_from <- 2006
grid_search_to <- prediction_season_grid_search

run_baio_blangiardo_model <- function(
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
      result <- run_model_bb(data, start_season, prediction_season_grid_search)
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
  
  # 3 years of training data performs best
  
  result <- run_model_bb(data, start_season, prediction_season)
  
  teams <- result$teams
  prediction <- result$prediction
  opt_params <- result$params
  fit <- result$fit
  
  rhat_values <- rhat(fit)
  
  # Plots
  
  params_plot <- plot_parameters(teams, opt_params)
  if (grid_search) {grid_search_plot <- plot_grid_search(rps_results)}
  hta_plot <- plot_home_team_advantage(opt_params)
  ad_plot <- plot_attack_defense_strength(opt_params, teams)
  bayesplot_theme_set(ggplot2::theme_minimal())
  mcmc_trace_plot <- mcmc_trace(fit, pars = c("home_advantage"))
  mcmc_acf_plot <- mcmc_acf(fit, pars = c("home_advantage"))
  mcmc_dens_overlay_plot <- mcmc_dens_overlay(fit, pars = c("home_advantage"))
  
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
  
  result_list <- list(
    "prediction" = prediction,
    "teams" = teams,
    "opt_params" = opt_params,
    "comparison_table" = comparison_table,
    "rhat_values" = rhat_values,
    "rps" = rps,
    "accuracy" = accuracy,
    "league_table" = final_predicted_league_table,
    "mae_rank" = mae_rank,
    "mae_points" = mae_points,
    "bar_comp_plot" = bar_comp_plot,
    "scatter_point_comp_plot" = scatter_point_comp_plot,
    "scatter_comp_plot" = scatter_comp_plot,
    "params_plot" = params_plot,
    "hta_plot" = hta_plot,
    "ad_plot" = ad_plot,
    "mcmc_trace_plot" = mcmc_trace_plot,
    "mcmc_acf_plot" = mcmc_acf_plot,
    "mcmc_dens_overlay_plot" = mcmc_dens_overlay_plot
  )
  
  # Conditionally add grid_search_plot if it exists
  if (exists("grid_search_plot")) {
    result_list$"grid_search_plot" <- grid_search_plot
  }
  
  # Return the result list
  return(result_list)
}
