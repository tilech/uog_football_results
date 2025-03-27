library(nnet)
library(dplyr)
library(tidyr)
library(caret)
library(keras)
library(Metrics)
library(lubridate)
library(zoo)
library(tibble)

source("R/neural_network/neural_network_helper.R")
source("R/utils.R")
source("R/constants.R")

tensorflow::set_random_seed(42)

# Load data
data <- readRDS("data/germany.rds")

# Parameter settings
start_season <- 2006
prediction_season <- 2024
grid_search <- FALSE
model_comparison <- TRUE
input_shape <- 44
prediction_season_grid_search <- prediction_season - 1
grid_search_from <- 2006
grid_search_to <- prediction_season_grid_search

run_neural_network_model <- function(
    data,
    start_season, 
    prediction_season,
    input_shape,
    grid_search_from,
    grid_search_to,
    prediction_season_grid_search
) {

  # Define the model
  model1 <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = 'relu', input_shape = input_shape) %>%
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dense(units = 3, activation = 'softmax')
  
  # Define the model
  model2 <- keras_model_sequential() %>%
    layer_dense(units = 128, activation = 'relu', input_shape = input_shape, kernel_regularizer = regularizer_l2(0.01)) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 64, activation = 'relu', kernel_regularizer = regularizer_l2(0.01)) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 3, activation = 'softmax')
  
  # Define the model
  model3 <- keras_model_sequential() %>%
    layer_dense(units = 128, activation = 'relu', input_shape = input_shape, kernel_regularizer = regularizer_l2(0.01)) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 64, activation = 'relu', kernel_regularizer = regularizer_l2(0.01)) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 3, activation = 'softmax')
  
  # Define the model
  model4 <- keras_model_sequential() %>%
    layer_dense(units = 128, activation = 'relu', input_shape = input_shape) %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 3, activation = 'softmax')
  
  # Define the model
  model5 <- keras_model_sequential() %>%
    layer_dense(units = 128, activation = 'relu', input_shape = input_shape) %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 3, activation = 'softmax')
  
  models <- list("model1"=model1, "model2"=model2, "model3"=model3, "model4"=model4, "model5"=model5)
  
  if (model_comparison) {
    cat("Start model comparison for season ", prediction_season_grid_search, "\n")
    # Placeholder to store RPS results
    rps_results <- data.frame(
      model = numeric(), 
      rps = numeric())
    
    # Grid search over specified range
    for (model_name in names(models)) {
      model <- models[[model_name]]
      cat("Running for model =", model_name, "\n")
      result <- run_model_nn(data, start_season, prediction_season_grid_search, model)
      prediction <- result$prediction
      
      # Compute RPS for the current configuration
      rps <- compute_rps(prediction)
      rps <- rps$mean
      cat("RPS for model =", model_name, ":", rps, "\n")
      
      # Store the result
      rps_results <- rbind(rps_results, data.frame(
        model = model_name, 
        rps = rps))
    }
    
    # Find the best time_decay
    best_model <- rps_results[which.min(rps_results$rps), ]$model
    print("Finished comparison.")
    cat("Best model is ", best_model, "\n")
    model <- models[[best_model]]
  }
  
  if (model_comparison) {nn_model_selection_plot <- plot_nn_model_selection(rps_results)}
  
  result <- run_model_nn(data, start_season, prediction_season, model)
  prediction <- result$prediction
  history <- result$history
  training_plot <- plot(history, method = "ggplot2", theme_bw = TRUE)
  
  
  if (grid_search) {
    cat("Start grid seach for season ", prediction_season_grid_search, "\n")
    # Placeholder to store RPS results
    rps_results <- data.frame(start_season = numeric(), rps = numeric())
    
    # Grid search over specified range
    for (season in seq(grid_search_from, grid_search_to, by = 1)) {
      cat("Running for start season =", season, "\n")
      result <- run_model_nn(data, start_season, prediction_season_grid_search, model)
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
    "comparison_table" = comparison_table,
    "rps" = rps,
    "history" = history,
    "accuracy" = accuracy,
    "league_table" = final_predicted_league_table,
    "mae_rank" = mae_rank,
    "mae_points" = mae_points,
    "bar_comp_plot" = bar_comp_plot,
    "scatter_point_comp_plot" = scatter_point_comp_plot,
    "scatter_comp_plot" = scatter_comp_plot,
    "nn_model_selection_plot" = nn_model_selection_plot,
    "training_plot" = training_plot
  ))
}
  
  
  
