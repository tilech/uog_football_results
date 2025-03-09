library(dplyr)
library(tidyr)

source("R/dixon_coles/dixon_coles_model.R")
source("R/bayesian/baio_blangiardo_model.R")
source("R/bayesian/bayesian_model.R")
source("R/neural_network/neural_network_model.R")

# Set the seed
set.seed(42)

# Load data
data <- readRDS("data/germany.rds")

# General parameter settings 
start_season <- 2006
prediction_season <- 2024
prediction_season_grid_search <- 2023
grid_search <- TRUE

# Dixon Coles parameter settings 
grid_search_from_dc <- 0.001
grid_search_to_dc <- 0.01
grid_search_by_dc <- 0.001
time_decay <- 0.002

# Bayesian parameter settings 
grid_search_from_bay <- 2006
grid_search_to_bay <- prediction_season_grid_search

# Neural Network parameter settings
grid_search_nn <- FALSE
model_comparison <- TRUE
input_shape <- 44


# Run Dixon Coles model
dixon_coles_model <- run_dixon_coles_model(
  data,
  start_season,
  prediction_season,
  grid_search,
  grid_search_from_dc,
  grid_search_to_dc,
  grid_search_by_dc,
  prediction_season_grid_search,
  time_decay
)

# Run Baio Blangiardo model
baio_blangiardo_model <- run_baio_blangiardo_model(
    data,
    start_season, 
    prediction_season,
    grid_search,
    grid_search_from_bay,
    grid_search_to_bay,
    prediction_season_grid_search
)

# Run Bayesian model
bayesian_model <- run_bayesian_model(
  data,
  start_season, 
  prediction_season,
  grid_search,
  grid_search_from_bay,
  grid_search_to_bay,
  prediction_season_grid_search
)

# Run Neural Network model
neural_network_model <- run_neural_network_model(
  data,
  start_season,
  prediction_season,
  grid_search_nn,
  model_comparison,
  input_shape,
  prediction_season_grid_search,
  grid_search_from_bay,
  grid_search_to_bay
)

# Save Plots
plot_names <- list(
  "dixon_coles_model" = list(
    "bar_comp_plot", 
    "scatter_comp_plot", 
    "params_plot",
    "grid_search_plot",
    "time_decay_plot"
  ),
  "baio_blangiardo_model" = list(
    "bar_comp_plot", 
    "scatter_comp_plot", 
    "params_plot",
    "grid_search_plot",
    "hta_plot",
    "ad_plot",
    "mcmc_trace_plot",
    "mcmc_acf_plot",
    "mcmc_dens_overlay_plot"
  ),
  "bayesian_model" = list(
    "bar_comp_plot", 
    "scatter_comp_plot", 
    "params_plot",
    "grid_search_plot",
    "hta_plot",
    "ad_plot",
    "mcmc_trace_plot",
    "mcmc_acf_plot",
    "mcmc_dens_overlay_plot"
  ),
  "neural_network_model" = list(
    "bar_comp_plot", 
    "scatter_comp_plot",
    "nn_model_selection_plot",
    "training_plot"
  )
)

# Loop over each model and plot
for (model_name in names(plot_names)) {
  # Access the model object using get()
  model_object <- get(model_name)
  
  for (plot_name in plot_names[[model_name]]) {
    plots_to_save <- model_object[[plot_name]]
    
    if (!"attack" %in% names(plots_to_save)) {
      # Single plot
      filename <- paste0("output/", model_name, "/", plot_name, ".png")
      ggsave(filename, plot = model_object[[plot_name]], device = "png", bg = "white")
    } else {
      # List of plots
      for (sub_plot_name in names(plots_to_save)) {
        sub_plot <- plots_to_save[[sub_plot_name]]
        filename <- paste0("output/", model_name, "/", plot_name, "_", sub_plot_name, ".png")
        ggsave(filename, plot = sub_plot, device = "png", bg = "white")
      }
    }
  }
}





