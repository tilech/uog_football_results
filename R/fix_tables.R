teams <- bayesian_model$teams

# Toy example 
df <- tibble(Parameter = names(bayesian_model$rhat_values), 
             Teams = c(teams[1:28], teams[1:28], teams, rep("", 5), teams, teams, ""),
             Value = bayesian_model$rhat_values)

# Plain latex output 
kable(df, "latex")

df <- tibble(Model = c("Dixon-Coles", "Baio-Blangiardo", "Extended Bayesian", "Neural Network", "Actual"), 
             Home_Wins = c(sum(dixon_coles_model$prediction$predicted_home_win),
                           sum(baio_blangiardo_model$prediction$predicted_home_win),
                           sum(bayesian_model$prediction$predicted_home_win),
                           sum(neural_network_model$prediction$predicted_home_win),
                           sum(neural_network_model$prediction$actual_home_win)),
             Draws = c(sum(dixon_coles_model$prediction$predicted_draw),
                       sum(baio_blangiardo_model$prediction$predicted_draw),
                       sum(bayesian_model$prediction$predicted_draw),
                       sum(neural_network_model$prediction$predicted_draw),
                       sum(neural_network_model$prediction$actual_draw)),
             Away_Wins = c(sum(dixon_coles_model$prediction$predicted_away_win),
                           sum(baio_blangiardo_model$prediction$predicted_away_win),
                           sum(bayesian_model$prediction$predicted_away_win),
                           sum(neural_network_model$prediction$predicted_away_win),
                           sum(neural_network_model$prediction$actual_away_win)))
             #Predicted_Home_Win_Mean = c(sum(dixon_coles_model$prediction$probs.home_win)/163,
             #                            sum(baio_blangiardo_model$prediction$probs.home_win)/163,
             #                            sum(bayesian_model$prediction$probs.home_win)/163,
             #                            sum(neural_network_model$prediction$probs.home_win)/163,
             #                            sum(neural_network_model$prediction$actual_home_win)/163),
             #Predicted_Draw_Mean = c(sum(dixon_coles_model$prediction$probs.draw)/163,
             #                            sum(baio_blangiardo_model$prediction$probs.draw)/163,
             #                            sum(bayesian_model$prediction$probs.draw)/163,
             #                            sum(neural_network_model$prediction$probs.draw)/163,
             #                            sum(neural_network_model$prediction$actual_draw)/163),
             #Predicted_Away_Win_Mean = c(sum(dixon_coles_model$prediction$probs.away_win)/163,
             #                        sum(baio_blangiardo_model$prediction$probs.away_win)/163,
             #                        sum(bayesian_model$prediction$probs.away_win)/163,
             #                        sum(neural_network_model$prediction$probs.away_win)/163,
             #                        sum(neural_network_model$prediction$actual_away_win)/163))

# Plain latex output 
kable(df, "latex")
