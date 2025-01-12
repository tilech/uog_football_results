library(goalmodel)
library(dplyr)
library(lubridate)

data <- readRDS("data/germany.rds")

data_train <- data %>%
  filter(Date < as.Date("2017-07-01") |
           month(Date) >= 7)

data_test <- data %>%
  filter(Date >= as.Date("2017-07-01") &
           month(Date) < 7)

# Fit the default model, with the home team as team1.

my_weights <- weights_dc(data_train$Date, xi=0.0019)

gm_res <- goalmodel(goals1 = data_train$FTHG, goals2 = data_train$FTAG,
                    team1 = data_train$HomeTeam, team2=data_train$AwayTeam,
                    weights = my_weights)

# Show the estimated attack and defense ratings and more.
summary(gm_res)

predictions <- predict_result(gm_res, team1 = data_test$HomeTeam, team2 = data_test$AwayTeam, return_df = TRUE)

data_test3 <- data_test %>% 
  # Transform the result column (H, D, A) to reflect the column names of the prediction matrix.
  mutate(result2 = c('H' = 'p1', 'D'='pd', 'A' = 'p2')[FTR]) %>% 
  select(HomeTeam, AwayTeam, result2)

predictions$result <- apply(predictions[, c("p1", "pd", "p2")], 1, function(row) {
  names(row)[which.max(row)]
})

result <- cbind(data_test3, predictions)
result$outcome <- ifelse(result$result == result$result2, 1, 0)

sum(result$outcome/length(result$outcome))

predictions_scores_default <- score_predictions(predictions = predictions[,c('p1','pd','p2')], 
                                                observed = data_test3$result2, 
                                                score = c('log', 'brier', 'rps'))

mean_brier_score_dc <- mean(predictions_scores_default$brier)
cat("Mean Brier Score:", mean_brier_score_dc, "\n")



