# Functions to run the neural network model for a specific season
run_model_rf <- function(data, start_season, prediction_season) {
  process_team_data <- function(data, season) {
    data_home <- data %>%
      filter(Season == season & month(Date) >= 7) %>%
      group_by(HomeTeam) %>%
      summarize(home_wins = sum(FTHG > FTAG),
                home_draws = sum(FTHG == FTAG),
                home_losses = sum(FTHG < FTAG),
                home_games = n(),
                home_goals_scored = sum(FTHG),
                home_goals_conceded = sum(FTAG),
                home_shots = sum(HS),
                home_shots_against = sum(AS),
                home_fouls = sum(HF),
                home_corners = sum(HC),
                home_yellows = sum(HY),
                home_reds = sum(HR)
      )
    
    data_away <- data %>%
      filter(Season == season & month(Date) >= 7) %>%
      group_by(AwayTeam) %>%
      summarize(away_wins = sum(FTHG < FTAG),
                away_draws = sum(FTHG == FTAG),
                away_losses = sum(FTHG > FTAG),
                away_games = n(),
                away_goals_scored = sum(FTAG),
                away_goals_conceded = sum(FTHG),
                away_shots = sum(AS),
                away_shots_against = sum(HS),
                away_fouls = sum(AF),
                away_corners = sum(AC),
                away_yellows = sum(AY),
                away_reds = sum(AR))
    
    data_join_current <- data_home %>%
      left_join(data_away, by = join_by(HomeTeam == AwayTeam)) %>%
      rename(Team = HomeTeam) %>%
      mutate(
        total_games = home_games + away_games,
        av_wins = (home_wins + away_wins)/total_games,
        av_draws = (home_draws + away_draws)/total_games,
        av_losses = (home_losses + away_losses)/total_games,
        av_goals_scored = (home_goals_scored + away_goals_scored)/total_games,
        av_goals_conceded = (home_goals_conceded + away_goals_conceded)/total_games,
        av_shots = (home_shots + away_shots)/total_games,
        av_shots_against = (home_shots_against + away_shots_against)/total_games,
        av_fouls = (home_fouls + away_fouls)/total_games,
        av_corners = (home_corners + away_corners)/total_games,
        av_yellows = (home_yellows + away_yellows)/total_games,
        av_reds = (home_reds + away_reds)/total_games) %>%
      select(Team, starts_with("av")) %>%
      rename_with(~ paste(., "current", sep = "_"))
    
    data_train <- data %>%
      filter(Season == season & month(Date) < 7) %>%
      left_join(data_join_current, by = join_by(HomeTeam == Team_current)) %>%
      rename_at( .vars = vars(starts_with("av")),list(~paste0("home_", .))) %>%
      left_join(data_join_current, by = join_by(AwayTeam == Team_current)) %>%
      rename_at( .vars = vars(starts_with("av")),list(~paste0("away_", .))) %>%
      mutate(HomeWin = ifelse(FTHG > FTAG, 1, 0),
             Draw = ifelse(FTHG == FTAG, 1, 0),
             AwayWin = ifelse(FTHG < FTAG, 1, 0)) %>%
      select(starts_with("home_"), starts_with("away_"), HomeWin, Draw, AwayWin)
    
    return(data_train)
  }
  
  # Aggregate training data across the years
  training_data_list <- list()
  
  for (season in start_season:(prediction_season-1)) {
    season_data <- process_team_data(data, season)
    training_data_list[[as.character(season)]] <- season_data
  }
  
  # Combine all training data
  data_train <- bind_rows(training_data_list)
  
  data_train$Outcome <- factor(
    apply(data_train[, c("HomeWin", "Draw", "AwayWin")], 1, function(row) {
      if (row[1] == 1) return("HomeWin")
      else if (row[2] == 1) return("Draw")
      else return("AwayWin")
    })
  )
  
  data_train <- data_train %>%
    select(starts_with("home_"), starts_with("away_"), Outcome)
  
  rf_model <- randomForest(Outcome ~ ., data = data_train, ntree = 500, mtry = 3, importance = TRUE)
  
  data_test <- process_team_data(data, prediction_season)
  
  predictions <- predict(rf_model, data_test, type = "prob")
  
  predictions <- as.data.frame(predictions)
  colnames(predictions) <- c("probs.home_win", "probs.draw", "probs.away_win")
  
  data_test <- data %>%
    filter(Season == prediction_season & month(Date) < 7) %>%
    mutate(
      actual_home_win = ifelse(FTHG > FTAG, 1, 0),
      actual_draw = ifelse(FTHG == FTAG, 1, 0),
      actual_away_win = ifelse(FTHG < FTAG, 1, 0)
    )
  
  prediction <- cbind(data_test, predictions)
  
  return(list("model" = rf_model, "prediction" = prediction))
}
