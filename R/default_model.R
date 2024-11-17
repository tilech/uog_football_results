library(goalmodel)

data <- readRDS("data/germany.rds")

# Fit the default model, with the home team as team1.

gm_res <- goalmodel(goals1 = data$FTHG, goals2 = data$FTAG,
                    team1 = data$HomeTeam, team2=data$AwayTeam)

# Show the estimated attack and defense ratings and more.
summary(gm_res)
