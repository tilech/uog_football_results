data {
  int<lower=1> N;            // Number of matches
  int<lower=1> N_teams;      // Number of teams
  int<lower=1> N_years;      // Number of years
  int<lower=1> home_team[N]; // Home team ID
  int<lower=1> away_team[N]; // Away team ID
  int<lower=0> home_goals[N];// Home team goals
  int<lower=0> away_goals[N];// Away team goals
  int<lower=1> year[N];      // Year of each match (1 = first year, ..., N_years = last year)
}

parameters {
  matrix[N_teams, N_years] attack;    // Attack strength by team and year
  matrix[N_teams, N_years] defense;   // Defense strength by team and year
  real home_advantage;               // Home advantage
}

model {
  // Priors for attack and defense strengths (random walk over years)
  for (t in 1:N_teams) {
    attack[t, 1] ~ normal(0, 1); // Initial year
    defense[t, 1] ~ normal(0, 1);
    for (y in 2:N_years) {
      attack[t, y] ~ normal(attack[t, y - 1], 0.5);
      defense[t, y] ~ normal(defense[t, y - 1], 0.5);
    }
  }
  
  home_advantage ~ normal(0, 1);

  // Likelihood
  for (i in 1:N) {
    real lambda_home = exp(home_advantage + attack[home_team[i], year[i]] - defense[away_team[i], year[i]]);
    real lambda_away = exp(attack[away_team[i], year[i]] - defense[home_team[i], year[i]]);
    home_goals[i] ~ poisson(lambda_home) T[0,];
    away_goals[i] ~ poisson(lambda_away) T[0,];
  }
}
