data {
  int<lower=1> N;            // Number of matches
  int<lower=1> N_teams;      // Number of teams
  int<lower=1> home_team[N]; // Home team ID
  int<lower=1> away_team[N]; // Away team ID
  int<lower=0> home_goals[N];// Home team goals
  int<lower=0> away_goals[N];// Away team goals
}

parameters {
  vector[N_teams] attack;    // Attack strength for each team
  vector[N_teams] defense;   // Defense strength for each team
  real home_advantage;       // Home advantage
}

model {
  // Priors
  attack ~ normal(0, 1);
  defense ~ normal(0, 1);
  home_advantage ~ normal(0, 1);

  // Likelihood
  for (i in 1:N) {
    home_goals[i] ~ poisson(exp(home_advantage + attack[home_team[i]] - defense[away_team[i]]));
    away_goals[i] ~ poisson(exp(attack[away_team[i]] - defense[home_team[i]]));
  }
}
