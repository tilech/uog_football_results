data {
  int<lower=1> n_games;            // Number of matches
  int<lower=1> n_teams;      // Number of teams
  int<lower=1> home_team[n_games]; // Home team ID
  int<lower=1> away_team[n_games]; // Away team ID
  int<lower=0> home_goals[n_games];// Home team goals
  int<lower=0> away_goals[n_games];// Away team goals
}

parameters {
  vector[n_teams] attack;             // Attack strength for each team
  vector[n_teams] defense;            // Defense strength for each team
  vector[n_teams] home_advantage;     // Team-specific home advantage
  real<lower=0> sigma_attack;         // Std dev for attack strength
  real<lower=0> sigma_defense;        // Std dev for defense strength
  real<lower=0> sigma_home_advantage; // Std dev for home advantage
}

model {
  // Priors
  sigma_attack ~ gamma(0.1, 0.1);
  sigma_defense ~ gamma(0.1, 0.1);
  sigma_home_advantage ~ gamma(0.1, 0.1);
  attack ~ normal(0, 1/sigma_attack);                     // Prior for attack strength
  defense ~ normal(0, 1/sigma_defense);                   // Prior for defense strength
  home_advantage ~ normal(0, 1/sigma_home_advantage);     // Prior for home advantage

  // Likelihood
  for (match in 1:n_games) {
    home_goals[match] ~ poisson(exp(home_advantage[home_team[match]]
                                     + attack[home_team[match]]
                                     - defense[away_team[match]]));
    away_goals[match] ~ poisson(exp(attack[away_team[match]]
                                     - defense[home_team[match]]));
  }
}
