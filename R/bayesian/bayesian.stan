data {
  int<lower=1> n_games;            // Number of matches
  int<lower=1> n_teams;      // Number of teams
  int<lower=1> home_team[n_games]; // Home team ID
  int<lower=1> away_team[n_games]; // Away team ID
  int<lower=0> home_goals[n_games];// Home team goals
  int<lower=0> away_goals[n_games];// Away team goals
}

parameters {
  vector[n_teams-1] attack_free;    // Attack strength for each team
  vector[n_teams-1] defense_free;   // Defense strength for each team
  vector[n_teams-1] home_advantage_free;   // Home advantage for each team
  real mu_attack;
  real mu_defense;
  real<lower=0> tau_attack;
  real<lower=0> tau_defense;
  real<lower=-1,upper=1> rho; // Dixon-Coles adjustment parameter
}

transformed parameters {
  vector[n_teams] attack;
  vector[n_teams] defense;
  vector[n_teams] home_advantage;

  // need to make sum(att)=sum(def)=0
  for (k in 1:(n_teams-1)) {
    attack[k] = attack_free[k];
    defense[k] = defense_free[k];
    home_advantage[k] = home_advantage_free[k];
  }
  attack[n_teams] = -sum(attack_free);
  defense[n_teams] = -sum(defense_free);
  home_advantage[n_teams] = -sum(home_advantage_free);

}

model {
  // Priors
  mu_attack ~ normal(0, 100);    // Prior for mu_attack
  mu_defense ~ normal(0, 100);   // Prior for mu_defense
  tau_attack ~ gamma(0.1, 0.1);   // Prior for tau_attack
  tau_defense ~ gamma(0.1, 0.1);  // Prior for tau_defense
  attack ~ normal(mu_attack, 1/tau_attack);  // Attack strength modeling
  defense ~ normal(mu_defense, 1/tau_defense); // Defense strength modeling
  home_advantage ~ normal(0, 100);  // Home advantage prior
  rho ~ normal(0, 100); // Prior for Dixon-Coles adjustment, typically small

  for (i in 1:n_games) {
    real lambda = exp(attack[home_team[i]] - defense[away_team[i]] + home_advantage[home_team[i]]);
    real mu = exp(attack[away_team[i]] - defense[home_team[i]]);
                      
    if (home_goals[i] == 0 && away_goals[i] == 0)
      target += log(1 - lambda * mu * rho); // adjustment for 0-0
    if (home_goals[i] == 1 && away_goals[i] == 0)
      target += log(1 + mu * rho); // adjustment for 1-0
    if (home_goals[i] == 0 && away_goals[i] == 1)
      target += log(1 + lambda * rho); // adjustment for 0-1
    if (home_goals[i] == 1 && away_goals[i] == 1)
      target += log(1 - rho); // adjustment for 1-1

    home_goals[i] ~ poisson(lambda);
    away_goals[i] ~ poisson(mu);
  }
}
