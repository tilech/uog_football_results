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
  real home_advantage;       // Home advantage
  real mu_attack;
  real mu_defense;
  real<lower=0> tau_attack;
  real<lower=0> tau_defense;
}

transformed parameters {
  vector[n_teams] attack;
  vector[n_teams] defense;

  // need to make sum(att)=sum(def)=0
  for (k in 1:(n_teams-1)) {
    attack[k] = attack_free[k];
    defense[k] = defense_free[k];
  }
  attack[n_teams] = -sum(attack_free);
  defense[n_teams] = -sum(defense_free);

}

model {
  // Priors
    mu_attack ~ normal(0, 100);    // Prior for mu_attack
    mu_defense ~ normal(0, 100);   // Prior for mu_defense
    tau_attack ~ gamma(0.1, 0.1);   // Prior for tau_attack
    tau_defense ~ gamma(0.1, 0.1);  // Prior for tau_defense
    attack ~ normal(mu_attack, 1/tau_attack);  // Attack strength modeling
    defense ~ normal(mu_defense, 1/tau_defense); // Defense strength modeling
    home_advantage ~ normal(0, 1);  // Home advantage prior

  // Likelihood with time weights
    for (i in 1:n_games) {
      home_goals[i] ~ poisson(exp(attack[home_team[i]] - defense[away_team[i]] + home_advantage));
      away_goals[i] ~ poisson(exp(attack[away_team[i]] - defense[home_team[i]]));
    }
}
