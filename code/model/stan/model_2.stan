data {
  int<lower=1> num_clubs;                                     // number of clubs
  int<lower=1> num_games;                                     // number of games

  int<lower=1> num_seasons;                                   // number of seasons
  int<lower=1, upper=num_seasons> season[num_games];                     // seasons

  int<lower=1,upper=num_clubs> home_team_code[num_games];     // home club for game g
  int<lower=1,upper=num_clubs> away_team_code[num_games];     // away club for game g
  
  real h_point_diff[num_games];                       // home point differential for game g

  int<lower=0,upper=1> h_adv[num_games];                    // indicator if game is home or neutral site
}
parameters {
  vector[num_clubs] theta;                  // team strength

  real<lower=0> sigma_t;                  // team strength sd
  
  real<lower=0> sigma_ai;                  // home advantage intercept sd
  
  real<lower=0> sigma_at;                  // home advantage trend sd
  
  real<lower=0> sigma_m;                  // point diff sd

  real alpha_intercept;                     // home advantage
  
  real alpha_trend;                     // home advantage

  
}
model {
  vector[num_games] mu;

  // priors (half normal on scale parameters)
  theta ~ normal(0, sigma_t);
  sigma_t ~ normal(0, 5);
  alpha_intercept ~ normal(0, sigma_ai);
  alpha_trend ~ normal(0, sigma_at);
  sigma_ai ~ normal(0, 5);
  sigma_at ~ normal(0, 5);
  sigma_m ~ normal(0, 5);


  
  // likelihood
  for (g in 1:num_games) {
    mu[g] = theta[home_team_code[g]] - theta[away_team_code[g]] + (alpha_trend  * season[g] + alpha_intercept) * h_adv[g];
  }
  
  h_point_diff ~ normal(mu, sigma_m);
  
}
generated quantities{
  vector[num_games] mu;
  
  real log_lik = 0;
  
  for(g in 1:num_games) {
    mu[g] = theta[home_team_code[g]] - theta[away_team_code[g]] + (alpha_trend  * season[g] + alpha_intercept) * h_adv[g];
    
    // Update total log likelihood
    log_lik += normal_lpdf(h_point_diff[g] | mu[g], sigma_m);
  }
}