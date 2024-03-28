data {
  int<lower=1> num_clubs;                                     // number of clubs (total)
  int<lower=1> num_games;                                     // number of games (total)
  
  int<lower=1> num_seasons;                                   // number of seasons
  int<lower=1, upper=num_seasons> season[num_games];                     // seasons

  int<lower=1,upper=num_clubs> home_team_code[num_games];     // home club for game g
  int<lower=1,upper=num_clubs> away_team_code[num_games];     // away club for game g
  
  int<lower=1> num_leagues;                                   // number of leagues
  int<lower=1,upper=num_leagues> league[num_games];       // vector of league ids;
  int<lower=1,upper=num_clubs> num_clubs_league[num_leagues];                       // number of clubs (w/in league)
  
  real h_point_diff[num_games];                       // home point differential for game g

  int<lower=0,upper=1> h_adv[num_games];                    // indicator if game is home or neutral site
}
parameters {
  vector[num_clubs] theta;                  // team strength (as many as total # of clubs)

  vector<lower=0>[num_leagues] sigma_t;                  // team strength sd (one per league)
  vector<lower=0>[num_leagues] sigma_ai;                  // home advantage intercept sd (one per league)
  vector<lower=0>[num_leagues] sigma_m;                  // point diff sd (one per league)

  vector [num_leagues] alpha_intercept;                     // home advantage (league effect)
  vector [num_leagues] alpha_trend;                     // home advantage (league effect)
  
  real grand_trend; // home advantage (Shared trend)
  real <lower = 0> sigma_at; // sd of deviations shared trend

  
}
model {
  int pos_clubs;
  vector[num_games] mu;

  // priors
  pos_clubs = 1;
  for(l in 1:num_leagues) {
    segment(theta, pos_clubs, num_clubs_league[l]) ~ normal(0, sigma_t[l]);
    pos_clubs += num_clubs_league[l];
  }
    
  // half normal priors since we declared lower bounds of 0 on all the scale parameters  
  sigma_t ~ normal(0,5); 
  sigma_ai ~ normal(0,5); 
  sigma_m ~ normal(0,5); 
  sigma_at ~ normal(0,5); 
  
  alpha_intercept ~ normal(0, sigma_ai);
  alpha_trend ~ normal(grand_trend, sigma_at);

  // likelihood
  for(g in 1:num_games) {
    mu[g] = theta[home_team_code[g]] - theta[away_team_code[g]] + (alpha_trend[league[g]]  * season[g] + alpha_intercept[league[g]]) * h_adv[g];
    h_point_diff[g] ~ normal(mu[g], sigma_m[league[g]]);
  }
  
  // h_point_diff ~ normal(mu, sigma_m[league]); 
  
  
}
generated quantities{
  vector[num_games] mu;
  
  real log_lik = 0;
  
  for(g in 1:num_games) {
    mu[g] = theta[home_team_code[g]] - theta[away_team_code[g]] + (alpha_trend[league[g]]  * season[g] + alpha_intercept[league[g]]) * h_adv[g];
    
    // Update total log likelihood
    log_lik += normal_lpdf(h_point_diff[g] | mu[g], sigma_m[league[g]]);
  }
}
