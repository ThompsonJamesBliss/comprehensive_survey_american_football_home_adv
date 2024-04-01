### Function to quickly computer the posterior means of all parameters
### which may differ in dimension
posterior_means <- function(x) {
  if (length(dim(x)) == 2) { ### 2-D array of draws, posterior means are column means of array
    return(colMeans(x))
  } else if (length(dim(x)) == 1) { ### 1-D vector
    return(mean(x))
  }
}

### Function to compute DIC give a stan model object, which model, and the input data
compute_dic <- function(model, likelihood, stan_data) {
  ### Extract Posterior Samples
  posterior <- extract(model)
  
  ### Posterior Mean of all parameters
  post_means <- map(posterior, posterior_means)
  
  ### (1) Posterior Mean of the log-likelihood of each iteration
  mean_log_lik <- post_means$log_lik
  
  ### (2) Compute log-liklihood at posterior mean
  log_lik_mean <- 0
  mu <- rep(NA, stan_data$num_games)
  for (i in 1:stan_data$num_games) {
    ### Unpack data for game
    home_ix <- stan_data$home_team_code[i]
    away_ix <- stan_data$away_team_code[i]
    h_adv <- stan_data$h_adv[i]
    h_point_diff <- stan_data$h_point_diff[i]
    distance <- stan_data$distance[i]
    season <- stan_data$season[i]
    school_matchup_type <- stan_data$school_matchup_type[i]
    league <- stan_data$league[i]
    
    ### Mean of normal distrbution according to model we are fitting
    if (likelihood == "model_1") {
      mu[i] <-
        post_means$theta[home_ix] -
        post_means$theta[away_ix] +
        post_means$alpha * h_adv
    } else if (likelihood == "model_2") {
      mu[i] <-
        post_means$theta[home_ix] -
        post_means$theta[away_ix] +
        (post_means$alpha_trend * season + post_means$alpha_intercept) * h_adv
    } else if (likelihood == "model_3") {
      mu[i] <-
        post_means$theta[home_ix] -
        post_means$theta[away_ix] +
        post_means$alpha[season] * h_adv
    } else if(likelihood == 'model_2_hierarchical') {
      mu[i] <-
        post_means$theta[home_ix] -
        post_means$theta[away_ix] +
        (post_means$alpha_trend[league] * season + post_means$alpha_intercept[league]) * h_adv
      
    }
    
    ### Increment Likelihood
    if(likelihood == 'model_2_hierarchical') {
      sigma_m <- post_means$sigma_m[league]
    } else {
      sigma_m <- post_means$sigma_m
    }
    
    log_lik_mean <-
      log_lik_mean +
      dnorm(x = h_point_diff, mean = mu[i], sd = sigma_m, log = T)
  }
  
  ### Compute DIC
  deviance_mean <- -2 * log_lik_mean
  mean_deviance <- -2 * mean_log_lik
  pD <- mean_deviance - deviance_mean
  DIC <- pD + mean_deviance
  return(DIC)
}


### Function to compute pointwise log-likelihood [will take a few seconds]
expand_log_lik <- function(model, likelihood, stan_data) {
  ### Extract Posterior Samples
  posterior <- extract(model)
  n_iter <- length(posterior$log_lik)
  if(n_iter == 0) {
    n_iter <- length(posterior$lp__)
  }
    
  n_games <- stan_data$num_games
  
  ### (2) Compute log-liklihood at each iteration
  log_lik <- matrix(NA, nrow = n_iter, ncol = n_games)
  
  ### Unpack data for games
  home_ix <- stan_data$home_team_code
  away_ix <- stan_data$away_team_code
  h_adv <- stan_data$h_adv
  h_point_diff <- stan_data$h_point_diff
  distance <- stan_data$distance
  season <- stan_data$season
  school_matchup_type <- stan_data$school_matchup_type
  league <- stan_data$league
  
  for (i in 1:n_iter) {
    ### Mean of normal distrbution according to model we are fitting
    if (likelihood == "model_1") {
      mu <-
        posterior$theta[i, home_ix] -
        posterior$theta[i, away_ix] +
        posterior$alpha[i] * h_adv
    } else if (likelihood == "model_2") {
      mu <-
        posterior$theta[i, home_ix] -
        posterior$theta[i, away_ix] +
        (posterior$alpha_trend[i] * season + posterior$alpha_intercept[i]) * h_adv
    } else if (likelihood == "model_3") {
      mu <-
        posterior$theta[i, home_ix] -
        posterior$theta[i, away_ix] +
        posterior$alpha[i, season] * h_adv
    } else if(likelihood == 'model_2_hierarchical') {
      mu <-
        posterior$theta[i, home_ix] -
        posterior$theta[i, away_ix] +
        (posterior$alpha_trend[i, league] * season + posterior$alpha_intercept[i, league]) * h_adv
      
    }
    
    ### Save Log-Likelihood
    if(likelihood == 'model_2_hierarchical') {
      sigma_m <- posterior$sigma_m[i, league]
    } else {
      sigma_m <- posterior$sigma_m[i]
    }
    
    log_lik[i, ] <- dnorm(x = h_point_diff, mean = mu, sd = sigma_m, log = T)
  }
  
  return(log_lik)
}

retry_this <- function(expr, error_return = NA,
                       max_tries = 3, sleep_time = 1,
                       silent = FALSE) {
  attempts <- 0
  retval <- try(eval(expr), silent = silent)
  while ("try-error" %in% class(retval)) {
    attempts <- attempts + 1
    if (attempts >= max_tries) {
      return(error_return)
    }
    Sys.sleep(sleep_time)
    retval <- try(eval(expr), silent = silent)
  }
  return(retval)
}



get_loo_comparison <- function(league, model_names) {
  ### Read in Objects
  loo_objects <-
    map(model_names, ~read_rds(paste0("stan_results/", .x, "/loo_objects/", league, ".rds")))
  
  ### Execute Comparison
  df_compare <- 
    loo_compare(loo_objects) %>% 
    as_tibble(rownames = 'model_name') %>% 
    mutate('league' = league,
           'model_name' = gsub('model', 'model_', model_name))
  
  return(df_compare)
}


### Function to compute R-hat and ESS statistics 
model_diagnostics <- function(model) {
  
  model_summary <- summary(model)
  df_summary <- 
    as.data.frame(model_summary[[1]]) %>% 
    mutate('parameter' = as.factor(gsub("\\[.*]", "", rownames(.)))) %>% 
    filter(grepl('alpha', parameter) | grepl('theta', parameter) | grepl('sigma', parameter) | grepl('grand', parameter)) %>% 
    group_by(parameter) %>% 
    summarise('rhat_pct' = mean(abs(Rhat - 1) < 0.1),
              'max_rhat' = max(Rhat),
              'min_rhat' = min(Rhat),
              'mean_ess' = mean(n_eff)) 
  
  return(df_summary)
}

