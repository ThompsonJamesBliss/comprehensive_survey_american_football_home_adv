# A Comprehensive Survey of the home Advantage in American football
This repository contains the data and code used in a manuscript _A Comprehensive Survey of the home Advantage in American football_ by Luke Benz, Thompson Bliss and Michael Lopez.

### Data

#### NFL:

* __data/final/NFL_games.csv:__ Includes info on season, teams, travel distance and scores for each Regular Season/Postseason NFL game 2004-2023 Regular Season.

NOTE: Data was pulled internally from NFL database so no R file is included to pull data.


#### NCAA:

* __data/final/NCAA_games.csv:__ Includes info on season, teams, travel distance and scores for each NCAA game from 2004-2023.
* __code/data/get_NCAA_data.R:__ Script to pull NCAA games data from [MasseyRatings](https://masseyratings.com/cf/fbs).

#### High School:

* __final/hs_games_[SEASON].csv:__: Folder containing all data used for this project. Each of the 17 leagues has its own folder, containing 5 csv files of game level statistics for games played that year. 
* __code/data/get_hs_data.R:__ Script to pull high school games data from [Max Preps](https://www.maxpreps.com/football/). Calls scripts from __code/data/high_school_helper__.
* __code/data/test_various_game_cutoffs_HS.R:__ Script to test various cut-offs for minimum number of games/seasons for a high school team to be included in analysis.

### Model

* __code/model/run_model.R__ Script to iteratively fit the three models for each state.
* __code/model/stan/model_[MODEL_NAME].stan__: Stan code for respective model.

### Results Analysis

* __code/results_analysis/loo_compare.R__ Script to compute expected log pointwise predictive density (ELPD) estimated via the leave-one-out cross-validation.
* __code/results_analysis/visualize_results.R__: Script to create visualize model results.


---

Note the `rstan` package is required to work with model objects and/or run the modeling scripts. For assistance installing Stan, please refer to the [official documentation](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).
