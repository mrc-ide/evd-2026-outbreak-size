## outbreak size analysis

library(tidyverse)
## method 1: back calculation from deaths
source("estimate_outbreak_size_backcalc_deaths.R")
backcalc_deaths_grid <- expand.grid("r" = log(2)/c(7,10,14),
                                    "cfr" = c(0.26,0.33,0.4),
                                    "deaths" = 204,
                                    "alpha" = 4.42,
                                    "beta" = 0.388,
                                    "k" = 1e6)

backcalc_deaths_results <- pmap(backcalc_deaths_grid, estimate_outbreak_size_backcalc_deaths) %>% bind_rows()
backcalc_deaths_results 

## method 2: geographical spread
source("estimate_outbreak_size_geographical_spread.R")
geographical_spread_grid <- expand.grid(imported_cases = 3,
                                        geography_scenario = c("I","I+NK"),
                                        case_scenario = c("cases","infections"),
                                              r = log(2)/c(7,10,14),
                                              mean_incub_period = 6.3)
geographical_spread_results <- pmap(geographical_spread_grid, estimate_outbreak_size_geographical_spread) %>% bind_rows()
geographical_spread_results 
