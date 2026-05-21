## outbreak size analysis for paper

library(tidyverse)

## method 1: back calculation from deaths
source("estimate_outbreak_size_backcalc_deaths.R")
backcalc_deaths_grid <- expand.grid("r" = log(2)/c(7,10,14),
                                    "cfr" = c(0.26,0.33,0.4),
                                    "deaths" = 139,
                                    "ttd_mean" = 11.37,
                                    "ttd_sd" = 5.41,
                                    "k" = 1000000)

backcalc_deaths_results <- pmap(backcacl_deaths_grid, estimate_outbreak_size_backcacl_deaths) %>% bind_rows()
rownames(backcalc_deaths_results) <- NULL

## method 2: geographical spread
source("estimate_outbreak_size_geographical_spread.R")
geographical_spread_grid <- expand.grid(imported_cases = 2,
                                        geography_scenario = c("I","I+NK"),
                                        alpha = ,
                                        beta,
                                        r = log(2)/c(7,10,14),
                                        mean_incub_period = 7)
geographical_spread_results <- pmap(geographical_spread_grid, estimate_outbreak_size_geographical_spread) %>% bind_rows()

