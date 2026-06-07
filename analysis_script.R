## outbreak size analysis for Lancet ID correspondence

library(tidyverse)

## method 1: back calculation from deaths
source("estimate_outbreak_size_backcalc_deaths_with_pi.R")
backcalc_deaths_grid <- expand.grid("r" = log(2)/c(7,10,14),
                                    "cfr" = c(0.26,0.33,0.4),
                                    "deaths" = 240,
                                    "alpha" = 4.42,
                                    "beta" = 0.388,
                                    "k" = 1e6,
                                    "pi" = c(0.3,1))

backcalc_deaths_results <- pmap(backcalc_deaths_grid, estimate_outbreak_size_backcalc_deaths_with_pi) %>% bind_rows()

## table 1
backcalc_deaths_results %>% filter(pi==0.3)

## table S1 
backcalc_deaths_results %>% filter(pi==1)


## method 2: geographical spread
source("estimate_outbreak_size_geographical_spread.R")
geographical_spread_grid <- expand.grid(imported_cases = c(1,3),
                                        geography_scenario = c("I","I+NK"),
                                        case_scenario = c("cases","infections"),
                                        r = log(2)/c(7,10,14),
                                        mean_incub_period = 6.3)

geographical_spread_results <- pmap(geographical_spread_grid, estimate_outbreak_size_geographical_spread) %>% bind_rows()

## table 2
geographical_spread_results %>% filter(imported_cases==3,case_scenario=="cases")

## table s2
geographical_spread_results %>% filter(imported_cases==1,case_scenario=="cases")

## table s3
geographical_spread_results %>% filter(imported_cases==3,case_scenario=="infections")

