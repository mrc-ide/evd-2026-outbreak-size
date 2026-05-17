rm(list = ls())

###################################################
### parameters
###################################################

#https://www.imperial.ac.uk/media/imperial-college/medicine/mrc-gida/2020-01-17-COVID19-Report-1.pdf
#prob_travel_during_window <- 10*3301/19e6 
## 10 is the detection window
## 3301 is passengers per day - Wuhan
## 19m is population of Wuhan

get_outbreak_size <- function(detection_window,
                              cases,
                              geography_scenario){
  
  if(!(geography_scenario %in% c("I","I+NK","I+NK+R"))){
   stop("geography_scenario incorrect") 
  }
  
  if(geography_scenario=="I"){
    passengers_per_day <- 1871
    population <- 4392200
  } else if(geography_scenario=="I+NK"){
    passengers_per_day <- 1871 + 2468
    population <- 4392200 + 9000000
  } else if(geography_scenario=="I+NK+R"){
    passengers_per_day <- 1871 + 2468 + 3296
    population <- 4392200 + 9000000 + 14600000
  }
   
  prob_travel_during_window <- detection_window * passengers_per_day/population
  ### exact negative binomial estimate and confidence interval
  exact_nb_central <- round(cases / prob_travel_during_window) 
  exact_nb_CI <- qnbinom(c(0.025,0.975),cases,prob_travel_during_window) + cases
  
  return(data.frame(
    "central" = exact_nb_central,
    "lower" = exact_nb_CI[1],
    "upper" = exact_nb_CI[2],
    "detection_window" = detection_window,
    "cases" = cases,
    "geography_scenario" = geography_scenario,
    "passengers_per_day" = passengers_per_day,
    "prob_travel_during_window" =  prob_travel_during_window,
    "population" = population
  ))
}

grid <- expand.grid("detection_window" = c(10,15,20),
                    "cases" = 2,
                    "geography_scenario" = c("I","I+NK","I+NK+R"))


results <- pmap(grid, get_outbreak_size) %>% bind_rows()



