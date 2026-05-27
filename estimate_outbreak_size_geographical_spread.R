estimate_outbreak_size_geographical_spread <- function(imported_cases,
                                                       geography_scenario,
                                                       case_scenario,
                                                       #alpha,
                                                       #beta,
                                                       r,
                                                       mean_incub_period){
  
  if(!(geography_scenario %in% c("I","I+NK"))){
   stop("geography_scenario incorrect") 
  }
  
  if(!(case_scenario %in% c("cases","infections"))){
    stop("case_scenario incorrect")
  }
  
  
  if(geography_scenario=="I"){
    passengers_per_day <- 1871
    population <- 4392200
  } else if(geography_scenario=="I+NK"){
    passengers_per_day <- 1871 + 2468
    population <- 4392200 + 8147400
  } 
  
  if(case_scenario=="cases"){
    alpha <- 4.42
    beta <- 0.388
  } else if(case_scenario=="infections"){
    alpha <- 6.93
    beta <- 0.388
  }
  
  effective_detection_window <- (1-1/((1+r/beta)^alpha))/r  ## as r->0 this tends to alpha3/beta3=mean of infection to outcome dist
  prob_travel_during_window <- effective_detection_window * passengers_per_day/population
   
  
  exact_nb_central <- round(imported_cases / prob_travel_during_window) 
  exact_nb_CI <- qnbinom(c(0.025,0.975),imported_cases,prob_travel_during_window) + imported_cases

    corr_nb_central <- exact_nb_central * exp(-r * mean_incub_period)
    corr_nb_CI <- exact_nb_CI * exp(-r * mean_incub_period)

    p_new <- prob_travel_during_window/(1-(1-prob_travel_during_window)*(1-exp(-r * mean_incub_period)))
    
    ## note that p_new -> prob_travel_during_window * exp(r * mean_incub_period) as prob_travel_during_window->0, as we want
    compound_nb_central <- round(imported_cases / p_new) 
    compound_nb_CI <- qnbinom(c(0.025,0.975),imported_cases,p_new) + imported_cases
  
  return(data.frame(
    "central" = ifelse(case_scenario=="cases",exact_nb_central,compound_nb_central),
    "lower" = ifelse(case_scenario=="cases",exact_nb_CI[1],compound_nb_CI[1]),
    "upper" = ifelse(case_scenario=="cases",exact_nb_CI[2],compound_nb_CI[2]),
    "effective_detection_window" = effective_detection_window,
    "imported_cases" = imported_cases,
    "geography_scenario" = geography_scenario,
    "case_scenario" = case_scenario,
    "passengers_per_day" = passengers_per_day,
    "prob_travel_during_window" =  prob_travel_during_window,
    "population" = population,
    "r" = r,
    "doubling_time" = log(2)/r,
    "alpha" = alpha,
    "beta" = beta
  ))
}




