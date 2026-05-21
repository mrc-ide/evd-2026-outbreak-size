# #https://www.imperial.ac.uk/media/imperial-college/medicine/mrc-gida/2020-01-17-COVID19-Report-1.pdf
# #prob_travel_during_window <- 10*3301/19e6 
# ## 10 is the detection window
# ## 3301 is passengers per day - Wuhan
# ## 19m is population of Wuhan
# 
# cases <- 2
# geography_scenario <- "I"
# r <- 0.05
# mean_incub_period <- 7
# alpha <- 4.42
# beta <- 0.39
# ### shape and rate 

alpha <- epitrix::gamma_mucv2shapescale(mu = , cv =)
beta <- epitrix::gamma_mucv2shapescale()
#scale <- epitrix::gamma_mucv2shapescale(mu = ttd_mean, cv = ttd_sd / ttd_mean)$scale
#shape <- epitrix::gamma_mucv2shapescale(mu = ttd_mean, cv = ttd_sd / ttd_mean)$shape

estimate_outbreak_size_geographical_spread <- function(imported_cases,
                                                       geography_scenario,
                                                       alpha,
                                                       beta,
                                                       r,
                                                       mean_incub_period){
  
  if(!(geography_scenario %in% c("I","I+NK"))){
   stop("geography_scenario incorrect") 
  }
  
  if(geography_scenario=="I"){
    passengers_per_day <- 1871
    population <- 4392200
  } else if(geography_scenario=="I+NK"){
    passengers_per_day <- 1871 + 2468
    population <- 4392200 + 9000000
  } 
  
  effective_detection_window <- (1-1/((1+r/beta)^alpha))/r  ## as r->0 this tends to alpha3/beta3=mean of infection to outcome dist
  prob_travel_during_window <- effective_detection_window * passengers_per_day/population
   
  ### exact negative binomial estimate and confidence interval
  exact_nb_central <- round(imported_cases / prob_travel_during_window) 
  exact_nb_CI <- qnbinom(c(0.025,0.975),imported_cases,prob_travel_during_window) + cases
  
  ## now need to back transform infections to symptomatic cases
  ## First with just a fixed scaling:
  corr_nb_central <- exact_nb_central * exp(-r * mean_incub_period)
  corr_nb_CI <- exact_nb_CI * exp(-r * mean_incub_period)
  
  ## But better to do it probabilistically
  ## at it's simplest, this would require combining a binomial and negative binomial
  ## result of that is another negative binomial, but with different "p"
  p_new <- prob_travel_during_window/(1-(1-prob_travel_during_window)*(1-exp(-r * mean_incub_period)))
  
  ## note that p_new -> prob_travel_during_window * exp(r * mean_incub_period) as prob_travel_during_window->0, as we want
  compound_nb_central <- round(imported_cases / p_new) 
  compound_nb_CI <- qnbinom(c(0.025,0.975),imported_cases,p_new) + imported_cases
  
  return(data.frame(
    "central_exact" = exact_nb_central,
    "lower_exact" = exact_nb_CI[1],
    "upper_exact" = exact_nb_CI[2],
    "central_compound" = compound_nb_central,
    "lower_compound" = compound_nb_CI[1],
    "upper_compound" = compound_nb_CI[2],
    "central_corr" = corr_nb_central,
    "lower_corr" = corr_nb_CI[1],
    "upper_corr" = corr_nb_CI[2],
    "effective_detection_window" = effective_detection_window,
    "imported_cases" = imported_cases,
    "geography_scenario" = geography_scenario,
    "passengers_per_day" = passengers_per_day,
    "prob_travel_during_window" =  prob_travel_during_window,
    "population" = population,
    "p_new" = p_new
  ))
}



