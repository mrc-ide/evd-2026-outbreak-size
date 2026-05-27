estimate_outbreak_size_backcalc_deaths <- function(#ttd_mean,
                                                   #ttd_sd,
                                                   alpha,
                                                   beta,
                                                   cfr,
                                                   deaths,
                                                   r,
                                                   k){
  doubling_time <- log(2)/r

  # raw estimate
  Nt <- (deaths/cfr) * (1 + (r/beta))^alpha
  
  # add CIs
  #Fixing r and then looping through values of T between 10 and 250 on a fine (0.02) grid to give required likelihood profile
  t_max_grid <- seq(10,250,0.02)
  
  results <- data.frame("t_max" = t_max_grid) %>%
    mutate(Nt_max = exp(r*t_max_grid),
           Dt_max = Nt_max * cfr / (1 + r/beta)^alpha,
           llt_max = lgamma(k + deaths) - lgamma(1 + deaths) - lgamma(k) +
             k * log(k / (k + Dt_max)) + deaths * log(Dt_max / (k + Dt_max)))

    ## bounds
    threshold <- max(results$llt_max)-1.92

    results_with_CIs <- results %>% mutate(
      lower = case_when(
        llt_max < threshold & lead(llt_max) > threshold ~ 1,
        .default = 0
      ),
      upper = case_when(
        llt_max > threshold & lead(llt_max) < threshold ~ 1,
        .default = 0
      )
    ) 

  return(data.frame("cfr" = cfr,
                    "deaths" = deaths,
                    "r" = r,
                    "doubling_time" = doubling_time,
                    "Nt" = Nt,
                    "N_central" = results_with_CIs %>% filter(llt_max==max(llt_max)) %>% 
                      select(Nt_max) %>% rename(N_central = Nt_max), 
                    "N_lower" = results_with_CIs %>% filter(lower==1) %>% 
                      select(Nt_max) %>% rename(N_lower = Nt_max),
                    "N_upper" = results_with_CIs %>% filter(upper==1) %>% 
                      select(Nt_max) %>% rename(N_upper = Nt_max)))
}






