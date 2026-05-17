## estimate movement 

library(tidyverse)

df <- readxl::read_excel("data/PoE_UGA.xlsx",sheet="combined")
#df %>% summary()

df_summary <- df %>% filter(bordering_province!="NA") %>%
  group_by(POE) %>%
  mutate(daily_total_screened = `Travelers passing through`/7) %>%
  summarise(mean_daily_total_screened = mean(daily_total_screened),
            n_obs = length(POE),
            province = first(bordering_province)) 

# daily crossings per province 
df_summary %>% group_by(province) %>% summarise(
  total = sum(mean_daily_total_screened)
)

# n obs
df %>% 
  filter(Uganda_Rwanda==1|Uganda_DRC==1) %>% 
  group_by(POE) %>%
  summarise(n = length(POE))

# average weekly crossings
df %>% 
  filter(Uganda_Rwanda==1|Uganda_DRC==1) %>% 
  group_by(POE) %>%
  summarise(mean = mean(`Travelers passing through`))


