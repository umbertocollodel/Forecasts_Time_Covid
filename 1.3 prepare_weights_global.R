########## Script to construct weights used in global forecasts for 2020 from WEO over different 2020 issues:

# Set parameters: ----


sheets=c("jan","apr","jun","oct") %>% 
  map_chr(~ paste0(.x,"2020"))


# Construct country weights for global aggregate from GDP PPP serie: ----

# Forecasts at different horizons:


df_weights_global <- sheets %>% 
  map(~ clean_weo_2020_forecasts("../Forecasts_Time_Covid_material/raw_data/gdp_ppp_2020.xlsx",sheet = .x)) %>% 
  map(~ .x %>% mutate(weight = value/sum(value,na.rm = T))) %>% 
  map2(sheets, ~ .x %>% mutate(horizon = str_to_sentence(str_remove(.y,"2020")))) %>%
  bind_rows() %>% 
  select(-value) %>% 
  suppressWarnings()


# Actual value (April 2020 WEO issue): 

df_weights_global_actual <- clean_weo_2020_forecasts("../Forecasts_Time_Covid_material/raw_data/gdp_ppp_2020.xlsx", sheet = "apr2021") %>% 
  mutate(weight = value/sum(value,na.rm = T)) %>%
  select(country_code,weight) %>% 
  rename(actual_weight = weight) %>% 
  suppressWarnings()
  
  

# Export: ----

list(df_weights_global, df_weights_global_actual) %>% 
  walk2(c("","_actual"), ~ saveRDS(.x,paste0("../Forecasts_Time_Covid_material/intermediate_data/weights_aggregates/weights_global",.y,".RDS")))