########## Script to construct weights used in aggregate forecasts from WEO over different 2020 issues:

# Set parameters: ----


sheets=c("jan","apr","jun","oct") %>% 
  map_chr(~ paste0(.x,"2020"))

paths=rep("../Forecasts_Time_Covid_material/raw_data/gdp_ppp_2020.xlsx",4)



# Construct country weights for global aggregate from GDP PPP serie: ----

# Forecasts at different horizons:


df_weights_global <- sheets %>% 
  map(~ read_xlsx("../Forecasts_Time_Covid_material/raw_data/gdp_ppp_2020.xlsx", sheet = .x)) %>%
  map(~ .x %>% slice(1: which(Country == "Zimbabwe"))) %>% 
  map(~ .x %>% select(Series_code,`1950`:ncol(.))) %>%
  map(~ .x %>% mutate(Series_code = str_extract(Series_code, "\\d{3}"))) %>% 
  map(~ .x %>% select(-matches("Q|M"))) %>% 
  map(~ .x %>% gather("year","value",`1950`:ncol(.))) %>% 
  map(~ .x %>% filter(year == 2020)) %>% 
  map(~ .x %>% rename(country_code = Series_code)) %>% 
  map(~ .x %>% mutate(weight = value/sum(value,na.rm = T))) %>% 
  map2(sheets, ~ .x %>% mutate(horizon = str_to_sentence(str_remove(.y,"2020")))) %>%
  bind_rows() %>% 
  select(-value)


# Actual value:

df_weights_global_actual <- read_xlsx("../Forecasts_Time_Covid_material/raw_data/gdp_ppp_2020.xlsx", sheet = "apr2021") %>% 
  slice(1: which(Country == "Zimbabwe")) %>% 
  select(Series_code,`1950`:ncol(.)) %>%
  mutate(Series_code = str_extract(Series_code, "\\d{3}")) %>% 
  select(-matches("Q|M")) %>% 
  gather("year","value",`1950`:ncol(.)) %>% 
  filter(year == 2020) %>% 
  rename(country_code = Series_code) %>% 
  mutate(weight = value/sum(value,na.rm = T)) %>%
  select(country_code,weight) %>% 
  rename(actual_weight = weight)
  
  