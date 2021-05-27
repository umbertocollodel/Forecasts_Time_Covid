########## Script to construct weights used in aggregate forecasts from WEO over different 2020 issues

# Set parameters: ----


sheets=c("jan","apr","jun","oct") %>% 
  map_chr(~ paste0(.x,"2020"))

paths=rep("../Forecasts_Time_Covid_material/raw_data/gdp_ppp_2020.xlsx",4)



# Construct country weights for the country groups from GDP PPP serie: ----


# Dataframe with GDP PPP and country group classification:


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


%>% 
  map(~ .x %>% merge(read_xlsx("~/Dropbox/When_where_and_why/When_where_and_why_material/raw_data/country_group.xlsx"), by = "ifscode")) %>% 
  map(~ .x %>% as_tibble())

# Filter by income group and calculate weight as individula GDP value over total of the group:


df_weights_lidc <- preliminary_df_weights %>%   
  map(~ .x %>% filter(lidc == 1)) %>% 
  map(~ .x %>% group_by(year)) %>% 
  map(~ .x %>% mutate(weight = value/sum(value, na.rm = T))) 

df_weights_em <- preliminary_df_weights %>%   
  map(~ .x %>% filter(eme == 1 & lidc == 0)) %>% 
  map(~ .x %>% group_by(year)) %>% 
  map(~ .x %>% mutate(weight = value/sum(value, na.rm = T)))

df_weights_adv <- preliminary_df_weights %>%   
  map(~ .x %>% filter(adv == 1)) %>% 
  map(~ .x %>% group_by(year)) %>% 
  map(~ .x %>% mutate(weight = value/sum(value, na.rm = T))) 

# Get list of countries by group:

em_list=df_weights_em[[1]] %>% 
  .$ifscode %>% 
  unique()

lidc_list=df_weights_lidc[[1]] %>% 
  .$ifscode %>% 
  unique()

adv_list=df_weights_adv[[1]] %>% 
  .$ifscode %>% 
  unique()







