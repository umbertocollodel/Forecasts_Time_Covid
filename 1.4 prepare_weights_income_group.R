########## Script to construct weights used in aggregate forecasts (AE, EM and LICS) for 2020 from WEO over different 2020 issues: -----

# Set parameters: ----


sheets=c("jan","apr","jun","oct") %>% 
  map_chr(~ paste0(.x,"2020"))


# Clean GDP PPP data for every WEO issue ----


list_gdp_ppp <- sheets %>% 
  map(~ clean_weo_2020_forecasts("../Forecasts_Time_Covid_material/raw_data/gdp_ppp_2020.xlsx",sheet = .x)) %>%
  map(~ .x %>% rename(ifscode = country_code)) %>% 
  suppressWarnings()


# Combine with country classification: -----

country_groups_df=read.xlsx("~/Dropbox/When_where_and_why/When_where_and_why_material/raw_data/country_group.xlsx") %>% 
  select(ifscode, matches("^[a-z]{3,4}$"))


list_gdp_ppp_classified <- df_gdp_ppp %>% 
  map(~ .x %>% merge(country_groups, by=c("ifscode"))) %>% 
  map(~ .x %>% as_tibble())


# Create weights for every country group and WEO issue: -----
# Note: the "double map" produces for every WEO issue three dfs with the relative income groups

conditions=list("adv == 1", 
             "eme == 1 & lidc == 0",
             "eme == 1 & lidc == 1")


list_weights <- preliminary_df %>% 
  map(~ map(conditions, function(x){
    .x %>% 
      filter_(x)
  })) %>% 
  modify_depth(2, ~ .x %>% mutate(weight = value/sum(value,na.rm = T))) %>% 
  modify_depth(2, ~ .x %>% select(-value))



# Combine together different WEO issues for each income group: -----

tidy_weights_group <- function(list,number_list_element){
  
  list %>% 
  map(~ .x[[number_list_element]]) %>% 
  map2(sheets, ~ .x %>% mutate(horizon = str_to_sentence(str_remove(.y,"2020")))) %>%
  bind_rows() 

}


list_weights_group <- 1:3 %>% 
  map(~ list_weights %>% tidy_weights_group(.x)) 


names(list_weights_group)=c("adv","em","lidc")


# Export: -----


list_weights_group %>% 
  iwalk(~ saveRDS(.x, paste0("../Forecasts_Time_Covid_material/intermediate_data/weights_aggregates/weights_",.y,".RDS")))
















 