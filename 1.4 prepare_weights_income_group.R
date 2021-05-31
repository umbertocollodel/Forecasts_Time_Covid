########## Script to construct weights used in aggregate forecasts (AE, EM and LICS) from WEO over different 2020 issues: -----



########## Script to construct weights used in aggregate forecasts from WEO over different 2020 issues:

# Set parameters: ----


sheets=c("jan","apr","jun","oct") %>% 
  map_chr(~ paste0(.x,"2020"))

paths=rep("../Forecasts_Time_Covid_material/raw_data/gdp_ppp_2020.xlsx",4)



# Clean GDP PPP data for every WEO issue ----


df_gdp_ppp <- sheets %>% 
  map(~ read_xlsx("../Forecasts_Time_Covid_material/raw_data/gdp_ppp_2020.xlsx", sheet = .x)) %>%
  map(~ .x %>% slice(1: which(Country == "Zimbabwe"))) %>% 
  map(~ .x %>% select(Series_code,`1950`:ncol(.))) %>%
  map(~ .x %>% mutate(Series_code = str_extract(Series_code, "\\d{3}"))) %>% 
  map(~ .x %>% select(-matches("Q|M"))) %>% 
  map(~ .x %>% gather("year","value",`1950`:ncol(.))) %>% 
  map(~ .x %>% filter(year == 2020)) %>% 
  map(~ .x %>% rename(ifscode = Series_code)) 


# Combine with country classification: -----

country_groups=read.xlsx("~/Dropbox/When_where_and_why/When_where_and_why_material/raw_data/country_group.xlsx") %>% 
  select(ifscode, matches("^[a-z]{3,4}$"))


preliminary_df <- df_gdp_ppp %>% 
  map(~ .x %>% merge(country_groups, by=c("ifscode"))) %>% 
  map(~ .x %>% as_tibble())


# Create weights by different country groups: -----

conditions=list("adv == 1", 
             "eme == 1 & lidc == 0",
             "eme == 1 & lidc == 1")


list_df <- preliminary_df %>% 
  map(~ map(conditions, function(x){
    .x %>% 
      filter_(x)
  })) %>% 
  modify_depth(2, ~ .x %>% mutate(weight = value/sum(value,na.rm = T))) %>% 
  modify_depth(2, ~ .x %>% select(-value))



# Organize better: -----

tidy_weights <- function(list,number_list_element){
  
  list %>% 
  map(~ .x[[number_list_element]]) %>% 
  map2(sheets, ~ .x %>% mutate(horizon = str_to_sentence(str_remove(.y,"2020")))) %>%
  bind_rows() 

}


final_list <- 1:3 %>% 
  map(~ list_df %>% tidy_weights(.x)) 


names(final_list)=c("adv","em","lidc")


# Export: -----


final_list %>% 
  iwalk(~ saveRDS(.x, paste0("../Forecasts_Time_Covid_material/intermediate_data/weights_",.y,".RDS")))
















 