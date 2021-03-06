# DESCRIPTION: the script produces a dataframe with Consensus forecasts for 2020 for each month of the year and country. 
# FOR EXTERNAL USERS: change main path with location regional subdirectories Consensus forecasts and export path

# Wrangling Consensus forecasts function ----

#' Wrangling Consensus forecasts
#' 
#' Wrangling Consensus forecasts from excel with each individual sheet for every country to
#' more user-friendly dataframe.
#' 
#' @param main_path Path to directory with Consensus forecasts.
#' @param subdirectory Subdirectory after main path with regional distinction survey.
#' @param row_value_to_extract Number of row with the value to extract: 5 for mean, 7 for max, 8 for min and 9 for sd.
#' @param variable Name of the variable to extract. To get a sample of the correct names, read a sheet skipping the first two lines.
#' Default is GDP.
#' 
#' @return tibble with three identifiers (horizon, value, country)
#' 
#' @details Problem with Asia-Pacific: one sheet is missing data for Vietnam. For the moment we remove it completelly.



clean_consensus <- function(main_path,subdirectory,row_value_to_extract, variable = "Gross Domestic"){

  if(subdirectory == "Asia_Pacific"){
    
name_countries= list.files(paste0(main_path,"/",subdirectory)) %>% 
  map_chr(~ paste0(main_path,"/",subdirectory,"/",.x)) %>% 
  map(~ .x %>% getSheetNames()) %>% 
  map(~ .x %>% str_subset("A1|TrendCharts|Data|Additional|Forex|Oil|Copyright", negate = T)) %>% 
  .[[1]] %>% 
  str_subset("Vietnam",negate = T)
  }
  else{
  name_countries= list.files(paste0(main_path,"/",subdirectory)) %>% 
    map_chr(~ paste0(main_path,"/",subdirectory,"/",.x)) %>% 
    map(~ .x %>% getSheetNames()) %>% 
    map(~ .x %>% str_subset("A1|TrendCharts|Data|Additional|Forex|Oil|Copyright", negate = T)) %>% 
    .[[1]]
  }

  
path_files=list.files(paste0(main_path,"/",subdirectory)) %>% 
  map_chr(~ paste0(main_path,"/",subdirectory,"/",.x))



df_to_clean=path_files %>% 
  map(~ name_countries %>% 
        map(function(x) { 
            read_xlsx(.x, sheet = x, skip = 2)
  })
  ) 



dfs_final =df_to_clean %>%
  modify_depth(2, ~ .x %>% slice(row_value_to_extract)) %>% 
  modify_depth(2, ~ .x %>% select(variable)) %>%
  modify_depth(2, ~ .x %>% setNames(c("value"))) %>%
  map(~ .x %>% bind_rows()) %>% 
  map(~ .x %>% mutate(country = name_countries))

names(dfs_final) = path_files %>% str_extract("[A-Z]{1}[a-z]{2}2020") %>% str_remove("2020")

  
dfs_final %>% 
  bind_rows(.id = "horizon") %>% 
  arrange(horizon) %>% 
  mutate(value = as.numeric(value)) 

}



# Set parameters before running the function: -----


main_path="../Forecasts_Time_Covid_material/raw_data/Consensus_2020"
subdirectory=c("Asia_Pacific","G7_Western_Europe","East_Europe","Latin_America") # Regions sheets
rows=c(5,7,8,9) # Distribution values of Consensus forecasts


# Running the function and export intermediate datasets: -----
# Note: apply function for each value of the distribution we want to extract and each region.
# We obtain a nested list whose upper level is the value of the distribution and the lower is the region. 


# Run for GDP:


gdp_forecasts <- rows %>% 
  map(~ map(subdirectory, function(x){
    clean_consensus(main_path,x,.x,"Gross Domestic")
    })
  ) %>% 
  map(~ .x %>% bind_rows())
  map(~ .x %>% mutate(country = case_when(country == "USA" ~ "United States",
                           country == "UK" ~ "United Kingdom",
                           country == "Czechia" ~ "Czech Republic",
                           T ~ country)))


# Run for inflation:

inflation_forecasts <- rows %>% 
  map(~ map(subdirectory, function(x){
    clean_consensus(main_path,x,.x,contains("Consumer"))
  })
  ) %>% 
  map(~ .x %>% bind_rows()) %>% 
  map(~ .x %>% mutate(country = case_when(country == "USA" ~ "United States",
                                        country == "UK" ~ "United Kingdom",
                                        country == "Czechia" ~ "Czech Republic",
                                        T ~ country)))




# Export:


# Export GDP:

gdp_forecasts %>% 
  walk2(c("","_max","_min","_sd"), ~ saveRDS(.x, paste0("../Forecasts_Time_Covid_material/intermediate_data/consensus_2020",.y,".RDS")))
 
# Export inflation:
 
inflation_forecasts %>% 
  walk2(c("","_max","_min","_sd"), ~ saveRDS(.x, paste0("../Forecasts_Time_Covid_material/intermediate_data/consensus_2020_inflation",.y,".RDS")))


