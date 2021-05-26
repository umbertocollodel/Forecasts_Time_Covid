########### Script to prepare WEO forecasts: 
# DESCRIPTION: cleans forecasts for 2020 growth rate from all 2020 WEO issues
# FOR EXTERNAL USERS: change paths to local paths with january and july updates and april and october issues


# Clean June,January 2020 WEO and Actuals from April 2021 WEO ----

# Set Parameters

paths=c("~/Dropbox/When_where_and_why/When_where_and_why_material/raw_data/weo_july_update.xlsx",
        "~/Dropbox/When_where_and_why/When_where_and_why_material/raw_data/weo_january_update.xlsx",
        "~/Dropbox/When_where_and_why/When_where_and_why_material/raw_data/weo_rgdp.xlsx")

sheets=c("jul2020","jan2020","apr2021")

var_name=c("Jun","Jan","Actual")

# Clean


forecasts_updates <- paths %>% 
  map2(sheets, ~ read_xlsx(.x,sheet = .y)) %>% 
  map(~ .x %>% slice(1: which(Country == "Zimbabwe"))) %>% 
  map(~ .x %>% select(Series_code, `2019`:`2020`)) %>% 
  map(~ .x %>% select(-matches("Q"))) %>% 
  map(~ .x %>% mutate(country_code = str_extract(Series_code,"\\d{3}"))) %>% 
  map(~ .x %>% mutate(growth_2020 = ((`2020` - `2019`)/`2019`)*100,
               year = "2020")) %>% 
  map(~ .x %>% select(country_code,year,growth_2020)) %>% 
  map2(var_name, ~ .x %>% setNames(c("country_code", "year", .y)))



# Wrangle WEO April & October: ----


# Custom function:

wrangle_weo_forecasts <- function(path = "../When_where_and_why_material/raw_data/weo_rgdp.xlsx", year_exclude = "2019") {
  
  path = path
  
  
  sheets_name <- getSheetNames(path)
  sheets_year <- getSheetNames(path) %>%
    str_extract("\\d{4}")
  
  # Calculate growth rates only for real Gdp and inflation:
  
  if(str_detect(path, paste(c("rgdp","pcpi"),collapse = "|"))){
    
    forecasts <- sheets_name %>% 
      map(~ read_xlsx(path,sheet = .x)) %>%
      map(~ .x %>% slice(1:which(Country == "Zimbabwe"))) %>% 
      map(~ .x %>% gather("year_forecasted","variable",7:ncol(.))) %>% 
      map(~ .x %>% group_by(Series_code) %>% mutate(variable = as.numeric(variable)) %>% mutate(variable = ((variable - dplyr::lag(variable,1))/dplyr::lag(variable,1))*100)) %>% 
      map(~ .x %>% ungroup())
  } else {
    forecasts <- sheets_name %>% 
      map(~ read_xlsx(path,sheet = .x)) %>%
      map(~ .x %>% slice(1:which(Country == "Zimbabwe"))) %>% 
      map(~ .x %>% gather("year_forecasted","variable",7:ncol(.)))
  }
  
  forecasts <- forecasts %>% 
    map(~ .x %>% mutate(Series_code = str_extract(Series_code, "\\d{3}"))) %>%  
    map(~ .x %>% select(Series_code, year_forecasted, variable)) %>% 
    map2(sheets_name, ~ .x %>% mutate(date_publication = .y)) %>% 
    map2(sheets_year, ~ .x %>% mutate(year_publication = .y)) %>% 
    map(~ .x %>% filter(year_forecasted >= year_publication & year_forecasted <= as.character(as.numeric(year_publication) + 5))) %>%
    map(~ .x %>% filter(complete.cases(Series_code))) %>% 
    bind_rows() %>% 
    group_split(year_forecasted) %>% 
    map(~ .x %>% select(-year_publication)) %>% 
    map(~ .x %>% spread(date_publication,variable)) %>% 
    map(~ .x %>% rename_at(vars(starts_with("apr")), ~ paste0(.,"apr"))) %>% 
    map(~ .x %>% rename_at(vars(starts_with("apr")), ~ str_remove(.,"^apr"))) %>% 
    map(~ .x %>% rename_at(vars(starts_with("oct")), ~ paste0(.,"oct"))) %>% 
    map(~ .x %>% rename_at(vars(starts_with("oct")), funs(str_remove(.,"^oct"))))
  
  # Order names of the columns:
  
  for(i in 1:length(forecasts)){
    forecasts[[i]] <- forecasts[[i]][,order(names(forecasts[[i]]))]
  }
  
  # Discard years for which no actual value:
  
  
  forecasts <- forecasts %>% 
    discard(~ unique(.x$year_forecasted) > year_exclude)
  
  
  # Naming similar to Zidong and bind together:
  
  
  final_forecasts <-  forecasts %>% 
    map(~ if(length(names(.x)) == 14){
      .x %>% setNames(c(rev(paste0("variable",seq(12:1))),"country_code","year"))
    } else if(length(names(.x)) == 12){
      .x %>% setNames(c(rev(paste0("variable",seq(10:1))),"country_code","year"))
    } else if(length(names(.x)) == 10){
      .x %>% setNames(c(rev(paste0("variable",seq(8:1))),"country_code","year"))
    } else if(length(names(.x)) == 8){
      .x %>% setNames(c(rev(paste0("variable",seq(6:1))),"country_code","year"))
    } else if(length(names(.x)) == 6){
      .x %>% setNames(c(rev(paste0("variable",seq(4:1))),"country_code","year"))
    } else if(length(names(.x)) == 4){
      .x %>% setNames(c(rev(paste0("variable",seq(2:1))),"country_code","year"))
    }
    ) %>%  
    bind_rows() %>% 
    mutate(country = countrycode(country_code,"imf","country.name")) %>% 
    filter(complete.cases(country)) %>% 
    select(country_code, country, year, everything()) %>% 
    arrange(country)
  
  return(final_forecasts)
  
}


# Wrangle:


forecast_2020 <- wrangle_weo_forecasts("~/Dropbox/When_where_and_why/When_where_and_why_material/raw_data/weo_rgdp.xlsx", year_exclude = 2020) %>% 
  filter(year == 2020) %>% 
  select(country_code,country, year, num_range("variable",1:2)) %>% 
  setNames(c("country_code","country","year","Oct","Apr"))





# Combine all and export: -----


weo_2020 <- c(list(forecast_2020), forecasts_updates) %>% 
  reduce(merge, by=c("country_code","year")) %>% 
  as_tibble() %>% 
  gather("horizon","value",Oct:ncol(.))


saveRDS(weo_2020,"../Forecasts_Time_Covid_material/intermediate_data/weo_2020.RDS")






