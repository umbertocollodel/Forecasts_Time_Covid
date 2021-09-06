#### Script to prepare inflation forecast from WEO


# Set paramters: -----

sheets <- getSheetNames("../Forecasts_Time_Covid_material/raw_data/WEO/inflation.xlsx") %>% 
  str_subset(.,"2020") 

var_name=c("Apr","Jan","Jun","Oct")


# Clean inflation sheet with 2020 estimates: -----

weo_inflation <- sheets %>% 
  map(~ read_xlsx("../Forecasts_Time_Covid_material/raw_data/WEO/inflation.xlsx", sheet = .x)) %>% 
  set_names(sheets) %>% 
  map(~ .x %>% slice(1: which(Country == "Zimbabwe"))) %>% 
  map(~ .x %>% select(Series_code, `2019`:`2020`)) %>% 
  map(~ .x %>% select(-matches("Q"))) %>% 
  map(~ .x %>% mutate(country_code = str_extract(Series_code,"\\d{3}"))) %>% 
  map(~ .x %>% mutate(growth_2020 = ((`2020` - `2019`)/`2019`)*100,
                      year = "2020")) %>% 
  map(~ .x %>% select(country_code,year,growth_2020)) %>% 
  map2(var_name, ~ .x %>% setNames(c("country_code", "year", .y))) %>% 
  map(~ .x %>% gather("horizon","value",3)) %>% 
  reduce(rbind) 

# Clean actuals sheet: ----- 


actuals <- read_xlsx("../Forecasts_Time_Covid_material/raw_data/WEO/inflation.xlsx", sheet = "apr2021") %>% 
  slice(1: which(Country == "Zimbabwe")) %>% 
  select(Series_code, `2019`:`2020`) %>% 
  select(-matches("Q")) %>% 
  mutate(country_code = str_extract(Series_code,"\\d{3}")) %>% 
  mutate(Actual = ((`2020` - `2019`)/`2019`)*100,
                      year = "2020") %>% 
  select(country_code,year,Actual)


# Merge and export:

merge(weo_inflation, actuals, by=c("country_code","year")) %>%
  mutate(country = countrycode(country_code,"imf","country.name")) %>% 
  filter(complete.cases(country)) %>% 
  as_tibble() %>% 
  saveRDS("../Forecasts_Time_Covid_material/intermediate_data/weo_2020_inflation.RDS")
