#' Cleaning WEO 2020 Forecasts
#' 
# From sheet with weo issue forecasts for every country and year, get all country forecasts for 2020
#' 
#' @param path path to excel file
#' @param sheet individual sheet with single weo issue
#' 
#' @return tibble with three identifiers (country_code, year and value)
#' 

clean_weo_2020_forecasts <- function(path, sheet){
  read_xlsx(path, sheet = sheet) %>%
  slice(1: which(Country == "Zimbabwe")) %>% 
  select(Series_code,`1950`:ncol(.)) %>%
  mutate(Series_code = str_extract(Series_code, "\\d{3}")) %>% 
  select(-matches("Q|M")) %>% 
  gather("year","value",`1950`:ncol(.)) %>% 
  filter(year == 2020) %>% 
  rename(country_code = Series_code)
}
