########## Script to compare forecasts from World Bank GEP and WEO


wb=read_xlsx("~/Desktop/gep_forecasts.xlsx", skip = 1) %>% 
  rename(country = 1) %>% 
  mutate_at(vars(contains("2020")), as.numeric) %>% 
  mutate(country_code = countrycode(country, "country.name","imf")) %>% 
  select(-country,-jan2021) %>% 
  gather("horizon","wb",jan2020:jun2020) %>% 
  mutate(horizon = str_to_sentence(str_remove(horizon,"2020")))


wb_list=unique(wb$country_code)

df <- read_rds("../Forecasts_Time_Covid_material/intermediate_data/weights_aggregates/weights_lidc.RDS") %>%
  rename(country_code = ifscode) %>% 
  merge(read_rds("../Forecasts_Time_Covid_material/intermediate_data/weo_2020.RDS"),by=c("country_code","horizon","year")) %>%
  rename(imf = value) %>% 
  merge(wb, by=c("country_code","horizon"), all.x = T) %>% 
  as_tibble() %>% 
  filter(country_code %in% wb_list) %>% 
  mutate(imf_weighted = imf*weight,
         wb_weighted = wb*weight) %>% 
  group_by(horizon) %>% 
  summarise_at(vars(contains("weighted")),funs(sum(. , na.rm = T))) %>%
  mutate(wb_weighted = case_when(wb_weighted == 0 ~ NA_real_,
                                T ~ wb_weighted)) %>% 
  gather("institution","value",imf_weighted:ncol(.)) %>% 
  mutate(institution = case_when(institution == "imf_weighted" ~ "WEO",
                                 institution == "wb_weighted" ~ "GEP")) %>% 
  mutate(horizon = factor(horizon, levels = c("Jan","Apr","Jun","Oct")))


df %>% 
  ggplot(aes(horizon, value, col = institution)) +
  geom_point(size = 3, alpha = 0.8) +
  ylab("Real GDP Growth Forecast (%)") +
  xlab("") +
  labs(col = "",
       linetype = "") +
  scale_color_manual(values = c("#4472C4","#ED7D31")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21))
