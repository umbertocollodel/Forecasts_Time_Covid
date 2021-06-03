########## Script to compare forecasts from World Bank GEP and WEO for Emerging Markets and LICs


# Prepare World Bank forecasts dataframe: -----

wb <- read_xlsx("../Forecasts_Time_Covid_material/raw_data/gep_forecasts_2020.xlsx", skip = 1) %>% 
  rename(country = 1) %>% 
  mutate_at(vars(contains("2020")), as.numeric) %>% 
  mutate(country_code = countrycode(country, "country.name","imf")) %>% 
  select(-country,-jan2021) %>% 
  gather("horizon","wb",jan2020:jun2020) %>% 
  mutate(horizon = str_to_sentence(str_remove(horizon,"2020")))


wb_list=unique(wb$country_code) %>% 
  .[complete.cases(.)]


# Combine WEO forecasts with weights for EMs and LICs: ----

# Set parameters:

path_weights=c("../Forecasts_Time_Covid_material/intermediate_data/weights_aggregates/weights_em.RDS",
               "../Forecasts_Time_Covid_material/intermediate_data/weights_aggregates/weights_lidc.RDS")

path_forecasts=rep("../Forecasts_Time_Covid_material/intermediate_data/weo_2020.RDS",2)

group_names=c("em","lidc")

# Combine:

weo_group_df <- path_weights %>% 
  map(~ read_rds(.x)) %>% 
  map(~ .x %>% rename(country_code = ifscode)) %>% 
  map2(path_forecasts, ~ .x %>% merge(read_rds(.y), by=c("country_code","horizon","year"))) %>% 
  map(~ .x %>% rename(imf = value)) 

names(weo_group_df) = group_names


# Combine with World Bank forecasts: -----
# Note: all.x in the merge to keep all weo horizons (also those for which no wb data), we then filter by the list of
# countries for which world bank forecasts to remove cases in which only imf values that would bias the aggregate calculation

preliminary_df <- weo_group_df %>% 
  map(~ .x %>% merge(wb, by=c("country_code","horizon"), all.x = T)) %>% 
  map(~ .x %>% as_tibble()) %>% 
  map(~ .x %>% filter(country_code %in% wb_list))



# Calculate aggregate evolution: ------


df <- preliminary_df %>% 
  map(~ .x %>% mutate(imf_weighted = imf*weight,
         wb_weighted = wb*weight)) %>% 
  map(~ .x %>% group_by(horizon)) %>% 
  map(~ .x %>% summarise_at(vars(contains("weighted")),funs(sum(. , na.rm = T)))) %>%
  map(~ .x %>% mutate(wb_weighted = case_when(wb_weighted == 0 ~ NA_real_,
                                T ~ wb_weighted))) %>% 
  map(~ .x %>% gather("institution","value",imf_weighted:ncol(.))) %>% 
  map(~ .x %>% mutate(institution = case_when(institution == "imf_weighted" ~ "WEO",
                                 institution == "wb_weighted" ~ "GEP"))) %>% 
  map(~ .x %>% mutate(horizon = factor(horizon, levels = c("Jan","Apr","Jun","Oct"))))


# Plot and export: -----

group_plots <- df %>% 
  map(~ .x %>% 
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
        axis.title = element_text(size = 21)))



# Export:

group_plots %>% 
  iwalk(~ ggsave(paste0("../Forecasts_Time_Covid_material/output/figures/aggregate_comparison/world_bank/",.y,".pdf"),
                 .x,
                 height = 5.7,
                 width = 11))


df %>% 
  iwalk(~ export(.x,
                 paste0("../Forecasts_Time_Covid_material/intermediate_data/replication_figures/aggregate_comparison/world_bank/",.y,".xlsx")))






