# Load programs dataset: ----

program_countries=read_xlsx("~/Dropbox/Emergency_response_covid/Emergency_response_covid_material/raw_data/emergency_facilities.xlsx") %>% 
  .$Country %>%
  countrycode(.,"country.name","imf") %>%
  .[complete.cases(.)] %>% 
  unique()


# Do the regressions:


significance <- read_rds("../Forecasts_Time_Covid_material/intermediate_data/weo_2020.RDS") %>% 
  mutate(ef = case_when(country_code %in% program_countries ~ "EF",
                        T ~ "No EF")) %>%
  mutate(error = Actual - value) %>% 
  filter(horizon != "Jan") %>% 
  group_by(horizon, ef) %>% 
  mutate(error = DescTools::Winsorize(error, na.rm = T)) %>% 
  split(.$horizon) %>% 
  map(~ .x %>% split(.$ef)) %>% 
  modify_depth(2, ~ lm(error ~ 1, .x)) %>% 
  modify_depth(2, ~ summary(.x)) %>% 
  modify_depth(2, ~ .x$coefficients[,"t value"]) %>%
  modify_depth(2, ~ data.frame(`t-value` = .x)) %>%
  map(~ .x %>% bind_rows(.id = "ef")) %>% 
  bind_rows(.id = "horizon") %>% 
  mutate(signif = case_when(t.value >= 1.64 | t.value <= -1.64 ~ "***",
                             T ~ ""))
  
  


# Merge with forecasts data:

  read_rds("../Forecasts_Time_Covid_material/intermediate_data/weo_2020.RDS") %>% 
    mutate(ef = case_when(country_code %in% program_countries ~ "EF",
                          T ~ "No EF")) %>%
    mutate(error = Actual - value) %>% 
    filter(horizon != "Jan") %>% 
    mutate(horizon = factor(horizon, levels = c("Apr","Jun","Oct"))) %>% 
    group_by(horizon, ef) %>% 
    mutate(error = DescTools::Winsorize(error, na.rm = T)) %>% 
    summarise(mean_error = mean(error, na.rm = T)) %>% 
    merge(significance) %>% 
    ggplot(aes(y=mean_error, x = ef, fill = ef)) +
          geom_col(width = 0.5, alpha = 0.9) +
          geom_text(aes(y=mean_error*1.1,label = signif)) +
          xlab("") +
          ylab("Forecast Error (%)") +
          labs(fill = "") +
          facet_wrap(~ horizon, nrow = 1) +
          scale_fill_manual(values = c("#4472C4","#ED7D31")) +
          theme_minimal() +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 15)) +
          theme(panel.grid.major.x = element_blank()) +
          theme(strip.text = element_text(size = 15),
                axis.text.x = element_blank(),
                axis.text.y = element_text(size = 18),
                axis.title = element_text(size = 21))
  
# Export:
  
ggsave("../Forecasts_Time_Covid_material/output/figures/ef_forecasts/ef_forecasts_bias.pdf",
       height = 5.7,
       width = 11)
