####### Script to show whether pessimistic forecaster errors in June generalized or some big outliers: 


# Build dataframe: -----


df <- readRDS("../Forecasts_Time_Covid_material/intermediate_data/weo_2020.RDS") %>% 
  filter(horizon == "Jun") %>%
  merge(read_rds("../Forecasts_Time_Covid_material/intermediate_data/weights_aggregates/weights_global.RDS"), 
        by=c("country_code","horizon","year")) %>%
  merge(read_rds("../Forecasts_Time_Covid_material/intermediate_data/weights_aggregates/weights_global_actual.RDS"),
                 by=c("country_code")) %>% 
  mutate(weighted_forecast = value*weight,
         weighted_actual = Actual*actual_weight,
         weighted_error = weighted_actual - weighted_forecast) %>%
  mutate(most_pessimistic = case_when(rank(-weighted_error) %in% 1:5 ~ "yes",
                                      T ~ "no")) %>% 
  as_tibble()


# Plot and export: ------

df %>% 
  ggplot(aes(weighted_actual,weighted_forecast)) +
  geom_abline(slope=1, col = "black", size = 1) +
  geom_point(aes(col = most_pessimistic), size = 3, alpha = 0.5) +
  geom_point(data = subset(df, most_pessimistic == "yes"), aes(col=most_pessimistic), size = 3) +
  geom_text_repel(aes(label=ifelse(most_pessimistic == "yes",country,"")), col = "dodgerblue4", box.padding = 0.8) +
  scale_color_manual(values = c("darkgrey","dodgerblue4")) +
  ylim(-1.5,0.5) +
  xlim(-0.8,0.5) +
  theme_minimal() +
  xlab("Weighted Forecast (%)") +
  ylab("Weighted Actual (%)") +
  labs(col = "") +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))

  

ggsave("../Forecasts_Time_Covid_material/output/figures/june_errors.pdf",
       width = 9.7,
       height = 6.4)
