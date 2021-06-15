####### Script to show whether pessimistic forecaster errors in June generalized or some big outliers



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



# Figure - Scatter actual vs forecast (not weighted)


df %>% 
  ggplot(aes(value, Actual)) +
  geom_abline(slope = 1, col = "black", size = 1) +
  geom_point(size = 2,col = "dodgerblue4") +
  xlab("Forecast (%)") +
  ylab("Actual (%)") +
  xlim(-20,10) +
  ylim(-20,10) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21))


ggsave("../Forecasts_Time_Covid_material/output/figures/digging_june_pessimism/actuals_vs_forecasts_unweighted.pdf",
       width = 9.7,
       height = 6.4)



# Figure - Density plot of errors: balanced or asymmetric pessimism?: -----

# Build density manually: 

density <- df %>% 
  mutate(error = Actual - value) %>%
  .$error %>%
  .[complete.cases(.)] %>% 
  density()

# Plot and export:

data.frame(x = density$x, y = density$y) %>% 
  mutate(type = case_when(x >= 0 ~ "Pessimism",
                          T ~ "Optimism")) %>% 
  ggplot(aes(x,y)) +
  geom_line() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=type), alpha = 0.6) +
  theme_minimal() +
  xlim(-25,25) +
  xlab("Forecast Error (%)") +
  ylab("") +
  labs(fill="") +
  scale_fill_manual(values = c("#4472C4","#ED7D31")) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))

# Export:

ggsave("../Forecasts_Time_Covid_material/output/figures/digging_june_pessimism/distribution_errors.pdf",
       width = 9.7,
       height = 6.4)



# Figure - Errors for top 10 gloabl weight countries -----


# Plot:

df %>% 
  arrange(-weight) %>% 
  mutate(error = Actual - value,
         weight = weight*100) %>%
  mutate(type = case_when(error >= 0 ~ "Pessimism",
                          T ~ "Optimism")) %>% 
  select(country,weight, error, type) %>%
  mutate(country = paste0(country, " (", round(weight,1), " %)")) %>% 
  mutate(country = fct_reorder(country, weight, mean)) %>% 
  slice(1:10) %>% 
  ggplot(aes(country, error)) +
  geom_col(aes(fill = type), width= 0.5, alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  xlab("") +
  ylab("Forecast Error (%)")+
  labs(fill ="") +
  scale_fill_manual(values = c("#ED7D31","#4472C4")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))


# Export:

ggsave("../Forecasts_Time_Covid_material/output/figures/digging_june_pessimism/top_weight_errors.pdf",
       width = 9.7,
       height = 6.4)
  


# Figure - Weighted forecast vs. weighted actual ------

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

  

# Export:

ggsave("../Forecasts_Time_Covid_material/output/figures/digging_june_pessimism/actuals_vs_forecasts.pdf",
       width = 9.7,
       height = 6.4)


